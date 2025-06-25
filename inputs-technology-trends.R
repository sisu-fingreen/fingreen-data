# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(eurostat)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("tidyr"))
stopifnot(is_installed("readxl"))
stopifnot(is_installed("writexl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-technology/technology-trends")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-technology/technology-trends")
create_dir_if_not_exists(results_dir, "results")

# source data --------------------------------------------------------------

catn("Getting the data.")

data_years <- 2010L:2021L

# When the function asks if you want to stop downloading, input n for no in the r console
io_df <- get_eurostat(
  "naio_10_cp1750",
  time_format = "num",
  filters = list(
    geo = "FI",
    time = data_years,
    unit = "MIO_EUR"
  )
)

eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "ava")
eurostat_to_fingreen_industry_use_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "use")

esa_2010_vocabulary <- read.csv(
  "https://dd.eionet.europa.eu/vocabulary/eurostat/na_item/csv"
) %>% 
  select(esa_2010_description = Label, esa_2010_code = Notation)

# transform industry structure --------------------------------------------

catn("Processing data.")

# Transform the ind_ava
io_transform_ava <- io_df %>%
  inner_join(
    filter(eurostat_to_fingreen_industry_ava_map),
    by = c("ind_ava" = "eurostat_industry_code"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    fingreen_industry_code_ava = coalesce(fingreen_industry_code, ind_ava),
    industry_code_type_ava = industry_code_type
  ) %>% 
  group_by(geo, time, industry_code_type_ava, fingreen_industry_code_ava, ind_use, stk_flow) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

# Transform the ind_use
io_transform_use <- io_transform_ava %>% 
  inner_join(
    filter(eurostat_to_fingreen_industry_use_map),
    by = c("ind_use" = "eurostat_industry_code"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    fingreen_industry_code_use = coalesce(fingreen_industry_code, ind_use),
    industry_code_type_use = industry_code_type
  ) %>% 
  group_by(geo, time, industry_code_type_ava, fingreen_industry_code_ava, industry_code_type_use, fingreen_industry_code_use, stk_flow) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

# inflation correction ----------------------------------------------------

io_inflation_corrected <- io_transform_use %>% 
  mutate(values = convert_eur_value_between_years(values, from = time, to = 2010L))

# FD import shares ---------------------------------------------------------

# Import shares for final demand of households (P3_S14), final demand of government (P3_S13)
# and GFCF (P51G)

calculate_import_share_trend <- function(io_data, grouping_vars){
  # note that the "trend" is the median of the relative y2y changes
  res <- io_data %>% 
    tidyr::pivot_wider(names_from = "stk_flow", values_from = "values") %>%
    mutate(import_share = coalesce(IMP, 0) / TOTAL) %>%
    group_by(pick(all_of(grouping_vars))) %>% 
    arrange(time) %>% 
    mutate(
      rel_change_import_share = if_else(
        import_share == 0 & lag(import_share) == 0,
        0,
        (import_share - lag(import_share)) / lag(import_share)
      )
    ) %>%
    filter(time != min(time)) %>% 
    summarise(
      median_rel_change_import_share = coalesce(
        median(rel_change_import_share, na.rm = T),
        0
      ),
      .groups = "drop"
    )
  return(res)
}

fd_import_share_changes <- io_inflation_corrected %>% 
  filter(
    industry_code_type_ava == "nace-rev-2" &
      fingreen_industry_code_use %in% c("P3_S14", "P3_S13", "P51G")
  ) %>% 
  calculate_import_share_trend(
    grouping_vars = c("geo", "fingreen_industry_code_ava", "fingreen_industry_code_use")
  )

res_fd_import_shares <- fd_import_share_changes %>% 
  mutate(
    category = factor(
      fingreen_industry_code_use,
      levels = c("P3_S14", "P3_S13", "P51G"),
      labels = c("CONS_h", "CONS_g", "GFCF")
    )
  ) %>%
  select(-fingreen_industry_code_use, -geo) %>% 
  arrange(category, fingreen_industry_code_ava) %>% 
  tidyr::pivot_wider(names_from = "fingreen_industry_code_ava", values_from = "median_rel_change_import_share")


# Z cell import shares ----------------------------------------------------

# Import shares for intermediate input use by industry (io matrix)

io_import_share_changes <- io_inflation_corrected %>% 
  filter(
    industry_code_type_ava == "nace-rev-2" &
      industry_code_type_use == "nace-rev-2"
  ) %>% 
  calculate_import_share_trend(
    grouping_vars = c("geo", "fingreen_industry_code_ava", "fingreen_industry_code_use")
  )

res_z_cell_import_shares <- io_import_share_changes %>%
  arrange(geo, fingreen_industry_code_ava, fingreen_industry_code_use) %>%
  select(-geo) %>% 
  tidyr::pivot_wider(names_from = "fingreen_industry_code_use", values_from = "median_rel_change_import_share")

# trend gvex --------------------------------------------------------------

# Trends in exports (P6) and government final demand (P3_S13)

export_and_government_fd_changes <- io_inflation_corrected %>% 
  filter(
    industry_code_type_ava == "nace-rev-2" &
      fingreen_industry_code_use %in% c("P6", "P3_S13") &
      stk_flow == "TOTAL"
  ) %>% 
  group_by(geo, fingreen_industry_code_ava, fingreen_industry_code_use) %>% 
  arrange(time) %>% 
  mutate(
    rel_change = if_else(
      values == 0 & lag(values) == 0,
      0,
      (values - lag(values)) / lag(values)
    )
  ) %>%
  filter(time != min(time)) %>%
  summarise(
    median_rel_change = coalesce(median(rel_change, na.rm = T), 0),
    .groups = "drop"
  )

res_trend_gvex <- export_and_government_fd_changes %>% 
  mutate(
    category = factor(fingreen_industry_code_use, levels = c("P6", "P3_S13"), labels = c("exp", "gov"))
  ) %>% 
  select(-fingreen_industry_code_use, -geo) %>% 
  arrange(category, fingreen_industry_code_ava) %>% 
  tidyr::pivot_wider(names_from = "fingreen_industry_code_ava", values_from = "median_rel_change")

# validations -------------------------------------------------------------

catn("Data processing done. Validating...")

# Check for right number of cols and rows
stopifnot(ncol(res_fd_import_shares) == 40)
stopifnot(ncol(res_z_cell_import_shares) == 40)
stopifnot(ncol(res_fd_import_shares) == 40)

stopifnot(nrow(res_fd_import_shares) == 3)
stopifnot(nrow(res_z_cell_import_shares) == 39)
stopifnot(nrow(res_trend_gvex) == 2)


# Check the means of the resulting data. Reference values are ad hoc, if tests do not pass
# the data might still be fine but it's good to check.

mean_import_share_trends <- res_fd_import_shares %>% 
  rowwise(category) %>% 
  summarise(mean_import_share_trend = mean(c_across(A1:ST)), .groups = "drop") %>% 
  pull(mean_import_share_trend, name = category)

stopifnot(between(mean_import_share_trends["CONS_h"], 0, 0.04))
stopifnot(between(mean_import_share_trends["CONS_g"], -0.01, 0.01))
stopifnot(between(mean_import_share_trends["GFCF"], -0.02, 0.02))

catn("Validation tests passed. Writing results...")

# write results -----------------------------------------------------------

res_list <- list(res_fd_import_shares, res_z_cell_import_shares, res_trend_gvex)
names(res_list) <- c("FD import shares", "Z cell import shares", "trend gvex")

writexl::write_xlsx(res_list, path = paste0(results_dir, "/technology-trends.xlsx"))

catn("...finished.")
