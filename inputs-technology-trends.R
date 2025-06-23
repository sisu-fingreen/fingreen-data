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

# When the function asks if you want to stop downloading, input n for no in the r console
io_df <- get_eurostat(
  "naio_10_cp1750",
  time_format = "num",
  filters = list(
    geo = "FI",
    time = c(2010L:2022L),
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

io_inflation_corrected %>% 
  filter(
    industry_code_type_ava == "nace-rev-2" &
      fingreen_industry_code_use %in% c("P3_S14", "P3_S13", "P51G")
  ) %>% 
  tidyr::pivot_wider(names_from = "stk_flow", values_from = "values") %>%
  mutate(import_share = IMP / TOTAL) %>%
  group_by(geo, fingreen_industry_code_ava, fingreen_industry_code_use) %>% 
  arrange(time) %>% 
  mutate(
    rel_change_import_share = (import_share - lag(import_share)) /  lag(import_share)
  )

