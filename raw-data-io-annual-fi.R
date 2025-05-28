# libraries ---------------------------------------------------------------

library(dplyr)
library(eurostat)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace
stopifnot(is_installed("readxl"))
stopifnot(is_installed("writexl"))
stopifnot(is_installed("tidyr"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

results_dir <- paste0(working_directory, "/results/raw-data/")

create_dir_if_not_exists(results_dir, "results")

# source data -------------------------------------------------------------

io_df_info <- search_eurostat("naio_10_cp1750", column = "code")

# When the function asks if you want to stop downloading, input n for no in the r console
io_df <- get_eurostat(
  io_df_info$code[1],
  time_format = "num",
  filters = list(
    geo = "FI",
    time = c(2010L:2022L),
    unit = "MIO_EUR"
  )
)

eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "ava")
eurostat_to_fingreen_industry_use_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "use")

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

# A validation data frame
# check_df <- io_transform_use %>%
#   filter(industry_code_type_use == "nace-rev-2") %>% 
#   group_by(geo, time, fingreen_industry_code_ava, stk_flow) %>%
#   summarise(values = sum(values, na.rm = T), .groups = "drop") %>%
#   inner_join(eurostat_to_fingreen_industry_map, by = c("fingreen_industry_code_ava" = "fingreen_industry_code")) %>%
#   left_join(
#     filter(io_df, ind_use == "TOTAL"),
#     by = c("geo", "time", "stk_flow", "eurostat_industry_code" = "ind_ava")
#   ) %>%
#   mutate(difference = values.x - values.y)

# inflation correction ----------------------------------------------------

io_inflation_corrected <- io_transform_use %>% 
  mutate(values = convert_eur_value_between_years(values, from = time, to = 2010L))

# add total growth --------------------------------------------------------

io_growth <- filter(io_inflation_corrected, stk_flow == "TOTAL") %>% 
  group_by(geo, industry_code_type_ava, fingreen_industry_code_ava, industry_code_type_use, fingreen_industry_code_use) %>% 
  arrange(time) %>% 
  mutate(relative_growth = (values - lag(values)) / lag(values)) %>%
  ungroup() %>% 
  arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, desc(industry_code_type_use), fingreen_industry_code_use) %>% 
  select(-industry_code_type_use, -values) %>%
  tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = relative_growth) %>% 
  filter(time > min(time))


# metadata ----------------------------------------------------------------

metadata <- tibble(
  variable = c("geo", "time", "industry_code_type_ava", "fingreen_industry_code_ava", "A1:TU"),
  source = c(
    "Eurostat naio_10_cp1750",
    "Eurostat naio_10_cp1750",
    "Added in fingreen-data",
    "Modified from Eurostat naio_10_cp1750",
    "Modified from Eurostat naio_10_cp1750"
  ),
  explanation = c(
    "Country code",
    "Year",
    "The originating categorization of the 'industry' codes",
    "Data aggregated to the industry level used in Fingreen. Includes also esa-2010 accounting codes in addition to industries",
    "Basic prices, converted to 2010 euros. Data aggregated to the industry level used in Fingreen. Includes also esa-2010 accounting codes in addition to industries"
  )
)

sheet_info <- tibble(
  sheet = c("total", "domestic", "imports", "total_growth"),
  description = c(
    "Total io data, domestic + imports (STK_FLOW = 'TOTAL')",
    "Domestic io data (STK_FLOW = 'DOM')",
    "Imports io data (STK_FLOW = 'IMP')",
    "Relative total growth (x_[t] - x_[t-1] / x_[t-1]) from previous year, inflation adjusted"
  )
)

# results -----------------------------------------------------------------

prepare_io_results <- function(df, stock_or_flow) {
  res <- df %>%
    filter(stk_flow == !! stock_or_flow) %>% 
    arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, desc(industry_code_type_use), fingreen_industry_code_use) %>% 
    select(-industry_code_type_use) %>% 
    mutate(values = 1e6 * values) %>% 
    tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = values)
}

res_list <- lapply(c("TOTAL", "DOM", "IMP"), FUN = prepare_io_results, df = io_inflation_corrected)

names(res_list) <- c("total", "domestic", "imports")

res_list[["total_growth"]] <- io_growth
res_list[["metadata"]] <- metadata
res_list[["sheet_info"]] <- sheet_info

writexl::write_xlsx(res_list, path = "results/raw-data/io-annual-fi-2010-2022.xlsx")
