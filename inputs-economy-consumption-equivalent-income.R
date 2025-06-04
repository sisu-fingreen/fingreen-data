# libraries ---------------------------------------------------------------

library(dplyr)
library(pxweb)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("writexl"))
stopifnot(is_installed("tidyr"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/consumption")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/consumption")
create_dir_if_not_exists(results_dir, "results")


# source data --------------------------------------------------------------

data_year <- 2010L

mean_equivalent_income_per_income_group_query <- list(
  "Tulokymmenys tai fraktiiliryhmä" = c(
    "SS", "110", "1120", "2130", "3140", "4150", "5160", "6170", "7180", "8190", "91100", "100"
  ), # Codes for the income fractiles
  Tiedot = "ekvikturaha_mean", # Disposable cash income per consumption unit, mean
  Vuosi = as.character(data_year)
) %>% pxweb_query()

mean_equivalent_income_per_income_group <- pxweb_get(
  url = "https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin/tjt/statfin_tjt_pxt_11wh.px",
  query = mean_equivalent_income_per_income_group_query
) %>% 
  as.data.frame() %>% 
  fix_names() %>% 
  rename(income_group = income_decile_or_fractile_group)

household_stats_per_income_group_query <- list(
  Tiedot = c("asuntok", "modoecd_mean"), # n and mean size of households
  Vuosi = as.character(data_year),
  Tulokymmenys = c("SS", as.character(1:10))
) %>% pxweb_query()

household_stats_per_income_group <- pxweb_get(
  url = "https://pxdata.stat.fi:443/PxWeb/api/v1/en/StatFin/tjt/statfin_tjt_pxt_128c.px",
  query = household_stats_per_income_group_query
) %>%
  as.data.frame() %>% 
  fix_names() %>% 
  mutate(
    income_group = case_match(
      decile,
      "I (Lowest-income 10 %)" ~ "I",
      "X (Highest-income 10 %)" ~ "X",
      .default = decile
    )
  ) %>% 
  select(-decile)

# data processing ---------------------------------------------------------

decile_to_quintile <- function(decile){
  res <- factor(
    decile,
    levels = as.roman(1:10),
    labels = as.roman(1:5) %>% rep(each = 2)
  ) %>% as.character()
  return(res)
}

mean_equivalent_income_per_quintile <- mean_equivalent_income_per_income_group %>% 
  filter(!income_group %in% c("Top 1 %", "Total")) %>% 
  left_join(household_stats_per_income_group, by = c("year", "income_group")) %>%
  mutate(
    n_consumption_units = average_number_of_consumption_units * number_of_household_dwelling_units,
    income_per_group = disposable_cash_income_per_consumption_unit_mean * n_consumption_units,
    quintile = decile_to_quintile(income_group)
  ) %>%
  group_by(year, quintile) %>% 
  summarise(
    mean_income_per_consumption_unit = sum(income_per_group) / sum(n_consumption_units),
    .groups = "drop"
  )

# write results -----------------------------------------------------------

res_equivalent_income_by_decile <- mean_equivalent_income_per_income_group %>%
  tidyr::pivot_wider(names_from = income_group, values_from = disposable_cash_income_per_consumption_unit_mean) %>% 
  rename(All = Total) %>% 
  relocate(All, .after = last_col())

res_equivalent_income_by_quintile <- mean_equivalent_income_per_quintile %>% 
  tidyr::pivot_wider(names_from = quintile, values_from = mean_income_per_consumption_unit)

res_list <- list(
  "equivalent_income_by_decile" = res_equivalent_income_by_decile,
  "equivalent_income_by_quintile" = res_equivalent_income_by_quintile
)

writexl::write_xlsx(res_list, path = sprintf("%s/equivalent-income.xlsx", results_dir))
