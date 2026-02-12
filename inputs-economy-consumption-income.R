# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("pxweb"))
stopifnot(is_installed("tidyr"))
stopifnot(is_installed("writexl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/consumption")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/consumption")
create_dir_if_not_exists(results_dir, "results")

# parameters -------------------------------------------------------------

base_year = 2010L
geo = "FI" # not used atm

# source data --------------------------------------------------------------

# First get the data per decile and convert to quintile using the number of households in each decile
# You could just take the average, but as the household numbers vary marginally, this is marginally
# more precise (and very easy).

mean_disposable_household_income_per_decile <- pxweb::pxweb_get(
  url = "https://statfin.stat.fi/PxWeb/api/v1/en/StatFin/tjt/statfin_tjt_pxt_128c.px",
  query = list(
    Tiedot = c("kturaha", "asuntok"),
    Vuosi = as.character(base_year),
    Tulokymmenys = as.character(1:10)
  )
) |> as.data.frame() |> 
  fix_names()

# processing -------------------------------------------------------------

mean_disposable_household_income_per_quintile <- mean_disposable_household_income_per_decile |> 
  mutate(
    total_hh_income_per_decile = number_of_household_dwelling_units * x8_disposable_cash_income_mean,
    quintile = case_match(
      decile,
      c("I (Lowest-income 10 %)", "II") ~ "I",
      c("III", "IV") ~ "II",
      c("V", "VI") ~ "III",
      c("VII", "VIII") ~ "IV",
      c("IX", "X (Highest-income 10 %)") ~ "V"
    ) |> factor(levels = as.roman(1:5))
  ) |> 
  group_by(quintile) |> 
  summarise(mean_hh_disposable_income = sum(total_hh_income_per_decile) / sum(number_of_household_dwelling_units))

# output results ---------------------------------------------------------

res_mean_disposable_household_income_per_quintile <- mean_disposable_household_income_per_quintile |> 
  mutate(year = base_year) |> 
  tidyr::pivot_wider(names_from = quintile, values_from = mean_hh_disposable_income)

writexl::write_xlsx(
  res_mean_disposable_household_income_per_quintile,
  path = paste0(results_dir, "/mean-disposable-household-income-per-quintile.xlsx")
)
