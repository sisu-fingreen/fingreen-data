# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")
source("R/get-number-of-households.R")

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
geo = "FI"

# source data --------------------------------------------------------------

# We need to get the data from Eurostat, because there the income quantiles have
# equal number of households, instead of equal number of people. Refer to
# https://ec.europa.eu/eurostat/documents/54431/1966394/Standardised-key-social-variables.pdf
# In Eurostat, we don't have direct data on the mean disposable income per quintile, but
# we can deduct it from the shares of disposable income, the total disposable income, and
# the number of households per quintile.

share_of_disposable_income_per_quintile <- eurostat::get_eurostat(
  "icw_res_01",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    quant_inc = paste0("QU", 1L:5L),
    indic_il = "INC_DISP", # INC_DISP = disposable income
    quant_expn = "TOTAL",
    quant_wlth = "TOTAL",
    unit = "PC"
  )
) |> 
  select(geo, time, quant_inc, share_of_disposable_income = values)

total_disposable_income <- eurostat::get_eurostat(
  "nasa_10_nf_tr",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    unit = "CP_MEUR", # current price, millions of euros
    direct = "PAID",
    na_item = "B6G", # disposable income
    sector = "S14" # S14 = households
  )
) |> 
  select(geo, time, total_disposable_income = values)

n_households <- get_number_of_households(geo, base_year)

# processing -------------------------------------------------------------

mean_disposable_income_per_quintile <- share_of_disposable_income_per_quintile |> 
  left_join(total_disposable_income, by = c("geo", "time")) |> 
  left_join(n_households, by = c("geo", "time" = "year")) |> 
  mutate(
    n_households_per_quintile = n_households / 5,
    mean_hh_disposable_income = share_of_disposable_income / 100 *
      total_disposable_income  * 1e6 / n_households_per_quintile
  )

# output results ---------------------------------------------------------

res_mean_disposable_household_income_per_quintile <- mean_disposable_income_per_quintile |> 
  select(year = time, quant_inc, mean_hh_disposable_income) |> 
  tidyr::pivot_wider(names_from = quant_inc, values_from = mean_hh_disposable_income)

writexl::write_xlsx(
  res_mean_disposable_household_income_per_quintile,
  path = paste0(results_dir, "/mean-disposable-household-income-per-quintile-", tolower(geo), "-", base_year, ".xlsx")
)
