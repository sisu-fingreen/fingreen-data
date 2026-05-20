pull_raw_data_inputs_economy_consumption_income <- function() {

  library(dplyr)
  source("R/fingreen-r-utils.R", local = TRUE)

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/consumption")
  create_dir_if_not_exists(raw_data_dir, "raw data")

  global_params <- config::get(file = "global-params.yml")

  base_year <- global_params$base_year
  geo <- global_params$geo

  # We need to get the data from Eurostat (not statfin), because in Eurostat the income quantiles have
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
      indic_ewb = "DI", # DI = disposable income
      quant_expn = "TOTAL",
      quant_wlth = "TOTAL",
      unit = "PC"
    )
  )
  share_of_disposable_income_per_quintile_schema <- structure(
    list(
      column_name = c('freq', 'quant_inc', 'quant_expn', 'quant_wlth', 'indic_ewb', 'unit', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('freq', 'quant_inc', 'quant_expn', 'quant_wlth', 'indic_ewb', 'unit', 'geo', 'time', 'values')
  )
  validate_schema(share_of_disposable_income_per_quintile, share_of_disposable_income_per_quintile_schema, "share_of_disposable_income_per_quintile")

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
  )
  total_disposable_income_schema <- structure(
    list(
      column_name = c('freq', 'unit', 'direct', 'na_item', 'sector', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'integer')
    ),
    class = 'data.frame',
    row.names = c('freq', 'unit', 'direct', 'na_item', 'sector', 'geo', 'time', 'values')
  )
  validate_schema(total_disposable_income, total_disposable_income_schema, "total_disposable_income")

  datasets_to_write <- c("share_of_disposable_income_per_quintile", "total_disposable_income")

  output_path <- paste0(raw_data_dir, "income.ods")
  
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)
  
  return(output_path)
}

create_inputs_economy_consumption_income <- function(raw_data_path, n_households) {

  # dependencies: none
  
  # libraries ---------------------------------------------------------------

  library(dplyr)

  source("R/fingreen-r-utils.R", local = TRUE)

  # needed but not loaded to the namespace

  # stopifnot(is_installed("pxweb"))
  # stopifnot(is_installed("tidyr"))
  # stopifnot(is_installed("writexl"))

  # parameters -------------------------------------------------------------

  global_params <- config::get(file = "global-params.yml")

  base_year <- global_params$base_year
  geo <- global_params$geo

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/consumption")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------

  share_of_disposable_income_per_quintile <- readODS::read_ods(raw_data_path, sheet = "share_of_disposable_income_per_quintile") |> 
    select(geo, time, quant_inc, share_of_disposable_income = values)

  total_disposable_income <- readODS::read_ods(raw_data_path, sheet = "total_disposable_income") |> 
    select(geo, time, total_disposable_income = values)

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

  output_path <- paste0(results_dir, "/mean-disposable-household-income-per-quintile-", tolower(geo), "-", base_year, ".xlsx")
  writexl::write_xlsx(
    res_mean_disposable_household_income_per_quintile,
    path = output_path
  )
  
  return(output_path)

}