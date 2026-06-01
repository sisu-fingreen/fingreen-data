create_inputs_economy_consumption_income <- function(raw_data_path, n_households, global_params) {

  # dependencies: none
  
  # libraries ---------------------------------------------------------------

  library(dplyr)

  source("R/fingreen-r-utils.R", local = TRUE)

  # needed but not loaded to the namespace

  # stopifnot(is_installed("pxweb"))
  # stopifnot(is_installed("tidyr"))
  # stopifnot(is_installed("writexl"))

  # parameters -------------------------------------------------------------

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