pull_raw_data_inputs_economy_labour_hours <- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/labour/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  hours_worked_by_industry <- eurostat::get_eurostat(
    "nama_10_a64_e",
    time_format = "num",
    filters = list(
      geo = geo,
      na_item = c(
        "EMP_DC", # Total employment domestic concept // all employed
        "SAL_DC", # Employees domestic concept
        "SELF_DC" # Self-employed domestic concept
      ),
      unit = c("THS_HW"),
      time = base_year
    )
  )
  
  datasets_to_write <- c("hours_worked_by_industry")
  
  output_path <- paste0(raw_data_dir, "hours.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}

create_inputs_economy_labour_hours <- function(raw_data_path, global_params) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  source("R/fingreen-r-utils.R")

  # needed but not loaded to the namespace

  # stopifnot(is_installed("writexl"))

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------

  hours_worked_by_industry <- readODS::read_ods(
    path = raw_data_path, sheet = "hours_worked_by_industry"
  )

  eurostat_to_fingreen_industry_nama_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
    sheet = "nama"
  )

  # transform to fingreen industry structure

  hours_worked_by_fingreen_industry <- hours_worked_by_industry |> 
    inner_join(
      filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
      by = c("nace_r2" = "eurostat_nace_r2"),
      relationship = "many-to-many"
    ) |> 
    group_by(geo, time, fingreen_industry_code, unit, na_item) |>
    summarise(
      values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
      .groups = "drop"
    )

  results <- hours_worked_by_fingreen_industry |>
    tidyr::pivot_wider(names_from = "unit", values_from = "values") |>
    mutate(
      hours_worked = THS_HW * 1000
    ) %>%
    select(-THS_HW) %>% 
    tidyr::pivot_longer(
      cols = c("hours_worked"),
      names_to = "variable",
      values_to = "values"
    ) %>%
    tidyr::pivot_wider(names_from = "fingreen_industry_code", values_from  = "values") |> 
    mutate(
      variable = factor(
        variable,
        levels = c("hours_worked"),
        labels = c("hours/year per industry")
      ),
      na_item = factor(
        na_item,
        levels = c("EMP_DC", "SAL_DC", "SELF_DC"),
        labels = c("total", "employees", "self-employed")
      )
    ) |> 
    arrange(geo, time, variable)

  # results ----------------------------------------------------------------
  
  output_path <- sprintf("%shours-worked.ods", results_dir)

  readODS::write_ods(results, path = output_path)

  return(output_path)

}