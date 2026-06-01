pull_raw_data_n_households <- function(global_params) {

  source("R/fingreen-r-utils.R")

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/shared/")
  create_dir_if_not_exists(raw_data_dir, "raw data")

  n_households <- eurostat::get_eurostat(
    "lfst_hhnhtych",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      agechild = "TOTAL",
      n_child = "TOTAL",
      phhcomp = "TOTAL"
    )
  )
  n_households_schema <- structure(
    list(
      column_name = c('freq', 'agechild', 'n_child', 'phhcomp', 'unit', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('freq', 'agechild', 'n_child', 'phhcomp', 'unit', 'geo', 'time', 'values')
  )
  validate_schema(n_households, n_households_schema, "n_households")
  
  output_path <- paste0(raw_data_dir, "n-households.ods")
  
  readODS::write_ods(n_households, path = output_path, sheet = "n_households")

  return(output_path)
}

get_number_of_households <- function(raw_data_path) {
   n_households <- readODS::read_ods(raw_data_path, sheet = "n_households") |> 
    dplyr::transmute(
      geo = geo,
      year = as.integer(time),
      n_households = as.integer(values * 1e3)
    )

  return(n_households)
}