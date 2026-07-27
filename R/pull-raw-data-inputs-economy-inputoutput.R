pull_raw_data_inputs_economy_inputouput <- function(global_params, end_year = 2022L) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/inputoutput/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  io_annual <- eurostat::get_eurostat_json(
    "naio_10_cp1750",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year:end_year,
      unit = "MIO_EUR"
    ),
    ask_special_io = FALSE
  )
  # assignInNamespace("readline", value = orig_readline, ns = "base")
  
  datasets_to_write <- c("io_annual")
  
  output_path <- paste0(raw_data_dir, "inputoutput.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}
