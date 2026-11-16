pull_raw_data_inputs_economy_inputouput <- function(global_params, end_year = 2022L) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/inputoutput/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  # The data may be too big so divide to two queries
  get_io_annual <- function(from, to){
    res <- eurostat::get_eurostat(
      "naio_10_cp1750",
      time_format = "num",
      filters = list(
        geo = geo,
        time = from:to,
        unit = "MIO_EUR"
      )
    )
  }
  if(end_year - base_year > 8){
    io_annual_1 <- get_io_annual(base_year, base_year + 8)
    io_annual_2 <- get_io_annual(base_year + 9, end_year)
    io_annual <- bind_rows(io_annual_1, io_annual_2)
  } else{
    io_annual <- get_io_annual(base_year, end_year)
  }
  
  datasets_to_write <- c("io_annual")
  
  output_path <- paste0(raw_data_dir, "inputoutput.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}
