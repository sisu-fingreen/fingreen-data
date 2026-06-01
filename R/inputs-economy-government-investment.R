pull_raw_data_inputs_economy_government_investment <- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  if(geo != "FI") {
    # GFCF by industry and sector does not exist in Eurostat
    # nama_10r_2gfcf should have it but the data on government level is not there
    stop("Government investment data for other countries than FI not yet implemented.")
  }

  gfcf_gov_and_total <- pxweb::pxweb_get_data(
    url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/ntp/statfin_ntp_pxt_15af.px",
    query = list(
      Taloustoimi = "P51K", # gfcf gross fixed capital formation
      Sektori = c("S1", "S13"), # S1 total economy, S13 general government
      Toimiala = c("P", "Q"), # P education Q health
      Vara = "N0", # Non-financial assets total
      Vuosi = as.character(base_year:(base_year + 5L)),
      Tiedot = "cp"
    )
  )
  gfcf_gov_and_total_schema <- structure(
    list(
      column_name = c('Transaction', 'Sector', 'Industry', 'Instrument', 'Year', 'Current prices, millions of euro'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('Transaction', 'Sector', 'Industry', 'Instrument', 'Year', 'Current prices, millions of euro')
  )
  validate_schema(gfcf_gov_and_total, gfcf_gov_and_total_schema, "gfcf_gov_and_total")
  
  datasets_to_write <- c("gfcf_gov_and_total")
  
  output_path <- paste0(raw_data_dir, ".ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}

create_inputs_economy_government_investment <- function(raw_data_path, global_params) {
  
  # libraries ---------------------------------------------------------------

  library(dplyr)

  source("R/fingreen-r-utils.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/government/")
  create_dir_if_not_exists(results_dir, "results")

  # parameters ---------------------------------------------------------------

  base_year <- global_params$base_year
  geo <- global_params$geo

  # source data --------------------------------------------------------------
  gfcf_gov_and_total <- readODS::read_ods(raw_data_path, sheet = "gfcf_gov_and_total") |> 
    fix_names()

  # process ----------------------------------------------------------------

  gov_share_of_edu_and_health_gfcf <- gfcf_gov_and_total |> 
    group_by(year) |> 
    summarise(
      gov_gfcf = sum(if_else(sector == "S13 General government", current_prices_millions_of_euro, 0)),
      total_gfcf = sum(if_else(sector == "S1 Total economy", current_prices_millions_of_euro, 0)),
      gov_share = gov_gfcf / total_gfcf
    )

  mean_gov_share_of_edu_and_health_gfcf <- gov_share_of_edu_and_health_gfcf |> 
    summarise(mean_gov_share_of_edu_and_health_gfcf = mean(gov_share))


  # write results ----------------------------------------------------------

  output_path <- paste0(results_dir, "mean-gov-share-of-edu-and-health-gfcf.ods")

  readODS::write_ods(
    mean_gov_share_of_edu_and_health_gfcf,
    path = output_path
  )

}