# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/government/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/government/")
create_dir_if_not_exists(results_dir, "results")

# parameters ---------------------------------------------------------------
global_params <- config::get(file = "global-params.yml")

base_year <- global_params$base_year
geo <- global_params$geo

# source data --------------------------------------------------------------

if(geo != "FI") {
  # GFCF by industry and sector does not exist in Eurostat
  # nama_10r_2gfcf should have it but the data on government level is not there
  stop("Other countries than FI not yet implemented.")
}

gfcf_gov_and_total <- pxweb::pxweb_get(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/vtp/statfin_vtp_pxt_124l.px",
  query = list(
    Taloustoimi = "P51K", # gfcf gross fixed capital formation
    Sektori = c("S1", "S13"), # S1 total economy, S13 general government
    Toimiala = c("P", "Q"), # P education Q health
    Vara = "N0", # Non-financial assets total
    Vuosi = as.character(base_year:(base_year + 5L)),
    Tiedot = "cp"
  )
) |> 
  as.data.frame() |> 
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

writexl::write_xlsx(
  mean_gov_share_of_edu_and_health_gfcf,
  path = sprintf("%smean-gov-share-of-edu-and-health-gfcf-%s-%s-%s.xlsx", results_dir, tolower(geo), base_year, base_year + 5)
)
