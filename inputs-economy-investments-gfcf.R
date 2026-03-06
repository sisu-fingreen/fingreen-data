# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/investments/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/investments/")
create_dir_if_not_exists(results_dir, "results")

# parameters ---------------------------------------------------------------
global_params <- config::get(file = "global-params.yml")

base_year <- global_params$base_year
geo <- global_params$geo

# source data --------------------------------------------------------------

gfcf_investment_by_industry <- eurostat::get_eurostat(
  "nama_10_a64_p5",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    asset10 = "N11G", # Total fixed assets (gross)
    unit = "CP_MEUR", # current price millions of eur
    na_item = "P51G" # Gross fixed capital formation gfcf
  )
)

eurostat_to_fingreen_industry_nama_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
  sheet = "nama"
)

gfcf_investment_by_fingreen_industry <- gfcf_investment_by_industry |> 
  inner_join(
    filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
    by = c("nace_r2" = "eurostat_nace_r2")
  ) |> 
  group_by(fingreen_industry_code) |> 
  summarise(values = 1e6 * sum(values * coalesce(disaggregation_coefficient, 1)))

# TODO: convert to basic prices and make sure to match total GFCF from FD
