# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(pxweb)
library(eurostat)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("writexl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/")
create_dir_if_not_exists(results_dir, "results")

# source data --------------------------------------------------------------

global_params <- config::get(file = "global-params.yml")

geo <- global_params$geo
geo3 <- global_params$geo3

base_year <- global_params$base_year |> as.integer()

time_start <- 2000
time_end <- 2022

# check if there is a dir for EUKLEMS data, and create one if not

euklems_dir <- paste0(working_directory, "/source-data/euklems")

create_dir_if_not_exists(euklems_dir, "EUKLEMS")

euklems_2018 <- readr::read_csv(
  sprintf("%s/18II_capital.csv", euklems_dir)),
  show_col_types = FALSE
)

euklems_depreciation <- readxl::read_xlsx(
  path = sprintf("%s/18II_capital_meta.xlsx", euklems_dir),
  sheet = "Depreciation Rates"
)

asset_types <- c(
  "IT",
  "CT",
  "SOFT_DB",
  "TRAEQ",
  "OMACH",
  "OCON",
  "RSTRUC",
  "CULT",
  "RD",
  "OIPP"
)

capital_stock_2010 <- euklems_2018 |> 
  filter(
    iso3 == geo3,
    var == "KQ",
    asset %in% asset_types,
    year == 2010L
  )



depreciation_rate_by_industry <- capital_stock |> 
  group_by(iso3, year, isic4) |> 
  mutate(total_stock_by_industry = sum(value)) |> 
  ungroup() |> 
  mutate(share_of_industry_stock = value / total_stock_by_industry) |> 
  left_join(euklems_depreciation_rates, by = c("isic4", "asset")) |> 
  group_by(iso3, year, isic4) |> 
  summarise(
    industry_depreciation_rate = sum(depreciation_rate * share_of_industry_stock),
    .groups = "drop"
  )



net_capital_stock <- get_eurostat(
  id = "nama_10_nfa_st",
  time_format = "num",
  filters = list(
    geo = geo,
    time = time_start:time_end |> as.character(),
    asset10 = "N11N", # total fixed assets net
    unit = "CLV20_MEUR",
    freq = "A"
  )
) |> 
  mutate(time = as.integer(time))

gfcf <- get_eurostat(
  id = "nama_10_a64_p5",
  time_format = "num",
  filters = list(
    geo = geo,
    time = time_start:time_end |> as.character(),
    asset10 = "N11G", # total fixed assets gross
    unit = "CLV20_MEUR",
    freq = "A",
    na_item = "P51G" # gfcf
  )
) |> 
  mutate(time = as.integer(time))


# transform industry stucture ---------------------------------------------------------

transform_industry_structure_nama <- function(df, industry_mapping){
  res <- df |> 
    inner_join(
      industry_mapping,
      by = c("nace_r2" = "eurostat_nace_r2"),
      relationship = "many-to-many"
    ) |> 
    group_by(geo, time, fingreen_industry_code) |> 
    summarise(
      values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
      .groups = "drop"
    )

  return(res)
}

eurostat_nama_industry_to_fingreen_industry_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
  sheet = "nama"
)

net_capital_stock_transform <- net_capital_stock |> 
  transform_industry_structure_nama(
    industry_mapping = filter(eurostat_nama_industry_to_fingreen_industry_map, relationship != "extra")
  ) |> 
  rename(net_capital_stock = values)

gfcf_transform <- gfcf |> 
  transform_industry_structure_nama(
    industry_mapping = filter(eurostat_nama_industry_to_fingreen_industry_map, relationship != "extra")
  ) |> 
  rename(gfcf = values)


# calculate cfc rate -----------------------------------------------------

# Calculate the rate of consumption of fixed capital
# This gives crazy results so another way must be found
cfc_rate <- net_capital_stock_transform |> 
  left_join(gfcf_transform, by = c("geo", "time", "fingreen_industry_code")) |> 
  group_by(geo, fingreen_industry_code) |>
  mutate(
    cfc = lag(net_capital_stock) + gfcf - net_capital_stock,
    cfc_rate = cfc / net_capital_stock
  ) |> 
  ungroup() |> 
  filter(time != min(time)) |> 
  group_by(geo, fingreen_industry_code) |> 
  summarise(avg_cfc_rate = mean(cfc_rate), .groups = "drop")



