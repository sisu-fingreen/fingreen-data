# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(pxweb)
library(eurostat)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("writexl"))

global_params <- config::get(file = "global-params.yml")

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/investment/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/investment/")
create_dir_if_not_exists(results_dir, "results")

# functions --------------------------------------------------------------

create_data_depreciation_rates <- function() {
  geo <- global_params$geo
  geo3 <- global_params$geo3

  base_year <- global_params$base_year |> as.integer()

  time_start <- 2000
  time_end <- 2022

  # check if there is a dir for EUKLEMS data, and create one if not

  euklems_dir <- paste0(working_directory, "/source-data/euklems")

  euklems_2018 <- readr::read_csv(
    sprintf("%s/18II_capital.csv", euklems_dir),
    show_col_types = FALSE
  )

  euklems_depreciation <- readxl::read_xlsx(
    path = sprintf("%s/18II_capital_meta.xlsx", euklems_dir),
    sheet = "Depreciation Rates"
  ) |>
    select(-description) |>
    tidyr::pivot_longer(
      cols = !all_of("isic4"),
      names_to = "asset",
      values_to = "depreciation_rate",
      names_transform = toupper
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

  euklems_2018_isic4_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/euklems-2018-isic4-to-fingreen-industry-map.xlsx",
    sheet = "map"
  )

  # processing -------------------------------------------------------------

  stopifnot(base_year %in% euklems_2018$year)
  capital_stock_base_year <- euklems_2018 |>
    filter(
      iso3 == geo3,
      var == "KQ",
      asset %in% asset_types,
      year == base_year
    ) |>
    # unify isi4c code form with the depreciation data
    mutate(
      isic4 = isic4 |>
        gsub("_", "-", x = _) |>
        gsub("[A-Z](\\d)", "\\1", x = _, perl = T)
    )

  # Figure of the assets by industry
  # capital_stock_base_year |>
  #   ggplot(aes(isic4, value, fill = asset)) +
  #   geom_col(position = "stack")

  # Due to the nature of the many-to-many mapping,
  # the totals will be off, but it does not matter
  # as in the end we are just calculating the
  # best estimate for the depreciation rate
  capital_stock_by_fingreen_industry <- capital_stock_base_year |>
    convert_data_from_euklems_to_fingreen_industry(
      mapping = euklems_2018_isic4_to_fingreen_industry_map,
      join_var = "isic4",
      id_vars = c("iso3", "asset", "year"),
      vars_to_transform = "value"
    )

  # For the rates, the aggregated cases must use mean instead of
  # the default sum
  euklems_depreciation_by_fingreen_industry <- euklems_depreciation |>
    convert_data_from_euklems_to_fingreen_industry(
      mapping = euklems_2018_isic4_to_fingreen_industry_map,
      join_var = "isic4",
      id_vars = c("asset"),
      vars_to_transform = "depreciation_rate",
      aggregation_function = mean
    )

  depreciation_rate_by_fingreen_industry <- capital_stock_by_fingreen_industry |>
    group_by(iso3, year, fingreen_industry_code) |>
    mutate(total_stock_by_industry = sum(value)) |>
    ungroup() |>
    mutate(share_of_industry_stock = value / total_stock_by_industry) |>
    left_join(
      euklems_depreciation_by_fingreen_industry,
      by = c("fingreen_industry_code", "asset")
    ) |>
    group_by(iso3, year, fingreen_industry_code) |>
    summarise(
      industry_depreciation_rate = sum(
        depreciation_rate * share_of_industry_stock
      ),
      .groups = "drop"
    )
  
  unique_fingreen_industry_codes_in_map <- get_unique_nonmissing_values(
    df = euklems_2018_isic4_to_fingreen_industry_map,
    var = "fingreen_industry_code"
  )
  unique_fingreen_industry_codes_in_result <- get_unique_nonmissing_values(
    df = depreciation_rate_by_fingreen_industry,
    var = "fingreen_industry_code"
  )
  stopifnot(identical(unique_fingreen_industry_codes_in_map, unique_fingreen_industry_codes_in_result))
  # results ----------------------------------------------------------------

  res <- depreciation_rate_by_fingreen_industry |>
    tidyr::pivot_wider(
      names_from = fingreen_industry_code,
      values_from = industry_depreciation_rate
    )
  
  stopifnot(identical(ncol(res), 41L))
  
  return(res)
}

# write results ----------------------------------------------------------

create_data_depreciation_rates() |> 
  writexl::write_xlsx(
    path = sprintf("%s/depreciation-rates.xlsx", results_dir)
  )
