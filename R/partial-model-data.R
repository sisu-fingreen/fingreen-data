create_partial_model_data_investment <- function(
  io_raw_data_path,
  lp_raw_data_path,
  depreciation_data_path,
  global_params
) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  source("R/fingreen-r-utils.R")
  source("R/transform-categorization.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/partial-models/")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------
  
  base_year <- global_params$base_year |> as.integer()
  current_year <- get_current_year()
  clv_year <- global_params$clv_year |> as.integer()

  io_annual <- readODS::read_ods(
    path = io_raw_data_path, sheet = "io_annual"
  )

  ind_ava_to_fingreen_industry_map <- readxl::read_xlsx(
    path = "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
    sheet = "ava"
  )
  
  eurostat_nama_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
    sheet = "nama"
  ) |> 
    filter(relationship != "extra") |> 
    # We're not modelling U
    filter(fingreen_industry_code != "U")
  
  capital_stock_by_industry <- readODS::read_ods(
    path = lp_raw_data_path, sheet = "capital_stock_by_industry"
  )
  
  gfcf_by_industry <- readODS::read_ods(
    path = lp_raw_data_path, sheet = "gfcf_by_industry"
  )

  depreciation_rate_by_fingreen_industry <- readODS::read_ods(
      path = depreciation_data_path
  )
  
  # process ------------------------------------------------------------------
  
  total_output_base_year_eur <- io_annual |> 
    filter(ind_use == "TU", stk_flow == "DOM") |> # TU = total use
    inner_join(
      filter(ind_ava_to_fingreen_industry_map, relationship != "extra"),
      by = c("ind_ava" = "eurostat_industry_code"),
      relationship = "many-to-many"
    ) |> 
    group_by(geo, time, fingreen_industry_code) |> 
    summarise(
      values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
      .groups = "drop"
    ) |> 
    mutate(
      value_base_year_eur = 1e6 * convert_eur_value_between_years(
        values,
        from = time,
        to = base_year,
        geo = global_params$geo
      )
    )

  transform_industry <- function(df) {
    res <- df |> 
    mutate(year = as.integer(time)) |> 
    rename(eurostat_nace_r2 = nace_r2) |> 
    transform_categorization(
      new_mapping = eurostat_nama_to_fingreen_industry_map,
      old_mapping_name = "eurostat_nace_r2",
      new_mapping_name = "fingreen_industry_code",
      value_vars = "values",
      grouping_vars = c("geo", "year")
    )
  }
  
  capital_stock_by_fingreen_industry <- capital_stock_by_industry |> 
    # Capital stock in T is missing, we impute 0 as the investment is also 0
    # This is natural for the households as employers industry
    mutate(values = if_else(nace_r2 == "T", 0, values)) %>% 
    transform_industry() |> 
    rename(net_capital_stock_meur = values)
    
  gfcf_by_fingreen_industry <- gfcf_by_industry |> 
    transform_industry() |> 
    rename(gfcf_meur = values)

  # export -------------------------------------------------------------------
  
  res_total_output <- total_output_base_year_eur |> 
    select(-values) |> 
    tidyr::pivot_wider(names_from = time, values_from = value_base_year_eur)
  
  res_capital_stock <- capital_stock_by_fingreen_industry |> 
    filter(year == base_year) |> 
    mutate(
      net_capital_stock = convert_eur_value_between_years(
        1e6 * net_capital_stock_meur,
        from = clv_year, # The data is in chain linked values, year is determined earlier
        to = base_year,
        geo = global_params$geo
      )
    ) |> 
    select(year, fingreen_industry_code, net_capital_stock)
  
  res_gfcf <- gfcf_by_fingreen_industry |> 
    filter(year == base_year) |> 
    mutate(
      gfcf = convert_eur_value_between_years(
        1e6 * gfcf_meur,
        from = clv_year, # The data is in chain linked values, year is determined earlier
        to = base_year,
        geo = global_params$geo
      )
    ) |> 
    select(year, fingreen_industry_code, gfcf)
  
  res_depreciation_rates <- depreciation_rate_by_fingreen_industry |> 
    tidyr::pivot_longer(
      cols = A1:ST,
      names_to = "fingreen_industry_code",
      values_to = "depreciation_rate"
    )

  res_gfcf_real_timeseries <- gfcf_by_fingreen_industry |> 
    filter(year <= current_year - 2L) |> 
    mutate(
      gfcf = convert_eur_value_between_years(
        1e6 * gfcf_meur,
        from = clv_year, # The data is in chain linked values, year is determined earlier
        to = base_year,
        geo = global_params$geo
      )
    ) |> 
    select(year, fingreen_industry_code, gfcf) |> 
    tidyr::pivot_wider(names_from = year, values_from = gfcf)

  datasets_to_write <- c(
    "res_total_output",
    "res_capital_stock",
    "res_gfcf",
    "res_depreciation_rates",
    "res_gfcf_real_timeseries"
  )

  output_path <- paste0(results_dir, "investment.ods")

  readODS::write_ods(mget(datasets_to_write), path = output_path)

  return(output_path)

}
