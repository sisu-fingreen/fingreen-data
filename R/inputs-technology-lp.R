pull_raw_data_inputs_technology_lp <- function(global_params, start_year = 1995L, clv_year = 2015) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-technology/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  # we will read the data in chain linked volume series,
  # so read the closest preceding one to the base year.
  # In eurostat they are available every 5 years
  # best_clv_year <- base_year - (base_year %% 5L)
  # if(!best_clv_year %in% c(2005L, 2010L, 2020L)) {
  #   stop("Base year is too far from 2005-2020, which is the range of eurostat chain linked volume series")
  # }
  
  clv_unit <- paste0("CLV", substr(clv_year, 3, 4), "_MEUR")

  gfcf_by_industry <- eurostat::get_eurostat(
    "nama_10_a64_p5",
    time_format = "num",
    filters = list(
      geo = geo,
      unit = clv_unit,
      asset10 = "N11G", # Total fixed assets (gross)
      na_item = "P51G" # gfcf
    )
  ) %>% 
    filter(time >= start_year)    

  capital_stock_by_industry <- eurostat::get_eurostat(
    "nama_10_nfa_st",
    time_format = "num",
    filters = list(
      geo = geo,
      unit = clv_unit,
      asset10 = "N11N" # Net capital stock
    )
  ) %>%
    filter(time >= start_year)

  gross_output_by_industry <- eurostat::get_eurostat(
    "nama_10_a64",
    time_format = "num",
    filters = list(
      geo = geo,
      unit = clv_unit,
      na_item = "P1" # Gross output
    )
  ) %>%
    filter(time >= start_year)

  hours_worked_by_industry <- eurostat::get_eurostat(
    "nama_10_a64_e",
    time_format = "num",
    filters = list(
      geo = geo,
      na_item = "EMP_DC", # Total employed, domestic concept
      unit = "THS_HW" # Thousands of hours worked
    )
  ) |> 
    filter(time >= start_year)

  datasets_to_write <- c(
    "gfcf_by_industry",
    "capital_stock_by_industry",
    "gross_output_by_industry",
    "hours_worked_by_industry"
  )
  
  output_path <- paste0(raw_data_dir, "na-time-series.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)

}

create_inputs_technology_lp <- function(raw_data_path, global_params) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  source("R/fingreen-r-utils.R")
  source("R/transform-categorization.R")

  # parameters -------------------------------------------------------------

  geo <- global_params$geo
  base_year <- global_params$base_year
  
  # In the raw data we pulled, this will be the base year of the eur values
  clv_year <- base_year - (base_year %% 5L)
  
  current_year <- Sys.Date() |> format.Date(format = "%Y") |> as.integer()
  if(base_year > current_year - 2L){
    stop("Cannot have newer base year than current year - 2")
  }

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-technology/")
  create_dir_if_not_exists(results_dir, "results")
  graphs_dir <- paste0(working_directory, "/graphs/inputs-technology/")
  create_dir_if_not_exists(graphs_dir, "graphs")

  # source data --------------------------------------------------------------

  gfcf_by_industry <- readODS::read_ods(
    path = raw_data_path, sheet = "gfcf_by_industry"
  )
  capital_stock_by_industry <- readODS::read_ods(
    path = raw_data_path, sheet = "capital_stock_by_industry"
  )
  gross_output_by_industry <- readODS::read_ods(
    path = raw_data_path, sheet = "gross_output_by_industry"
  )
  hours_worked_by_industry <- readODS::read_ods(
    path = raw_data_path, sheet = "hours_worked_by_industry"
  )
  
  eurostat_nama_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
    sheet = "nama"
  ) |> 
    filter(relationship != "extra") |> 
    # We're not modelling U
    filter(fingreen_industry_code != "U")

  # process ------------------------------------------------------------------

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

  gross_output_by_fingreen_industry <- gross_output_by_industry |> 
    transform_industry() |> 
    mutate(gross_output_eur = 1e6 * values, .keep = "unused")
    
  hours_worked_by_fingreen_industry <- hours_worked_by_industry |> 
    transform_industry() |> 
    mutate(hours_worked = 1000 * values, .keep = "unused")

  gfcf_by_fingreen_industry <- gfcf_by_industry |> 
    transform_industry() |> 
    rename(gfcf_meur = values)

  capital_stock_by_fingreen_industry <- capital_stock_by_industry |> 
    # Capital stock in T is missing, we impute 0 as the investment is also 0
    # This is natural for the households as employers industry
    mutate(values = if_else(nace_r2 == "T", 0, values)) %>% 
    transform_industry() |> 
    rename(net_capital_stock_meur = values)
  
  labour_productivity <- gross_output_by_fingreen_industry %>%
    left_join(hours_worked_by_fingreen_industry, by = c("year", "fingreen_industry_code")) %>% 
    group_by(fingreen_industry_code) %>% 
    arrange(year) %>% 
    mutate(
      labour_productivity_output_eur_per_hour = gross_output_eur / hours_worked,
      d_labour_productivity_output_eur_per_hour = labour_productivity_output_eur_per_hour -
        lag(labour_productivity_output_eur_per_hour),
      g_labour_productivity = d_labour_productivity_output_eur_per_hour /
        lag(labour_productivity_output_eur_per_hour)
    ) %>% 
    ungroup()

  library(ggplot2)
  p_labour_productivity <- labour_productivity %>% 
    ggplot(aes(year, labour_productivity_output_eur_per_hour)) +
    geom_line() +
    facet_wrap(~fingreen_industry_code)

  ggplot2::ggsave(
    filename = paste0(graphs_dir, "labour-productivity.jpeg"),
    plot = p_labour_productivity,
    width = 8.5,
    height = 6
  )

  p_labour_productivity_changes <- labour_productivity %>% 
    filter(year != min(year)) |> 
    ggplot(aes(year, d_labour_productivity_output_eur_per_hour)) +
    geom_line() +
    facet_wrap(~fingreen_industry_code)

  ggplot2::ggsave(
    filename = paste0(graphs_dir, "labour-productivity-changes.jpeg"),
    plot = p_labour_productivity_changes,
    width = 8.5,
    height = 6
  )

  p_labour_productivity_growth <- labour_productivity %>% 
    filter(year != min(year)) |> 
    ggplot(aes(year, g_labour_productivity)) +
    geom_line() +
    facet_wrap(~fingreen_industry_code)

  ggplot2::ggsave(
    filename = paste0(graphs_dir, "labour-productivity-growth.jpeg"),
    plot = p_labour_productivity_growth,
    width = 8.5,
    height = 6
  )
  
  # Relevant for productivity changes is the investment of the previous period
  gfcf_previous_year <- gfcf_by_fingreen_industry %>% 
    mutate(year = year + 1L)

  shares_of_new_capital <- capital_stock_by_fingreen_industry %>% 
    left_join(gfcf_previous_year, by = c("fingreen_industry_code", "year")) %>% 
    mutate(share_of_new_capital = gfcf_meur / net_capital_stock_meur) %>% 
    # Don't allow negative values or too tiny shares of new capital,
    # because they would have weird effects in the normalization done next
    filter(share_of_new_capital > 0.001)

  all_labour_productivity_changes <- labour_productivity %>% 
    left_join(shares_of_new_capital, by = c("fingreen_industry_code", "year")) %>% 
    mutate(
      psi_lambda = g_labour_productivity / share_of_new_capital,
      # transform to compress the long tails of the distributions
      arcsinh_psi_lambda = asinh(psi_lambda)
    ) %>% 
    filter(year >= 1996L & !is.na(psi_lambda))

  all_labour_productivity_changes %>% 
    ggplot(aes(year, psi_lambda)) +
    geom_point() +
    geom_line() +
    facet_wrap(~fingreen_industry_code, scales = "free_y")

  # Convert the values to base_year euros
  eur_variables <- c(
    "gross_output_eur",
    "labour_productivity_output_eur_per_hour",
    "d_labour_productivity_output_eur_per_hour",
    "net_capital_stock_meur",
    "gfcf_meur"
  )

  # It is known that cpi adjustment for inflation is less accurate than clv, but
  # since we picked a close clv year earlier, the adjustment will be small
  all_labour_productivity_changes_base_year_eur <- all_labour_productivity_changes %>% 
    mutate(across(all_of(eur_variables), .fns = ~ convert_eur_value_between_years(.x, clv_year, base_year, geo = global_params$geo)))

  # results -----------------------------------------------------------------

  # Extract the distribution statistics from the data and export to excel

  lp_norm <- all_labour_productivity_changes_base_year_eur %>% 
    group_by(fingreen_industry_code) %>% 
    summarise(
      mean = mean(arcsinh_psi_lambda),
      median = median(arcsinh_psi_lambda),
      sd = sd(arcsinh_psi_lambda)
    ) %>%
    data.table::transpose(keep.names = "measure", make.names = "fingreen_industry_code")

  lp_change_limits <- all_labour_productivity_changes_base_year_eur |> 
    summarise(
      q05 = quantile(arcsinh_psi_lambda, 0.05),
      q95 = quantile(arcsinh_psi_lambda, 0.95)
    ) |> 
    select(q05, q95)
  
  lp_for_calibration <- labour_productivity |> 
    filter(between(year, base_year, current_year - 2L)) |> 
    mutate(
      labour_productivity_output_eur_per_hour = convert_eur_value_between_years(
        x = labour_productivity_output_eur_per_hour,
        from = clv_year, # The raw data is chain linked volumes, year is defined earlier
        to = base_year,
        geo = geo
      )
    ) |> 
    arrange(fingreen_industry_code) |> 
    select(fingreen_industry_code, year, labour_productivity_output_eur_per_hour) |> 
    tidyr::pivot_wider(names_from = year, values_from = labour_productivity_output_eur_per_hour)
  
  lp_g_total_for_calibration <- labour_productivity |> 
    group_by(year) |> 
    summarise(
      labour_productivity_total = sum(gross_output_eur) / sum(hours_worked),
      .groups = "drop"
    ) |>
    arrange(year) |> 
    mutate(
      g_labour_productivity = (labour_productivity_total - lag(labour_productivity_total)) / lag(labour_productivity_total)
    ) |> 
    filter(between(year, base_year, current_year - 2L)) |> 
    select(year, g_labour_productivity) |> 
    tidyr::pivot_wider(names_from = year, values_from = g_labour_productivity)

  lp_g_for_calibration <- labour_productivity |> 
    filter(between(year, base_year, current_year - 2L)) |> 
    arrange(fingreen_industry_code) |> 
    select(fingreen_industry_code, year, g_labour_productivity) |> 
    tidyr::pivot_wider(names_from = year, values_from = g_labour_productivity)

  # export -------------------------------------------------------------------
  
  datasets_to_write <- c("lp_norm", "lp_change_limits", "lp_for_calibration", "lp_g_for_calibration", "lp_g_total_for_calibration")

  output_path <- paste0(results_dir, "lp.ods")

  readODS::write_ods(mget(datasets_to_write), path = output_path)

  return(output_path)

}
