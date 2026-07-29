pull_raw_data_inputs_technology_lp <- function(global_params, start_year = 1995L) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-technology/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  gfcf_by_industry <- eurostat::get_eurostat(
    "nama_10_a64_p5",
    time_format = "num",
    filters = list(
      geo = geo,
      unit = "CLV20_MEUR", # chain linked values, 2020 meur
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
      unit = "CLV20_MEUR", # Chain linked values, 2020 meur
      asset10 = "N11N" # Net capital stock
    )
  ) %>%
    filter(time >= start_year)

  gross_output_by_industry <- eurostat::get_eurostat(
    "nama_10_a64",
    time_format = "num",
    filters = list(
      geo = geo,
      unit = "CLV20_MEUR", # Chain linked values, 2020 meur
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
    filter(relationship != "extra")

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
      psi_lambda = g_labour_productivity / share_of_new_capital
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

  all_labour_productivity_changes_2010eur <- all_labour_productivity_changes %>% 
    mutate(across(all_of(eur_variables), .fns = ~ convert_eur_value_between_years(.x, 2020L, base_year)))

  # results -----------------------------------------------------------------

  # Extract the distribution statistics from the data and export to excel

  lp_norm <- all_labour_productivity_changes_2010eur %>% 
    group_by(fingreen_industry_code) %>% 
    summarise(
      mean = mean(psi_lambda),
      median = median(psi_lambda),
      sd = sd(psi_lambda)
    ) %>%
    data.table::transpose(keep.names = "measure", make.names = "fingreen_industry_code")

  # export -------------------------------------------------------------------
  
  datasets_to_write <- c("lp_norm")

  output_path <- paste0(results_dir, "lp.ods")

  readODS::write_ods(mget(datasets_to_write), path = output_path)

  return(output_path)

}
