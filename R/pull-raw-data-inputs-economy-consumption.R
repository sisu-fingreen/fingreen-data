pull_raw_data_inputs_economy_consumption <- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/consumption/")
  create_dir_if_not_exists(raw_data_dir, "raw data")


  expenditure_by_coicop <- eurostat::get_eurostat(
    "nama_10_co3_p3",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      unit = "CP_MEUR" # Current prices millions of euros
    )
  )
  expenditure_by_coicop_schema <- structure(
    list(
      column_name = c('freq', 'unit', 'coicop', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('freq', 'unit', 'coicop', 'geo', 'time', 'values')
  )
  validate_schema(expenditure_by_coicop, expenditure_by_coicop_schema, "expenditure_by_coicop")
  

  hh_fd_bp <- eurostat::get_eurostat(
    "naio_10_cp1750",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      ind_use = "P3_S14",
      stk_flow = "TOTAL",
      unit = "MIO_EUR"
    )
  )
  hh_fd_bp_schema <- structure(
    list(
      column_name = c('freq', 'unit', 'ind_ava', 'ind_use', 'stk_flow', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('freq', 'unit', 'ind_ava', 'ind_use', 'stk_flow', 'geo', 'time', 'values')
  )
  validate_schema(hh_fd_bp, hh_fd_bp_schema, "hh_fd_bp")


  share_of_disposable_income_per_quintile <- eurostat::get_eurostat(
    "icw_res_01",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      quant_inc = paste0("QU", 1L:5L),
      indic_ewb = "DI", # DI = disposable income
      quant_expn = "TOTAL",
      quant_wlth = "TOTAL",
      unit = "PC"
    )
  )
  share_of_disposable_income_per_quintile_schema <- structure(
    list(
      column_name = c('freq', 'quant_inc', 'quant_expn', 'quant_wlth', 'indic_ewb', 'unit', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('freq', 'quant_inc', 'quant_expn', 'quant_wlth', 'indic_ewb', 'unit', 'geo', 'time', 'values')
  )
  validate_schema(share_of_disposable_income_per_quintile, share_of_disposable_income_per_quintile_schema, "share_of_disposable_income_per_quintile")


  total_disposable_income <- eurostat::get_eurostat(
    "nasa_10_nf_tr",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      unit = "CP_MEUR", # current price, millions of euros
      direct = "PAID",
      na_item = "B6G", # disposable income
      sector = "S14" # S14 = households
    )
  )
  total_disposable_income_schema <- structure(
    list(
      column_name = c('freq', 'unit', 'direct', 'na_item', 'sector', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'integer')
    ),
    class = 'data.frame',
    row.names = c('freq', 'unit', 'direct', 'na_item', 'sector', 'geo', 'time', 'values')
  )
  validate_schema(total_disposable_income, total_disposable_income_schema, "total_disposable_income")

  expenditure_share_by_coicop_by_quintile <- eurostat::get_eurostat(
    "hbs_str_t223",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      quant_inc = paste0("QU", 1:5),
      unit = "PM"
    )
  )
  expenditure_share_by_coicop_by_quintile_schema <- structure(
    list(
      column_name = c('freq', 'quant_inc', 'coicop', 'unit', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'numeric', 'integer')
    ),
    class = 'data.frame',
    row.names = c('freq', 'quant_inc', 'coicop', 'unit', 'geo', 'time', 'values')
  )
  validate_schema(
    expenditure_share_by_coicop_by_quintile,
    expenditure_share_by_coicop_by_quintile_schema,
    "expenditure_share_by_coicop_by_quintile"
  )

  mean_expenditure_by_quintile <- eurostat::get_eurostat(
    "hbs_exp_t133",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      quant_inc = paste0("QU", 1:5),
      unit = "PPS_HH" # Purchasing power standard per household
    )
  )
  mean_expenditure_by_quintile_schema <- structure(
    list(
      column_name = c('freq', 'quant_inc', 'unit', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'numeric', 'integer')
    ),
    class = 'data.frame',
    row.names = c('freq', 'quant_inc', 'unit', 'geo', 'time', 'values')
  )
  validate_schema(mean_expenditure_by_quintile, mean_expenditure_by_quintile_schema, "mean_expenditure_by_quintile")


  datasets_to_write <- c(
    "expenditure_by_coicop",
    "hh_fd_bp",
    "share_of_disposable_income_per_quintile",
    "total_disposable_income",
    "expenditure_share_by_coicop_by_quintile",
    "mean_expenditure_by_quintile"
  )
  
  output_path <- paste0(raw_data_dir, "consumption.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}