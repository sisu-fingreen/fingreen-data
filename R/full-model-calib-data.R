pull_raw_data_full_model_calib <- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo
  
  current_year <- get_current_year()

  if(base_year > current_year - 2L) {
    stop("Base year is too close to current year")
  }

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/full-model-calib/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  
  gva_and_output <- eurostat::get_eurostat(
    "nama_10_a64",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year:(current_year - 2L),
      na_item = c("B1G", "P1"), # B1G = Value added (gross), P1 = output
      unit = c("CP_MEUR", "PYP_MEUR")
    )
  )

  compensation_of_employees <- eurostat::get_eurostat(
    "nama_10_a64",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year:(current_year - 2L),
      na_item = "D1", # Compensation of employees
      unit = "CP_MEUR"
    )
  )
  
  lfpr <- eurostat::get_eurostat(
    "lfsa_argaedn",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year:(current_year - 2L),
      isced11 = c("ED0-2", "ED3_4", "ED5-8"),
      sex = c("F", "M"),
      citizen = "TOTAL",
      age = "Y15-64"
    )
  )
  
  datasets_to_write <- c("gva_and_output", "compensation_of_employees", "lfpr")
  
  output_path <- paste0(raw_data_dir, "full-model-calib.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}

create_full_model_calib_data <- function(
  raw_data_path,
  labour_productivity_path,
  global_params
) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  source("R/fingreen-r-utils.R")
  source("R/transform-categorization.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/full-model-calib/")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------
  
  # lambda (labour productivity) we already have calculated

  res_lambda_i <- readODS::read_ods(
    path = labour_productivity_path,
    sheet = "lp_for_calibration"
  )

  gva_and_output <- readODS::read_ods(
    path = raw_data_path, sheet = "gva_and_output"
  )
  compensation_of_employees <- readODS::read_ods(
    path = raw_data_path, sheet = "compensation_of_employees"
  )
  lfpr <- readODS::read_ods(
    path = raw_data_path, sheet = "lfpr"
  )

  # gva to output i ------------------------------------------------------------------
  
  eurostat_nama_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
    sheet = "nama"
  ) |> 
    filter(relationship != "extra", fingreen_industry_code != "U")
  
  gva_and_output_by_fingreen_industry <- gva_and_output |> 
    filter(unit == "CP_MEUR") |> 
    rename(eurostat_nace_r2 = nace_r2) |> 
    transform_categorization(
      new_mapping = eurostat_nama_to_fingreen_industry_map,
      old_mapping_name = "eurostat_nace_r2",
      new_mapping_name = "fingreen_industry_code",
      value_vars = "values",
      grouping_vars = c("na_item", "time")
    )

  res_gva_to_output_i <- gva_and_output_by_fingreen_industry |> 
    tidyr::pivot_wider(names_from = na_item, values_from = values) |> 
    mutate(gva_to_output_ratio = B1G / P1) |> 
    select(fingreen_industry_code, time, gva_to_output_ratio) |> 
    tidyr::pivot_wider(names_from = time, values_from = gva_to_output_ratio)      
  
  # p i --------------------------------------------------------------------

  cp_and_pyp_output_by_fingreen_industry <- gva_and_output |> 
    filter(unit %in% c("CP_MEUR", "PYP_MEUR"), na_item == "P1") |> 
    rename(eurostat_nace_r2 = nace_r2) |> 
    transform_categorization(
      new_mapping = eurostat_nama_to_fingreen_industry_map,
      old_mapping_name = "eurostat_nace_r2",
      new_mapping_name = "fingreen_industry_code",
      value_vars = "values",
      grouping_vars = c("unit", "time")
    ) |>
    tidyr::pivot_wider(names_from = unit, values_from = values)
  
  price_deflators <- cp_and_pyp_output_by_fingreen_industry |> 
    mutate(
      price_deflator = if_else(
        time == min(time),
        1,
        CP_MEUR / PYP_MEUR
      )
    )

  res_p_i <- price_deflators |> 
    group_by(fingreen_industry_code) |> 
    arrange(time) |> 
    mutate(p = cumprod(price_deflator)) |> 
    ungroup() |> 
    select(time, fingreen_industry_code, p) |> 
    tidyr::pivot_wider(names_from = time, values_from = p)

  # labour share -----------------------------------------------------------

  compensation_of_employees_by_fingreen_industry <- compensation_of_employees |> 
    rename(eurostat_nace_r2 = nace_r2) |> 
    transform_categorization(
      new_mapping = eurostat_nama_to_fingreen_industry_map,
      old_mapping_name = "eurostat_nace_r2",
      new_mapping_name = "fingreen_industry_code",
      value_vars = "values",
      grouping_vars = "time"
    ) |> 
    rename(compensation_of_employees = values)
    
  labour_share_by_fingreen_industry <- gva_and_output_by_fingreen_industry |> 
    filter(na_item == "B1G") |> 
    rename(value_added = values) |> 
    select(-na_item) |> 
    left_join(
      compensation_of_employees_by_fingreen_industry,
      by = c("time", "fingreen_industry_code")
    ) |> 
    mutate(labour_share = compensation_of_employees / value_added)

  res_labour_share <- labour_share_by_fingreen_industry |> 
    group_by(time) |> 
    summarise(
      value_added = sum(value_added),
      compensation_of_employees = sum(compensation_of_employees),
      .groups = "drop"
    ) |> 
    mutate(labour_share = compensation_of_employees / value_added)
  
  # labour force participation rate ----------------------------------------

  cat(
    "Labour force participation rates are corrected to match right",
    "levels of persons employed and unemployment rates. Check the documentation.\n",
    "These corrections must be set in the code manually if data creation country is changed.\n"
  )
  
  lfpr_corrections <- tibble::tibble(
    sex = rep(c("female", "male"), each = 3L),
    skill_level = rep(c("low", "middle", "high"), 2),
    correction = c(
      0.0330792226020817, 0.018040789097947, 0.02738201377682, 0.038942859296361,
      0.030855019301304, 0.0515599198428319
    ),
  ) |> 
    mutate(
      sex = factor(sex, levels = c("female", "male")),
      skill_level = factor(skill_level, levels = c("low", "middle", "high"))
    )

  lfpr_corrected <- lfpr |> 
    mutate(
      sex = factor(sex, levels = c("F", "M"), labels = c("female", "male")),
      skill_level = factor(
        isced11, levels = c("ED0-2", "ED3_4", "ED5-8"),
        labels = c("low", "middle", "high")
      )
    ) |> 
    left_join(lfpr_corrections, by = c("sex", "skill_level")) |> 
    # Correct the lfpr but limit between 0 and 1
    mutate(corrected_lfpr = (values / 100 + correction) |> pmin(1) |> pmax(0))

  res_lfpr_gs <- lfpr_corrected |> 
    select(sex, skill_level, corrected_lfpr, time) |> 
    arrange(sex, skill_level, time) |> 
    tidyr::pivot_wider(names_from = time, values_from = corrected_lfpr)

  # export -------------------------------------------------------------------
  
  datasets_to_write <- c("res_lambda_i", "res_gva_to_output_i", "res_p_i", "res_labour_share", "res_lfpr_gs")

  output_path <- paste0(results_dir, "full-model-calib-data.xlsx")

  writexl::write_xlsx(mget(datasets_to_write), path = output_path)

  return(output_path)

}
