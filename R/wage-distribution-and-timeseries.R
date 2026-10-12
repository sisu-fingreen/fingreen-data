create_wage_distribution_and_timeseries <- function(wage_microdata_path, global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  if(geo != "FI") {
    stop("Wage microdata analysis for other countries than FI not implemented.")
  }
  
  working_directory <- getwd()
  
  results_dir <- paste0(working_directory, "/results/intermediate/")
  create_dir_if_not_exists(results_dir, "results")

  wage_microdata <- readxl::read_xlsx(wage_microdata_path) |> 
    fix_names() |> 
    mutate(
      sex = factor(sex, levels = c("F", "M")),
      skill_adjusted = factor(skill_adjusted, levels = c("low", "mid", "high"))
    ) |> 
    filter(!is.na(year)) |> 
    tidyr::pivot_longer(cols = c("avg_wage", "sd_wage", "coef_disp"), names_to = "measure")

  # A3 industry has data only for 2012, so we use that to impute for the base_year
  wage_microdata_a3_imputation_mid_high <- wage_microdata |> 
    filter(year == 2012 & code == "A3")
  # Also for A3, replicate mid skill stats to low skilled, but set n 0 as to not bias
  # average calculations
  wage_microdata_a3_imputation_low <- wage_microdata_a3_imputation_mid_high |> 
    filter(skill_adjusted == "mid") |> 
    mutate(
      n = 0,
      skill_adjusted = factor("low", levels = c("low", "mid", "high"))
    )
  
  wage_microdata_a3_imputed <- bind_rows(wage_microdata_a3_imputation_mid_high, wage_microdata_a3_imputation_low) |> 
    select(-year) |> 
    cross_join(distinct(wage_microdata, year)) |> 
    mutate(
      value = convert_eur_value_between_years(value, 2012, year, geo = geo)
    )

  
  wage_microdata_imputed <- wage_microdata |> 
    filter(code != "A3") |> 
    bind_rows(wage_microdata_a3_imputed) |>
    # Some observations are completely missing so create those
    tidyr::complete(sex, skill_adjusted, code, year, measure) |> 
    # Fill missing values forward, luckily we don't have other missing than A3 for 2010,
    # which is already taken care of
    group_by(sex, skill_adjusted, code, measure) |> 
    arrange(year) |> 
    mutate(
      filled_value = value,
      fill_year = if_else(is.na(value), NA, year)
    ) |> 
    tidyr::fill(filled_value, fill_year) |> 
    ungroup() |> 
    mutate(
      value = coalesce(
        value,
        convert_eur_value_between_years(filled_value, fill_year, year, geo = geo),
      )
    ) |> 
    arrange(sex, skill_adjusted, code, measure, year)

  wage_microdata_base_year_imputed <- wage_microdata_imputed |>
    filter(year == base_year) |> 
    select(-filled_value, -fill_year) |> 
    tidyr::pivot_wider(names_from = measure, values_from = value)

  if(nrow(wage_microdata_base_year_imputed) != 39*3*2){
    stop(
      "Some industry, skill or sex is missing data. ",
      "Check the source data and add imputation step if needed."
    )
  }

  mean_wage_by_industry <- wage_microdata_base_year_imputed |> 
    group_by(code) |> 
    summarise(
      mean_wage = weighted.mean(x = avg_wage, w = n)
    )
  
  relative_wage_stats <- wage_microdata_base_year_imputed |> 
    left_join(mean_wage_by_industry, by = "code") |> 
    mutate(
      relative_avg_wage = avg_wage / mean_wage,
      relative_sd_wage = sd_wage / mean_wage,
      relative_coef_disp = coef_disp / mean_wage
    ) |> 
    select(-mean_wage, -avg_wage, -sd_wage, -coef_disp) |> 
    rename(fingreen_industry_code = code)
  
  wage_timeseries <- wage_microdata_imputed |> 
    filter(measure == "avg_wage") |> 
    select(year, sex, code, skill_adjusted, value) |> 
    tidyr::pivot_wider(names_from = year, values_from = value) |> 
    arrange(sex, skill_adjusted, code) |> 
    relocate(sex, skill_adjusted, code)
    
  datasets_to_write <- c("relative_wage_stats", "wage_timeseries")
  
  output_path <- paste0(results_dir, "wage-distribution-and-timeseries.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}