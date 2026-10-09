create_wage_distribution <- function(wage_microdata_path, global_params) {
  
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
    )

  # A3 industry has data only for 2012, so we use that to impute for the base_year
  wage_microdata_a3_imputation_mid_high <- wage_microdata |> 
    filter(year == 2012 & code == "A3") |> 
    mutate(
      year = base_year,
      avg_wage = convert_eur_value_between_years(avg_wage, 2012, base_year),
      sd_wage = convert_eur_value_between_years(sd_wage, 2012, base_year),
      coef_disp = convert_eur_value_between_years(coef_disp, 2012, base_year)
    )
  # Also for A3, replicate mid skill stats to low skilled, but set n 0 as to not bias
  # average calculations
  wage_microdata_a3_imputation_low <- wage_microdata_a3_imputation_mid_high |> 
    filter(skill_adjusted == "mid") |> 
    mutate(
      n = 0,
      skill_adjusted = factor("low", levels = c("low", "mid", "high"))
    )
  
  wage_microdata_imputed <- wage_microdata |> 
    bind_rows(wage_microdata_a3_imputation_mid_high, wage_microdata_a3_imputation_low) |> 
    arrange(sex, skill_adjusted, code)

  # validate that we have the data for the base year
  wage_microdata_base_year_imputed <- wage_microdata_imputed |>
    filter(year == base_year & !is.na(avg_wage))
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
    
  datasets_to_write <- c("relative_wage_stats")
  
  output_path <- paste0(results_dir, "wage-distribution.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}