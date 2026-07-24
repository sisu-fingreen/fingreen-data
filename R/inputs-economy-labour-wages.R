pull_raw_data_inputs_economy_labour_wages <- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/labour/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  wages_and_salaries_by_industry <- eurostat::get_eurostat(
    "nama_10_a64",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      unit = "CP_MEUR",
      na_item = "D11" # D11 Wages and salaries
    )
  )
  
  datasets_to_write <- c("wages_and_salaries_by_industry")
  
  output_path <- paste0(raw_data_dir, "wages.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}

create_inputs_economy_labour_wages <- function(
  raw_data_path,
  raw_data_labour_hours_path,
  labour_demographics_path,
  labour_unemployment_path,
  wage_distribution_data_path,
  global_params
) {

  # libraries ---------------------------------------------------------------

  library(dplyr)

  source("R/fingreen-r-utils.R", local = TRUE)

  # parameters -------------------------------------------------------------

  base_year <- global_params$base_year
  geo <- global_params$geo
  
  skill_levels <- c("low", "mid", "high")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------

  wages_and_salaries_by_industry <- readODS::read_ods(
    raw_data_path, sheet = "wages_and_salaries_by_industry"
  ) |> 
    mutate(wages_and_salaries = 1e6 * values) |> 
    select(-values, -freq, -unit)

  eurostat_nama_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
    sheet = "nama"
  )

  hours_worked_by_industry <- readODS::read_ods(
    raw_data_labour_hours_path, sheet = "hours_worked_by_industry"
  ) |> 
    mutate(hours_worked = 1000 * values) |> 
    select(-values, -freq, -unit)
  
  relative_wage_stats <- readODS::read_ods(
    path = wage_distribution_data_path,
    sheet = "relative_wage_stats"
  ) |> 
    mutate(
      sex = factor(sex, levels = c("F", "M"), labels = c("f", "m")),
      skill_adjusted = factor(skill_adjusted, levels = skill_levels)
    )
  
  n_employed_by_fingreen_industry <- readODS::read_ods(
    path = labour_demographics_path[grepl("n-employed-by-industry", labour_demographics_path)]
  )
    
  skill_share_by_fingreen_industry <- readODS::read_ods(
    path = labour_demographics_path[grepl("skill-share-by-industry", labour_demographics_path)]
  ) |> 
    mutate(skill_level = factor(skill_level, levels = skill_levels))
  
  male_share_by_fingreen_industry_and_skill <- readODS::read_ods(
    path = labour_demographics_path[grepl("male-share-by-industry-and-skill", labour_demographics_path)]
  ) |> 
    mutate(
      skill_level = factor(skill_level, levels = skill_levels)
    )
  
  unemployment_by_sex_and_skill <- readODS::read_ods(
    path = labour_unemployment_path,
    sheet = "res_unemployment_by_sex_and_skill"
  ) |> 
    tidyr::pivot_longer(cols = low:high, names_to = "skill_level", values_to = "share_unemployed") |> 
    mutate(
      skill_level = factor(skill_level, levels = skill_levels),
      sex = factor(sex, levels = c("f", "m"))
    )

  # processing -------------------------------------------------------------
  
  share_of_self_employed_by_industry <- hours_worked_by_industry |> 
    tidyr::pivot_wider(names_from = na_item, values_from = hours_worked) |> 
    mutate(share_of_self_employed = coalesce(SELF_DC, 0) / EMP_DC) |> 
    select(geo, time, nace_r2, share_of_self_employed)

  hourly_wage_by_industry <- hours_worked_by_industry |>
    filter(na_item == "EMP_DC") |>
    select(geo, time, nace_r2, hours_worked) |>
    left_join(wages_and_salaries_by_industry, by = c("geo", "time", "nace_r2")) |> 
    left_join(share_of_self_employed_by_industry, by = c("geo", "time", "nace_r2")) |> 
    mutate(
      wages_self_employed_adjusted = wages_and_salaries / (1 - share_of_self_employed),
      wages_per_hours_worked = wages_self_employed_adjusted / hours_worked
    )

  hourly_wage_by_fingreen_industry <- hourly_wage_by_industry |> 
    inner_join(
      eurostat_nama_to_fingreen_industry_map |> filter(relationship != "extra"),
      by = c("nace_r2" = "eurostat_nace_r2")
    ) |> 
    group_by(geo, time, fingreen_industry_code) |> 
    summarise(
      wages_self_employed_adjusted = sum(wages_self_employed_adjusted * coalesce(disaggregation_coefficient, 1)),
      hours_worked = sum(hours_worked * coalesce(disaggregation_coefficient, 1)),
      .groups = "drop"
    ) |> 
    mutate(wages_per_hours_worked = wages_self_employed_adjusted / hours_worked)
    
  hourly_wages_long <- relative_wage_stats |> 
    left_join(hourly_wage_by_fingreen_industry, by = "fingreen_industry_code") |> 
    select(-hours_worked) |> 
    mutate(
      avg_wage = relative_avg_wage * wages_per_hours_worked,
      sd_wage = relative_sd_wage * wages_per_hours_worked,
      coef_disp = relative_coef_disp * wages_per_hours_worked
    )
  
  res_hourly_wages <- hourly_wages_long |>
    select(sex, skill_adjusted, fingreen_industry_code, avg_wage) |> 
    arrange(fingreen_industry_code, desc(sex), skill_adjusted) |> 
    tidyr::pivot_wider(names_from = c("sex", "skill_adjusted"), values_from = "avg_wage")

  res_coefficient_wage_dispersion <- hourly_wages_long |>
    select(sex, skill_adjusted, fingreen_industry_code, coef_disp) |> 
    arrange(fingreen_industry_code, desc(sex), skill_adjusted) |> 
    tidyr::pivot_wider(names_from = c("sex", "skill_adjusted"), values_from = "coef_disp")
  
  # avg wage weighed by unemployed by sex and skill
  # This is needed to calculate "uw" for the model: the share of wage paid as unemployment benefits
  # It gets a bit complicated as we have to go the the sex and skill level with all data,
  # and that requires calculations using the skill shares and male shares per industry

  total_hours_by_fingreen_industry <- hourly_wage_by_fingreen_industry$hours_worked
  n_employed_by_fingreen_industry_vec <- n_employed_by_fingreen_industry[1, ] |> as.numeric()
  
  skill_share_matrix <- skill_share_by_fingreen_industry |>
    select(-skill_level) |> 
    as.matrix() |> 
    t()
  male_share_matrix <- male_share_by_fingreen_industry_and_skill |> 
    select(-skill_level) |> 
    as.matrix() |> 
    t()

  male_hours_by_fingreen_industry_and_skill <- (total_hours_by_fingreen_industry *
    skill_share_matrix *
    male_share_matrix) |> 
    t() |> 
    as.data.frame() |> 
    mutate(
      skill_level = factor(skill_levels, levels = skill_levels),
      sex = factor("m", levels = c("f", "m")),
      measure = "total_hours",
    )

  male_employed_by_fingreen_industry_and_skill <- (
    n_employed_by_fingreen_industry_vec *
      skill_share_matrix *
      male_share_matrix
  ) |> 
    t() |> 
    as.data.frame() |> 
    mutate(
      skill_level = factor(skill_levels, levels = skill_levels),
      sex = factor("m", levels = c("f", "m")),
      measure = "n_employed"
    )

  female_hours_by_fingreen_industry_and_skill <- (total_hours_by_fingreen_industry *
    skill_share_matrix *
    (1 - male_share_matrix)) |> 
    t() |> 
    as.data.frame() |> 
    mutate(
      skill_level = factor(skill_levels, levels = skill_levels),
      sex = factor("f", levels = c("f", "m")),
      measure = "total_hours"
    )
  
  female_employed_by_fingreen_industry_and_skill <- (
    n_employed_by_fingreen_industry_vec *
      skill_share_matrix *
      (1 - male_share_matrix)
  ) |> 
    t() |> 
    as.data.frame() |> 
    mutate(
      skill_level = factor(skill_levels, levels = skill_levels),
      sex = factor("f", levels = c("f", "m")),
      measure = "n_employed"
    )
  
  avg_annual_wage_by_sex_and_skill <- bind_rows(
    male_hours_by_fingreen_industry_and_skill,
    male_employed_by_fingreen_industry_and_skill,
    female_hours_by_fingreen_industry_and_skill,
    female_employed_by_fingreen_industry_and_skill
  ) |> 
    tidyr::pivot_longer(cols = A1:ST, names_to = "fingreen_industry_code") |> 
    tidyr::pivot_wider(names_from = measure, values_from = value) |> 
    left_join(
      hourly_wages_long,
      by = c("fingreen_industry_code", "sex", "skill_level" = "skill_adjusted")
    ) |> 
    mutate(total_wages = total_hours * avg_wage) |> 
    group_by(sex, skill_level) |> 
    summarise(
      total_wages = sum(total_wages),
      n_employed = sum(n_employed),
      .groups = "drop"
    ) |> 
    mutate(avg_annual_wage = total_wages / n_employed)

  # validate that the totals are still close enough. The difference
  # stems from the wage distribution statistics, which use a different
  # distribution of sex and skill from microdata. But let's try a 2 percent threshold
  wage_total_error <- abs(
    1 - sum(avg_annual_wage_by_sex_and_skill$total_wages) / sum(hourly_wage_by_fingreen_industry$wages_self_employed_adjusted)
  )
  wage_total_error_threshold <- 0.02
  if(wage_total_error > wage_total_error_threshold){
    stop(
      "There is an issue with the wage distribution calculations.\nThe total error was ",
      scales::percent(wage_total_error, accuracy = 0.01),
      " while the threshold is set at ",
      scales::percent(wage_total_error_threshold, accuracy = 0.01)
    )
  }

  # weighed by unemployed
  avg_annual_wage_weighed_by_u_by_sex_and_skill <- avg_annual_wage_by_sex_and_skill |> 
    left_join(unemployment_by_sex_and_skill, by = c("sex", "skill_level")) |> 
    # why use the ratio and not the share_unemployed directly? Well, think if share unemployed were eg. 0.9
    mutate(n_unemployed = n_employed * share_unemployed / (1 - share_unemployed)) |> 
    summarise(avg_annual_wage_uw = weighted.mean(avg_annual_wage, w = n_unemployed))

  # output results ---------------------------------------------------------

  datasets_to_write <- c(
    "res_hourly_wages",
    "res_coefficient_wage_dispersion",
    "avg_annual_wage_weighed_by_u_by_sex_and_skill"
  )
  
  output_path <- paste0(results_dir, "wages.ods")
  
  readODS::write_ods(mget(datasets_to_write), path = output_path)
  
  return(output_path)

}