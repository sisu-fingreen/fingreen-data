pull_raw_data_inputs_economy_labour_unemployment <- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/labour/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  unemployment_by_sex_and_education <- eurostat::get_eurostat(
    id = "une_educ_a",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year,
      unit = c(
        "PC_ACT" # [Unemployment as] Percentage of population in the labour force
      ),
      age = "Y15-74"
    )
  )
  
  datasets_to_write <- c("unemployment_by_sex_and_education")
  
  output_path <- paste0(raw_data_dir, "unemployment.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}

create_inputs_economy_labour_unemployment <- function(raw_data_path, global_params) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  source("R/fingreen-r-utils.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------

  unemployment_by_sex_and_education <- readODS::read_ods(
    path = raw_data_path, sheet = "unemployment_by_sex_and_education"
  )

  # process ------------------------------------------------------------------

  unemployment_by_sex_and_skill <- unemployment_by_sex_and_education |> 
    filter(sex != "T") |> 
    mutate(
      skill_level = factor(
        isced11,
        levels = c("ED0-2","ED3_4", "ED5-8"),
        labels = c("low", "mid", "high")
      ),
      sex = factor(sex, levels = c("F", "M"), labels = c("f", "m"))
    ) |>
    tidyr::pivot_wider(names_from = unit, values_from = values) |> 
    mutate(share_unemployed = PC_ACT / 100) |> 
    select(sex, skill_level, share_unemployed)
  
  res_unemployment_by_sex_and_skill <- unemployment_by_sex_and_skill |> 
    arrange(sex, skill_level) |> 
    tidyr::pivot_wider(names_from = skill_level, values_from = share_unemployed)
  
  # export -------------------------------------------------------------------
  
  datasets_to_write <- c("res_unemployment_by_sex_and_skill")

  output_path <- paste0(results_dir, "unemployment_by_sex_and_skill.ods")

  readODS::write_ods(mget(datasets_to_write), path = output_path)

  return(output_path)

}