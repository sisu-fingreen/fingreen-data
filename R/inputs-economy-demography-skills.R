pull_raw_data_inputs_economy_demography <- function(global_params) {

  source("R/fingreen-r-utils.R")

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/demography/")
  create_dir_if_not_exists(raw_data_dir, "raw data")

  base_year <- global_params$base_year
  geo <- global_params$geo
  if(!identical(geo, "FI")){
    stop("Other countries than FI not yet implemented for demography skills data")
  }
  # Satfin data is correct, eurostat is not (for Finland). Get the data from statfin, or your relevant national institution. Or at least verify
  # that eurostat data matches it.
  population_by_age_gender_skill <- pxweb::pxweb_get_data(
    url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/vkour/12bq.px",
    query = list(
      timeperiod_y = as.character(base_year),
      alue_23_20260101 = "SSS",
      ikaryhma_10_20180101 = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-"),
      sukupuoli_9_20180101 = c("SSS", "1", "2"),
      koulutusaste_17_20180101 = as.character(3:9),
      contentscode = "*"
    )
  )
  population_by_age_gender_skill_schema <- structure(
    list(
      column_name = c('Vuosi', 'Alue 2026', 'Ikä', 'Sukupuoli', 'Koulutusaste', '15 vuotta täyttänyt väestö 31.12'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('Vuosi', 'Alue 2026', 'Ikä', 'Sukupuoli', 'Koulutusaste', '15 vuotta täyttänyt väestö 31.12')
  )
  validate_schema(population_by_age_gender_skill, population_by_age_gender_skill_schema, "population_by_age_gender_skill")
  
  datasets_to_write <- c("population_by_age_gender_skill")
  
  output_path <- paste0(raw_data_dir, "demography.ods")
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)
  
  return(output_path)
}

create_inputs_economy_demography_skills <- function(raw_data_path, global_params) {

  # libraries ---------------------------------------------------------------

  library(dplyr)

  source("R/fingreen-r-utils.R")

  # needed but not loaded to the namespace

  # stopifnot(is_installed("tidyr"))
  # stopifnot(is_installed("pxweb"))

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/demography/")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------

  population_by_age_gender_skill <- readODS::read_ods(raw_data_path, sheet = "population_by_age_gender_skill") |> 
    fix_names()

  # process ----------------------------------------------------------------

  skill_share_by_gender_age <- population_by_age_gender_skill |> 
    mutate(
      gender = factor(sukupuoli, levels = c("Naiset", "Miehet", "Yhteensä"), labels = c("female", "male", "total")),
      age_class = case_match(
        ikä,
        c("15 - 19", "20 - 24") ~ "15-24",
        c("25 - 29", "30 - 34", "35 - 39", "40 - 44") ~ "25-44",
        c("45 - 49", "50 - 54", "55 - 59", "60 - 64") ~ "45-64",
        c("65 - 69", "70 - 74", "75 - 79", "80 -") ~ "65+"
      ) |> factor(levels = c("0-14", "15-24", "25-44", "45-64", "65+")),
      skill_level = case_match(
        substr(koulutusaste, 1, 1),
        c(NA, "9") ~ "low",
        as.character(3:4) ~ "middle",
        as.character(5:8) ~ "high"
      ) |> factor(levels = c("low", "middle", "high"))
    ) |> 
    group_by(age_class, gender, skill_level) |> 
    summarise(n = sum(x15_vuotta_täyttänyt_väestö_31_12)) |> 
    mutate(pct = n / sum(n)) |> 
    ungroup()

  res <- skill_share_by_gender_age |> 
    select(-n) |> 
    tidyr::pivot_wider(names_from = age_class, values_from = pct) |> 
    mutate(`0-14` = 0) |>
    relocate(`0-14`, .after = skill_level)

  # write results ----------------------------------------------------------
  
  output_path <- paste0(results_dir, "initial-skill-share-by-age-gender.ods")

  readODS::write_ods(
    x = res,
    path = output_path
  )
  
  return(output_path)

}
