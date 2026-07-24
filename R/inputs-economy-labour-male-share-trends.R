create_inputs_economy_labour_male_share_trends <- function(
  male_share_microdata_path,
  global_params
) {

  ##__________________________________________________
  ##
  ## Code for calculating male share trends from microdata output 
  ##  
  ##  By Teemu Koskimäki and Topi-Matti Heikkola
  ##  Email: teemu.koskimaki@live.fi
  ##  Started:  2026-02-02  (y-m-d)
  ##_________________________________________________
  ##


  # SETUP

  library(dplyr)
  source("R/fingreen-r-utils.R", local = TRUE)

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
  create_dir_if_not_exists(results_dir, "results")

  # load data
  male_shares <- readxl::read_xlsx(male_share_microdata_path) |> 
    mutate(Skill = factor(Skill, levels = c("low", "mid", "high")))

  # fit a separate linear regression of male_share on Year for each Code–Skill group and returns the estimated slope. 
  # The slope measures the yearly change in male_share within each group.

  male_share_trend_wide <- male_shares %>%
    group_by(Code, Skill) %>%
    summarise(trend = coef(lm(male_share ~ Year))[["Year"]], .groups = "drop") %>%
    tidyr::pivot_wider(names_from = Code, values_from = trend)

  # Export
  export_path <- paste0(results_dir, "male-share-trend.ods")
  readODS::write_ods(male_share_trend_wide, path = export_path)

  return(export_path)
}