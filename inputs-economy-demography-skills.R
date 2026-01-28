# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("tidyr"))
stopifnot(is_installed("pxweb"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/demography/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/demography/")
create_dir_if_not_exists(results_dir, "results")

# source data --------------------------------------------------------------

# Satfin data is correct, eurostat is not. Get the data from statfin
population_by_age_gender_skill <- pxweb::pxweb_get(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/vkour/statfin_vkour_pxt_12bq.px",
  query = list(
    Vuosi = "2010",
    Alue = "SSS",
    Ikä = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-"),
    Sukupuoli = c("SSS", "1", "2"),
    Koulutusaste = as.character(3:9),
    Tiedot = "*"
  )
) |> 
  as.data.frame() |> 
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

writexl::write_xlsx(
  x = res,
  path = paste0(results_dir, "initial-skill-share-by-age-gender.xlsx")
)
