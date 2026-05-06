# This script calculates most of the labour demographics in one go. This
# includes employed persons by industry, skill share by industry and
# male share by industry and skill. There is not ready data that would
# have n of employed by industry skill and sex, (except EUKLEMS data which
# at least for Finland does not lead to sensible aggregates) so we have to put it
# together from multiple sources. We get the data as granular as we can,
# and then correct for males and females separately with RAS, to make
# sure that n by skill and by industry are sensible on the aggregate level.

# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("tidyr"))
stopifnot(is_installed("writexl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/labour/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
create_dir_if_not_exists(results_dir, "results")

# parameters -------------------------------------------------------------

geo <- "FI"
base_year <- 2010L
# some data is missing for e.g. industry B for skill share by industry,
# we use this to impute
imputation_year <- 2020L

# employed by sex and industry -------------------------------------------

# We get the data on nace r2 1 and 2 digit level industry classifications
# and then use a mapping table to pick the right ones and aggregate
# to fingreen industry level

# get_n_employed_by_fingreen_industry_and_sex
employed_by_sex_and_industry_lvl1 <- eurostat::get_eurostat(
    "lfsa_egan2",
    time_format = "num",
    filters = list(
        geo = geo,
        time = base_year,
        age = "Y15-64",
        freq = "A",
        unit = "THS_PER",
        sex = c("T", "F", "M")
    )
) |> 
  select(-unit, -freq, -age, -geo, -time) |> 
  # U ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES is generally not needed, and there is no data
  filter(!nace_r2 %in% c("TOTAL", "NRP", "U")) |> 
  mutate(
    sex = factor(sex, levels = c("T", "F", "M"), labels = c("total", "female", "male")),
    values = 1000 * values
  ) |> 
  tidyr::pivot_wider(names_from = sex, values_from = values) |> 
  fix_names()
  
employed_by_sex_and_industry_lvl1_imputed <- employed_by_sex_and_industry_lvl1 |> 
  mutate(
    male = coalesce(male, total - female),
    female = coalesce(female, total - male)
  )

employed_by_sex_and_industry_lvl2 <- eurostat::get_eurostat(
    "lfsa_egan22d",
    time_format = "num",
    filters = list(
        geo = geo,
        time = base_year,
        age = "Y15-64",
        freq = "A",
        unit = "THS_PER",
        sex = c("T", "F", "M")
    )
) |> 
  select(-unit, -freq, -age, -geo, -time) |> 
# No data for (and not needed) T98 Undifferentiated goods- and services-producing activities of private households for own use
  filter(!nace_r2 %in% c("TOTAL", "NRP", "UNK", "T98", "U99")) |> 
  mutate(
    sex = factor(sex, levels = c("T", "F", "M"), labels = c("total", "female", "male")),
    values = 1000 * values
  ) |> 
  tidyr::pivot_wider(names_from = sex, values_from = values) |> 
  fix_names()

employed_by_sex_and_industry_lvl2_imputed <- employed_by_sex_and_industry_lvl2 |> 
  # impute the parent level information if the male share is missing from a detailed industry
  mutate(nace_lvl1 = substr(nace_r2, 1, 1)) |> 
  left_join(
    rename(employed_by_sex_and_industry_lvl1_imputed, total_lvl1 = total, male_lvl1 = male, female_lvl1 = female),
    by = c("nace_lvl1" = "nace_r2")
  ) |> 
  mutate(
    male = coalesce(male, total - female, total * male_lvl1 / total_lvl1),
    female = coalesce(female, total - male)
  )

nace_r2_1_and_2_digit_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/nace-r2-1-and-2-digit-to-fingreen-industry-map.xlsx"
)

employed_by_sex_and_fingreen_industry <- employed_by_sex_and_industry_lvl1 |> 
  bind_rows(employed_by_sex_and_industry_lvl2_imputed) |> 
  right_join(nace_r2_1_and_2_digit_to_fingreen_industry_map, by = "nace_r2", relationship = "one-to-many") |> 
  group_by(fingreen_industry_code) |> 
  summarise(
    total = sum(total * coalesce(disaggregation_coefficient, 1)),
    female = sum(female * coalesce(disaggregation_coefficient, 1)),
    male = sum(male * coalesce(disaggregation_coefficient, 1)),
    .groups = "drop"
  ) |> 
  tidyr::pivot_longer(cols = all_of(c("total", "female", "male")), names_to = "sex", values_to = "n_employed")

# impute missing totals using the parent industries if only
# one subindustry is missing

employed_by_sex_and_industry_lvl1_long <- employed_by_sex_and_industry_lvl1_imputed |> 
  tidyr::pivot_longer(cols = all_of(c("total", "female", "male")), names_to = "sex", values_to = "n_employed_lvl1")

employed_by_sex_and_fingreen_industry_imputed <- employed_by_sex_and_fingreen_industry |> 
  mutate(nace_lvl1 = substr(fingreen_industry_code, 1, 1)) |> 
  left_join(employed_by_sex_and_industry_lvl1_long, by = c("nace_lvl1" = "nace_r2", "sex")) |> 
  group_by(sex, nace_lvl1) |> 
  mutate(
    n_subindustries_missing = sum(is.na(n_employed)),
    n_employed_nonmissing_subindustries = sum(n_employed, na.rm = T)
  ) |> 
  ungroup() |> 
  mutate(
    n_employed = coalesce(
      n_employed,
      if_else(n_subindustries_missing == 1, n_employed_lvl1 - n_employed_nonmissing_subindustries, NA)
    )
  ) |> 
  filter(sex != "total") |> # from here on we are only concerned with data by sex
  select(fingreen_industry_code, sex, n_employed)

n_missing_employed_by_sex_and_fingreen_industry <- sum(is.na(employed_by_sex_and_fingreen_industry_imputed$n_employed))
if(!identical(0L, n_missing_employed_by_sex_and_fingreen_industry)) {
  stop("Missing values were left in the number of employed persons per sex and industry. Deduction not successful.")
}

# employed by skill and industry -----------------------------------------

skill_levels <- c("low" = "ED0-2", "mid" = "ED3_4", "high" = "ED5-8")

# The source data is for employees, not employed persons. But as we correct it to
# be consistent with the distribution of employed persons per industry and
# on the other hand per skill, then it should not matter too much.

skill_share_by_industry <- eurostat::get_eurostat(
  "edat_lfs_9910",
  time_format = "num",
  filters = list(
    geo = geo,
    time = c(base_year, imputation_year),
    isced11 = skill_levels,
    age = "Y15-64",
    sex = "T"
  )
) |> 
  filter(nace_r2 != "U") # No data for U industry, and none needed

eurostat_edat_to_fingreen_industry_map <- readxl::read_xlsx(
  path = "source-data/mappings/nace-r2-aggregate-to-fingreen-industry-map.xlsx"
)

industries_w_missing_data <- skill_share_by_industry |> 
  filter(time == base_year) |> 
  group_by(nace_r2) |> 
  summarise(n_complete_obs = sum(!is.na(values)), .groups = "drop") |> 
  filter(n_complete_obs < 2)

imputed_from_other_year <- skill_share_by_industry |> 
  filter(time == imputation_year & nace_r2 %in% industries_w_missing_data$nace_r2) |> 
  group_by(nace_r2) |> 
  mutate(n_complete_obs = sum(!is.na(values))) |> 
  ungroup()

if(min(imputed_from_other_year$n_complete_obs < 2)) {
  cat("Imputation year did not have enough data.")
  print(imputed_from_other_year)
  stop("Imputation error. Try another imputation year.")
}

skill_share_by_industry_cleaned <- skill_share_by_industry |> 
  filter(time == base_year & !nace_r2 %in% industries_w_missing_data$nace_r2) |> 
  bind_rows(imputed_from_other_year) |> 
  mutate(
    skill_level = factor(isced11, levels = skill_levels, labels = names(skill_levels))
  ) |> 
  group_by(nace_r2) |> 
  arrange(nace_r2, desc(values)) |> 
  mutate(
    # for those industries that have data for 2 skill classes (last one is missing),
    # allocate the missing share from the first 2
    values = c(values[1:2], 100 - sum(values[1:2]))
  ) |> 
  ungroup() |> 
  filter(! nace_r2 %in% c("NRP", "TOTAL"))

skill_share_by_fingreen_industry <- skill_share_by_industry_cleaned |> 
  left_join(
    eurostat_edat_to_fingreen_industry_map, by = c("nace_r2"),
    relationship = "many-to-many"
  ) |> 
  group_by(fingreen_industry_code, skill_level) |> 
    # in cases where fingreen industry is aggregated from multiple industries in the data, use
  # the shares in the eurostat_edat_to_fingreen_industry_map to calculate a weighted average
  summarise(
    skill_share = weighted.mean(values, w = coalesce(share_of_fingreen_industry, 1)) / 100,
    .groups = "drop"
  )

# RAS correction ---------------------------------------------------------

# Correct shares with RAS to get good totals.
# First go from shares to absolute numbers, correct with RAS, and then back to shares.

get_proper_skill_shares <- function(geo, year){
  if(geo == "FI") {
    education_levels_statfin = c("9_X", "3_4", "5T8")
    employed_persons_skill_shares <- pxweb::pxweb_get(
      url = "https://statfin.stat.fi/PxWeb/api/v1/en/StatFin/tyti/statfin_tyti_pxt_13av.px",
      query = list(
        Vuosi = as.character(base_year),
        Sukupuoli = c("1", "2"),
        Koulutusaste = education_levels_statfin,
        Tiedot = "*"
      )
    ) |> 
      as.data.frame() |> 
      fix_names() |> 
      mutate(
        skill_level = case_when(
          # The names in the data are long but this does the job
          substr(educational_level, 1, 1) == 9 ~ "low",
          substr(educational_level, 1, 1) == 3 ~ "mid",
          substr(educational_level, 1, 1) == 5 ~ "high",
        ) |> factor(levels = c("low", "mid", "high")),
        sex = case_match(sex, "Males" ~ "male", "Females" ~ "female")
      ) |> 
      group_by(sex) |> 
      mutate(share_of_employed_persons_by_sex = employed_1000_persons / sum(employed_1000_persons)) |> 
      ungroup() |> 
      arrange(sex, skill_level)
    
    res <- employed_persons_skill_shares |> 
      select(sex, skill_level, share_of_employed_persons_by_sex)
    return(res)
  } else {
    # We can get the data from eurostat lfsa_egaed table, but at least for Finland
    # it is off by multiple percentage points
    stop("Getting proper skill shares for other geo than FI is not yet implemented. TODO!")
  }
}

proper_skill_shares <- get_proper_skill_shares(geo, base_year)

n_employed_by_sex <- employed_by_sex_and_fingreen_industry_imputed |> 
  group_by(sex) |> 
  summarise(n_employed_by_sex = sum(n_employed))

n_employed_by_sex_and_skill <- proper_skill_shares |> 
  left_join(n_employed_by_sex, by = c("sex")) |> 
  mutate(n_employed_by_sex_and_skill = n_employed_by_sex * share_of_employed_persons_by_sex)

# This is how we get a starting point of employed per sex, skill and industry, but
# we have to correct it to get sensible total ppl per sex, skill and industry
employed_by_sex_and_fingreen_industry_and_skill <- employed_by_sex_and_fingreen_industry_imputed |> 
  left_join(skill_share_by_fingreen_industry, by = c("fingreen_industry_code"), relationship = "many-to-many") |> 
  mutate(n_employed = n_employed * skill_share)

correct_employed_demographics_for_sex <- function(this_sex, employed_by_sex_and_fingreen_industry_and_skill) {

  employed_demographics_initial <- employed_by_sex_and_fingreen_industry_and_skill |> 
    filter(sex == this_sex) |> 
    select(skill_level, fingreen_industry_code, n_employed) |> 
    tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = n_employed) |> 
    arrange(skill_level)

  n_employed_matrix <- employed_demographics_initial |> 
    select(-skill_level) |> 
    as.matrix()

  desired_row_sums <- n_employed_by_sex_and_skill |> filter(sex == this_sex) |> pull(n_employed_by_sex_and_skill)
  desired_column_sums <- employed_by_sex_and_fingreen_industry_imputed |> 
    filter(sex == this_sex) |> 
    arrange(fingreen_industry_code) |> 
    pull(n_employed)
    

  n_employed_matrix_ipfp <- mipfp::Ipfp(
    seed = n_employed_matrix,
    target.list = list(1,2), # over 1=rows and 2=columns
    target.data = list(desired_row_sums, desired_column_sums)
  )

  employed_demographics_corrected <- bind_cols(
    select(employed_demographics_initial, skill_level),
    as.data.frame(n_employed_matrix_ipfp$x.hat)
  ) |> 
    mutate(sex = this_sex) |> 
    relocate(sex)
  
  return(employed_demographics_corrected)
}

employed_demographics_corrected <- lapply(
  c("female", "male"),
  FUN = correct_employed_demographics_for_sex,
  employed_by_sex_and_fingreen_industry_and_skill = employed_by_sex_and_fingreen_industry_and_skill
) |> 
  bind_rows()

employed_demographics_corrected_long <- employed_demographics_corrected |> 
  tidyr::pivot_longer(cols = A1:ST, names_to = "fingreen_industry_code", values_to = "n_employed")

# results ----------------------------------------------------------------

res_employed_by_industry <- employed_by_sex_and_fingreen_industry_imputed |> 
  group_by(fingreen_industry_code) |> 
  summarise(n_employed = sum(n_employed)) |> 
  tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = n_employed)

res_skill_share_by_industry <- employed_demographics_corrected_long |> 
  group_by(fingreen_industry_code, skill_level) |> 
  summarise(n_employed = sum(n_employed), .groups = "drop_last") |> 
  mutate(skill_share = n_employed / sum(n_employed)) |> 
  ungroup() |> 
  select(-n_employed) |> 
  tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = skill_share)

res_male_share_by_industry_and_skill <- employed_demographics_corrected_long |> 
  group_by(fingreen_industry_code, skill_level) |> 
  summarise(male_share = sum(if_else(sex == "male", n_employed, 0)) / sum(n_employed), .groups = "drop") |> 
  tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = male_share)

result_path_from_filename <- function(filename){
  res <- paste0(results_dir, filename, "-", tolower(geo), "-", base_year, ".xlsx")
}

writexl::write_xlsx(
  x = res_employed_by_industry,
  path = result_path_from_filename("n-employed-by-industry")
)
writexl::write_xlsx(
  x = res_skill_share_by_industry,
  path = result_path_from_filename("skill-share-by-industry")
)
writexl::write_xlsx(
  x = res_male_share_by_industry_and_skill,
  path = result_path_from_filename("male-share-by-industry-and-skill")
)
