# labour male share
# start from here https://ec.europa.eu/eurostat/databrowser/view/lfsa_egan22d__custom_19954320/default/table

# libraries ---------------------------------------------------------------

library(dplyr)

source("fingreen-r-utils.R")

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/labour/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
create_dir_if_not_exists(results_dir, "results")

# parameters ---------------------------------------------------------------
global_params <- config::get(file = "global-params.yml")

base_year <- global_params$base_year
geo <- global_params$geo

# source data --------------------------------------------------------------

# We get the data on nace r2 1 and 2 digit level industry classifications
# and then use a mapping table to pick the right ones and aggregate
# to fingreen industry level

employed_by_sex_and_industry_lvl1 <- eurostat::get_eurostat(
    "lfsa_egan2",
    time_format = "num",
    filters = list(
        geo = geo,
        time = base_year,
        age = "Y15-64",
        sex = c("T", "M")
    )
) |> 
  filter(!nace_r2 %in% c("TOTAL", "NRP")) |> 
  mutate(sex = factor(sex, levels = c("T", "M"), labels = c("total", "male"))) |> 
  tidyr::pivot_wider(names_from = sex, values_from = values) |> 
  fix_names()
  

employed_by_sex_and_industry_lvl2 <- eurostat::get_eurostat(
    "lfsa_egan22d",
    time_format = "num",
    filters = list(
        geo = geo,
        time = base_year,
        age = "Y15-64",
        sex = c("T", "M")
    )
) |> 
# not needed T98 Undifferentiated goods- and services-producing activities of private households for own use
  filter(!nace_r2 %in% c("TOTAL", "NRP", "UNK", "T98")) |> 
  mutate(sex = factor(sex, levels = c("T", "M"), labels = c("total", "male"))) |> 
  tidyr::pivot_wider(names_from = sex, values_from = values) |> 
  fix_names()

employed_by_sex_and_industry_lvl2_imputed <- employed_by_sex_and_industry_lvl2 |> 
  # first we impute the parent level information if the male share is missing from a detailed industry
  mutate(nace_1_digit = substr(nace_r2, 1, 1)) |> 
  left_join(
    rename(employed_by_sex_and_industry_lvl1, total_1_digit = total, male_1_digit = male),
    by = c("time", "geo", "unit", "freq", "nace_1_digit" = "nace_r2")
  ) |> 
  mutate(
    is_imputed = is.na(male) & !is.na(total),
    male = coalesce(male, total * male_1_digit / total_1_digit)
  ) |> 
  # if data for one subindustry is missing but the rest is there, we can allocate the remainder of the total
  group_by(nace_1_digit) |> 
  mutate(
    n_missing_in_group = sum(is.na(male)),
    total_sum_nonmissing_subindustries_in_group = sum(total, na.rm = T),
    male_sum_nonmissing_subindustries_in_group = sum(male, na.rm = T)
  ) |>
  ungroup() |> 
  mutate(
    is_imputed2 = is.na(male) & n_missing_in_group == 1,
    total = coalesce(
        total,
        if_else(n_missing_in_group == 1, total_1_digit - total_sum_nonmissing_subindustries_in_group, NA)
    ),
    male = coalesce(
        male,
        if_else(n_missing_in_group == 1, male_1_digit - male_sum_nonmissing_subindustries_in_group, NA)
    )
  )

# check that the imputation did not produce any silly values
stopifnot(min(employed_by_sex_and_industry_lvl2_imputed$total) > 0)
stopifnot(min(employed_by_sex_and_industry_lvl2_imputed$male) > 0)

nace_r2_1_and_2_digit_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/nace-r2-1-and-2-digit-to-fingreen-industry-map.xlsx"
)

employment_share_by_sex_by_fingreen_industry <- employed_by_sex_and_industry_lvl1 |> 
  bind_rows(employed_by_sex_and_industry_lvl2) |> 
  right_join(nace_r2_1_and_2_digit_to_fingreen_industry_map, by = "nace_r2", relationship = "many-to-many") |> 
  group_by(sex, fingreen_industry_code) |> 
  summarise(
    values = sum(1000 * values * coalesce(disaggregation_coefficient, 1)),
    .groups = "drop"
  ) |> 
  tidyr::pivot_wider(names_from = sex, values_from = values) |> 
  mutate(male_share = M / T)
  
