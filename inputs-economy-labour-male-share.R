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
        freq = "A",
        unit = "THS_PER",
        sex = c("T", "M")
    )
) |> 
  select(-unit, -freq, -age, -geo, -time) |> 
  # U ACTIVITIES OF EXTRATERRITORIAL ORGANISATIONS AND BODIES is generally not needed, and there is no data
  filter(!nace_r2 %in% c("TOTAL", "NRP", "U")) |> 
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
        freq = "A",
        unit = "THS_PER",
        sex = c("T", "M")
    )
) |> 
  select(-unit, -freq, -age, -geo, -time) |> 
# No data for (and not needed) T98 Undifferentiated goods- and services-producing activities of private households for own use
  filter(!nace_r2 %in% c("TOTAL", "NRP", "UNK", "T98", "U99")) |> 
  mutate(sex = factor(sex, levels = c("T", "M"), labels = c("total", "male"))) |> 
  tidyr::pivot_wider(names_from = sex, values_from = values) |> 
  fix_names()

employed_by_sex_and_industry_lvl2_imputed <- employed_by_sex_and_industry_lvl2 |> 
  # impute the parent level information if the male share is missing from a detailed industry
  mutate(nace_lvl1 = substr(nace_r2, 1, 1)) |> 
  left_join(
    rename(employed_by_sex_and_industry_lvl1, total_lvl1 = total, male_lvl1 = male),
    by = c("nace_lvl1" = "nace_r2")
  ) |> 
  mutate(
    is_imputed = is.na(male) & !is.na(total),
    male = coalesce(male, total * male_lvl1 / total_lvl1)
  ) 

nace_r2_1_and_2_digit_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/nace-r2-1-and-2-digit-to-fingreen-industry-map.xlsx"
)

employment_share_by_sex_by_nace_lvl1 <- employed_by_sex_and_industry_lvl1 |> 
  mutate(male_share_lvl1 = male / total) |> 
  select(nace_r2, male_share_lvl1)

employment_share_by_sex_by_fingreen_industry <- employed_by_sex_and_industry_lvl1 |> 
  bind_rows(employed_by_sex_and_industry_lvl2_imputed) |> 
  right_join(nace_r2_1_and_2_digit_to_fingreen_industry_map, by = "nace_r2", relationship = "one-to-many") |> 
  group_by(fingreen_industry_code) |> 
  summarise(
    total = sum(1000 * total * coalesce(disaggregation_coefficient, 1)),
    male = sum(1000 * male * coalesce(disaggregation_coefficient, 1)),
    .groups = "drop"
  ) |> 
  mutate(male_share = male / total) |> 
  # impute the parent category value for the missing ones
  mutate(nace_lvl1 = substr(fingreen_industry_code, 1, 1)) |> 
  left_join(employment_share_by_sex_by_nace_lvl1, by = c("nace_lvl1" = "nace_r2")) |> 
  mutate(male_share = coalesce(male_share, male_share_lvl1))
  
res <- employment_share_by_sex_by_fingreen_industry |> 
  select(male_share, fingreen_industry_code) |> 
  tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = male_share)

writexl::write_xlsx(
  res,
  path = sprintf("%smale-share-by-industry-%s-%s.xlsx", results_dir, tolower(geo), base_year)
)

# TODO: get male share per industry and male share by skill and create the data from there. Other option: microdata