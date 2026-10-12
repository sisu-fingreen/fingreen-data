pull_raw_data_inputs_economy_labour_demographics  <- function(
  global_params,
  imputation_year
) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo
  
  current_year <- Sys.Date() |> format.Date(format = "%Y") |> as.integer()
  if(base_year > current_year - 2L) {
    stop("Cannot create data newer than current year - 2")
  }
  years <- (base_year - 1L):(current_year - 2L)

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/labour/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  employed_by_industry <- eurostat::get_eurostat(
    "nama_10_a64_e",
    time_format = "num",
    filters = list(
      geo = geo,
      time = years,
      na_item = "EMP_DC",
      unit = "THS_PER"
    )
  )
  employed_by_industry_schema <- structure(
    list(
      column_name = c('freq', 'unit', 'nace_r2', 'na_item', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('freq', 'unit', 'nace_r2', 'na_item', 'geo', 'time', 'values')
  )
  validate_schema(employed_by_industry, employed_by_industry_schema, "employed_by_industry")

  employed_by_sex_and_industry <- pxweb::pxweb_get_data(
    url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/tyokay/115i.px",
    query = list(
      alue_23_20260101 = "SSS", # This means area is all of Finland
      toimiala_108_20230101 = "*", # All industries
      timeperiod_y = as.character(years),
      sukupuoli_9_20180101 = c("SSS", "1", "2"), # sex: all, 1=male, 2=female
      contentscode = "*"
    ),
    variable.value.type = "code"
  )
  employed_by_sex_and_industry_schema <- structure(
    list(
      column_name = c('Area (residence)', 'Industry', 'Year', 'Sex', 'Employed'),
      column_type = c('character', 'character', 'character', 'character', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('Area (residence)', 'Industry', 'Year', 'Sex', 'Employed')
  )
  validate_schema(employed_by_sex_and_industry, employed_by_sex_and_industry_schema, "employed_by_sex_and_industry")

  skill_levels <- c("low" = "ED0-2", "mid" = "ED3_4", "high" = "ED5-8")

  skill_share_by_industry <- eurostat::get_eurostat(
    "edat_lfs_9910",
    time_format = "num",
    filters = list(
      geo = geo,
      time = years,
      isced11 = skill_levels,
      age = "Y15-64",
      sex = "T"
    )
  )
  skill_share_by_industry_schema <- structure(
    list(
      column_name = c('freq', 'unit', 'nace_r2', 'isced11', 'age', 'sex', 'geo', 'time', 'values'),
      column_type = c('character', 'character', 'character', 'character', 'character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('freq', 'unit', 'nace_r2', 'isced11', 'age', 'sex', 'geo', 'time', 'values')
  )
  validate_schema(skill_share_by_industry, skill_share_by_industry_schema, "skill_share_by_industry")

  
  if(!identical(geo, "FI")){
    stop(
      "Getting proper skill shares for other geo than FI is not yet implemented.",
      "Eurostat data is incorrect at least for Finland, so Statfin is used."
    )
  }

  education_levels_statfin = c("9_X", "3_4", "5T8")
  employed_persons_skill_shares <- pxweb::pxweb_get_data(
    url = "https://statfin.stat.fi/PxWeb/api/v1/en/StatFin/tyti/13av.px",
    query = list(
      timeperiod_y = as.character(years),
      sukupuoli_9_20180101 = c("1", "2"),
      koulutusaste_17_20180101 = education_levels_statfin,
      contentscode = "*"
    )
  )
  employed_persons_skill_shares_schema <- structure(
    list(
      column_name = c('Year', 'Sex', 'Educational level', 'Employed, 1000 persons'),
      column_type = c('character', 'character', 'character', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('Year', 'Sex', 'Educational level', 'Employed, 1000 persons')
  )
  validate_schema(employed_persons_skill_shares, employed_persons_skill_shares_schema, "employed_persons_skill_shares")

  datasets_to_write <- c(
    "employed_by_industry",
    "employed_by_sex_and_industry",
    "skill_share_by_industry",
    "employed_persons_skill_shares"
  )
  
  output_path <- paste0(raw_data_dir, "demographics.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}  

create_inputs_economy_labour_demographics <- function(
  raw_data_path,
  global_params,
  # some data is missing for e.g. industry B for skill share by industry,
  # we use this to impute
  imputation_year
) {

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

  source("R/fingreen-r-utils.R")

  # needed but not loaded to the namespace

  # stopifnot(is_installed("tidyr"))
  # stopifnot(is_installed("writexl"))

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
  create_dir_if_not_exists(results_dir, "results")

  # parameters -------------------------------------------------------------

  geo <- global_params$geo
  base_year <- global_params$base_year |> as.integer()

  current_year <- Sys.Date() |> format.Date(format = "%Y") |> as.integer()

  # employed by sex and industry -------------------------------------------

  # We get the data on nace r2 1 and 2 digit level industry classifications
  # and then use a mapping table to pick the right ones and aggregate
  # to fingreen industry level

  # get_n_employed_by_fingreen_industry_and_sex

  employed_by_industry <- readODS::read_ods(raw_data_path, sheet = "employed_by_industry")
  
  employed_by_sex_and_industry <- readODS::read_ods(raw_data_path, sheet = "employed_by_sex_and_industry") |> 
    fix_names() |> 
    rename(time = year) |> 
    mutate(sex = factor(sex, levels = c("2", "1", "SSS"), labels = c("female", "male", "total")))
  
  statfin_to_fingreen_industry_map <- readODS::read_ods(
    "source-data/mappings/statfin-labour-stats-tol-2008-to-fingreen-industry-map.ods"
  )

  employed_by_sex_and_fingreen_industry <- employed_by_sex_and_industry |> 
    left_join(
      statfin_to_fingreen_industry_map,
      by = c("industry" = "tol_2008"),
      relationship = "many-to-many"
    ) |> 
    filter(relationship != "extra", sex != "total") |> 
    group_by(time, fingreen_industry_code, sex) |> 
    summarise(
      n_employed = sum(employed * coalesce(disaggregation_coefficient, 1)),
      .groups = "drop"
    )

  n_missing_employed_by_sex_and_fingreen_industry <- sum(is.na(employed_by_sex_and_fingreen_industry$n_employed))
  if(!identical(0L, n_missing_employed_by_sex_and_fingreen_industry)) {
    stop("Missing values were left in the number of employed persons per sex and industry. Deduction not successful.")
  }

  # We have to use national accounting (NA) for industry totals, but it does not have sex, so use the above
  # to calculate male share, and then construct the by-industry-and-sex data from the by-industry NA totals.

  male_share_by_fingreen_industry <- employed_by_sex_and_fingreen_industry |> 
    tidyr::pivot_wider(names_from = "sex", values_from = n_employed) |> 
    mutate(male_share = male / (male + female)) |> 
    select(time, fingreen_industry_code, male_share)

  eurostat_nama_industry_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
    sheet = "nama"
  )

  employed_by_fingreen_industry <- employed_by_industry |> 
    inner_join(
      filter(eurostat_nama_industry_to_fingreen_industry_map, relationship != "extra"),
      by = c("nace_r2" = "eurostat_nace_r2"),
      relationship = "many-to-many"
    ) |> 
    group_by(time,fingreen_industry_code) |> 
    summarise(
      n_employed = 1000 * sum(values * coalesce(disaggregation_coefficient, 1)),
      .groups = "drop"
    )

  employed_by_sex_and_fingreen_industry_calculated <- employed_by_fingreen_industry |> 
    left_join(male_share_by_fingreen_industry, by = c("time", "fingreen_industry_code")) |> 
    mutate(
      female = (1- male_share) * n_employed,
      male = male_share * n_employed
    ) |>
    select(-n_employed, -male_share) |> 
    tidyr::pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "n_employed")

  # employed by skill and industry -----------------------------------------

  skill_levels <- c("low" = "ED0-2", "mid" = "ED3_4", "high" = "ED5-8")

  # The source data is for employees, not employed persons. But as we correct it to
  # be consistent with the distribution of employed persons per industry and
  # on the other hand per skill, then it should not matter too much.

  skill_share_by_industry <- readODS::read_ods(raw_data_path, sheet = "skill_share_by_industry") |> 
    filter(!nace_r2 %in% c("U", "NRP")) # No data for U industry, and none needed

  eurostat_edat_to_fingreen_industry_map <- readxl::read_xlsx(
    path = "source-data/mappings/nace-r2-aggregate-to-fingreen-industry-map.xlsx"
  )

  inds_and_years_w_missing_data <- skill_share_by_industry |> 
    group_by(time, nace_r2) |> 
    summarise(n_complete_obs = sum(!is.na(values)), .groups = "drop") |> 
    filter(n_complete_obs < 2)
  
  missing_data_n <- nrow(inds_and_years_w_missing_data)

  values_for_imputing <- skill_share_by_industry |> 
    filter(nace_r2 %in% inds_and_years_w_missing_data$nace_r2) |> 
    group_by(time, nace_r2) |> 
    mutate(n_complete_obs = sum(!is.na(values))) |> 
    ungroup() |> 
    # need at least two of the 3 skill categories to infer all 3
    filter(n_complete_obs > 1) |> 
    group_by(nace_r2) |> 
    # pick the data from the first suitable year
    filter(time == min(time)) |> 
    ungroup()

  if(min(values_for_imputing$n_complete_obs) < 2 & (missing_data_n > 0)) {
    catn("There were", missing_data_n, "industry+year combinations with missing values.")
    catn("Did not have enough data for imputation. Available data below.")
    print(values_for_imputing)
    stop("Imputation error. You need to fix the data manually or come up with another solution.")
  }

  imputed_from_other_year <- left_join(
    select(inds_and_years_w_missing_data, time, nace_r2),
    select(values_for_imputing, -time),
    by = "nace_r2",
    relationship = "many-to-many"
  )

  skill_share_by_industry_cleaned <- skill_share_by_industry |> 
    anti_join(imputed_from_other_year, by = c("time", "nace_r2")) |> 
    bind_rows(imputed_from_other_year) |> 
    mutate(
      skill_level = factor(isced11, levels = skill_levels, labels = names(skill_levels))
    ) |> 
    group_by(time, nace_r2) |> 
    arrange(time, nace_r2, desc(values)) |> 
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
    group_by(time, fingreen_industry_code, skill_level) |> 
      # in cases where fingreen industry is aggregated from multiple industries in the data, use
    # the shares in the eurostat_edat_to_fingreen_industry_map to calculate a weighted average
    summarise(
      skill_share = weighted.mean(values, w = coalesce(share_of_fingreen_industry, 1)) / 100,
      .groups = "drop"
    )

  # RAS correction ---------------------------------------------------------

  # Correct shares with RAS to get good totals.
  # First go from shares to absolute numbers, correct with RAS, and then back to shares.

  calculate_proper_skill_shares <- function(raw_data_path){
    if(geo == "FI") {
      education_levels_statfin = c("9_X", "3_4", "5T8")
      employed_persons_skill_shares <- readODS::read_ods(raw_data_path, sheet = "employed_persons_skill_shares") |> 
        as.data.frame() |> 
        fix_names() |> 
        mutate(
          time = as.integer(year),
          skill_level = case_when(
            # The names in the data are long but this does the job
            substr(educational_level, 1, 1) == 9 ~ "low",
            substr(educational_level, 1, 1) == 3 ~ "mid",
            substr(educational_level, 1, 1) == 5 ~ "high",
          ) |> factor(levels = c("low", "mid", "high")),
          sex = replace_values(sex, "Males" ~ "male", "Females" ~ "female")
        ) |> 
        group_by(time, sex) |> 
        mutate(share_of_employed_persons_by_sex = employed_1000_persons / sum(employed_1000_persons)) |> 
        ungroup() |> 
        arrange(time, sex, skill_level)
      
      res <- employed_persons_skill_shares |> 
        select(time, sex, skill_level, share_of_employed_persons_by_sex)
      return(res)
    } else {
      # We can get the data from eurostat lfsa_egaed table, but at least for Finland
      # it is off by multiple percentage points
      stop("Getting proper skill shares for other geo than FI is not yet implemented. TODO!")
    }
  }

  proper_skill_shares <- calculate_proper_skill_shares(raw_data_path)

  n_employed_by_sex <- employed_by_sex_and_fingreen_industry_calculated |> 
    group_by(time, sex) |> 
    summarise(n_employed_by_sex = sum(n_employed), .groups = "drop")

  n_employed_by_sex_and_skill <- proper_skill_shares |> 
    left_join(n_employed_by_sex, by = c("time", "sex")) |> 
    mutate(n_employed_by_sex_and_skill = n_employed_by_sex * share_of_employed_persons_by_sex)

  # This is how we get a starting point of employed per sex, skill and industry, but
  # we have to correct it to get sensible total ppl per sex, skill and industry
  employed_by_sex_and_fingreen_industry_and_skill <- employed_by_sex_and_fingreen_industry_calculated |> 
    left_join(skill_share_by_fingreen_industry, by = c("time", "fingreen_industry_code"), relationship = "many-to-many") |> 
    mutate(n_employed = n_employed * skill_share)

  correct_employed_demographics_for_year_and_sex <- function(this_year, this_sex, employed_by_sex_and_fingreen_industry_and_skill) {

    # catn(this_year, this_sex) # This can be handy for debugging

    employed_demographics_initial <- employed_by_sex_and_fingreen_industry_and_skill |> 
      filter(time == this_year, sex == this_sex) |> 
      select(time, skill_level, fingreen_industry_code, n_employed) |> 
      tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = n_employed) |> 
      arrange(skill_level)

    n_employed_matrix <- employed_demographics_initial |> 
      select(-time, -skill_level) |> 
      as.matrix()

    desired_row_sums <- n_employed_by_sex_and_skill |> filter(time == this_year, sex == this_sex) |> pull(n_employed_by_sex_and_skill)
    desired_column_sums <- employed_by_sex_and_fingreen_industry_calculated |> 
      filter(time == this_year, sex == this_sex) |> 
      arrange(fingreen_industry_code) |> 
      pull(n_employed)

    n_employed_matrix_ipfp <- mipfp::Ipfp(
      seed = n_employed_matrix,
      target.list = list(1,2), # over 1=rows and 2=columns
      target.data = list(desired_row_sums, desired_column_sums),
      tol.margins = 1e-7
    )

    employed_demographics_corrected <- bind_cols(
      select(employed_demographics_initial, time, skill_level),
      as.data.frame(n_employed_matrix_ipfp$x.hat)
    ) |> 
      mutate(sex = this_sex) |> 
      relocate(sex)
    
    return(employed_demographics_corrected)
  }

  years_in_data <- employed_by_sex_and_fingreen_industry_and_skill |> pull(time) |> unique()

  # Loop over years and sexes to do RAS-adjustment for each, and then combine the results
  employed_demographics_corrected <- mapply(
    FUN = correct_employed_demographics_for_year_and_sex,
    this_year = rep(years_in_data, each = 2),
    this_sex = rep(c("female", "male"), times = length(years_in_data)),
    MoreArgs = list(
      employed_by_sex_and_fingreen_industry_and_skill = employed_by_sex_and_fingreen_industry_and_skill
    ),
    SIMPLIFY = FALSE # just to have mapply output a list of dfs for bind rows
  ) |> 
    bind_rows()

  employed_demographics_corrected_long <- employed_demographics_corrected |> 
    tidyr::pivot_longer(cols = A1:ST, names_to = "fingreen_industry_code", values_to = "n_employed")

  # results ----------------------------------------------------------------

  res_employed_by_industry <- employed_by_fingreen_industry |> 
    filter(time == base_year) |> 
    select(fingreen_industry_code, n_employed) |> 
    tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = n_employed)
  
  res_g_employed_by_industry_calib <- employed_by_fingreen_industry |> 
    group_by(fingreen_industry_code) |> 
    arrange(time) |> 
    mutate(
      g_employed = (n_employed - lag(n_employed)) / lag(n_employed)
    ) |> 
    ungroup() |> 
    filter(between(time, base_year, current_year - 2L)) |>
    select(time, fingreen_industry_code, g_employed) |>
    tidyr::pivot_wider(names_from = time, values_from = g_employed)

  skill_share_by_industry_pre <- employed_demographics_corrected_long |> 
    group_by(time, fingreen_industry_code, skill_level) |> 
    summarise(n_employed = sum(n_employed), .groups = "drop_last") |> 
    mutate(skill_share = n_employed / sum(n_employed)) |> 
    ungroup()

  # This one for the model
  res_skill_share_by_industry <- skill_share_by_industry_pre |>
    filter(time == base_year) |> 
    select(-n_employed, -time) |> 
    tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = skill_share)
  
  # This one for calibration for the skill demand minimodel
  res_skill_share_by_industry_calib <- skill_share_by_industry_pre |> 
    select(-n_employed) |> 
    tidyr::pivot_wider(names_from = time, values_from = skill_share) |> 
    arrange(skill_level, fingreen_industry_code) |> 
    relocate(skill_level, fingreen_industry_code)

  male_share_by_industry_pre <- employed_demographics_corrected_long |> 
    group_by(time, fingreen_industry_code, skill_level) |> 
    summarise(male_share = sum(if_else(sex == "male", n_employed, 0)) / sum(n_employed), .groups = "drop")

  # This one for the model
  res_male_share_by_industry_and_skill <- male_share_by_industry_pre |> 
    filter(time == base_year) |> 
    select(-time) |> 
    tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = male_share)
  
  res_male_share_by_industry_and_skill_calib <- male_share_by_industry_pre |> 
    tidyr::pivot_wider(names_from = time, values_from = male_share) |> 
    arrange(skill_level, fingreen_industry_code) |> 
    relocate(skill_level, fingreen_industry_code)

  # This one for calibration

  result_path_from_filename <- function(filename){
    res <- paste0(results_dir, filename, ".ods")
  }

  readODS::write_ods(
    x = mget(c("res_employed_by_industry", "res_g_employed_by_industry_calib")),
    path = result_path_from_filename("n-employed-by-industry")
  )
  readODS::write_ods(
    x = mget(c("res_skill_share_by_industry", "res_skill_share_by_industry_calib")),
    path = result_path_from_filename("skill-share-by-industry")
  )
  readODS::write_ods(
    x = mget(c("res_male_share_by_industry_and_skill", "res_male_share_by_industry_and_skill_calib")),
    path = result_path_from_filename("male-share-by-industry-and-skill")
  )

  output_paths <- sapply(
    c("n-employed-by-industry", "skill-share-by-industry", "male-share-by-industry-and-skill"),
    FUN = result_path_from_filename
  )
  
  return(output_paths)
}