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
imputation_year <- 2020L # some data is missing for e.g. industry B, we use this to impute

# source data --------------------------------------------------------------

skill_levels <- c("low" = "ED0-2", "mid" = "ED3_4", "high" = "ED5-8")

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
      values = weighted.mean(values, w = coalesce(share_of_fingreen_industry, 1)),
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
        Sukupuoli = "SSS",
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
        ) |> factor(levels = c("low", "mid", "high"))
      ) |> 
      mutate(share_of_employed_persons = employed_1000_persons / sum(employed_1000_persons)) |> 
      arrange(skill_level)
    
    res <- employed_persons_skill_shares |> 
      select(skill_level, share_of_employed_persons)
    return(res)
  } else {
    # We can get the data from eurostat lfsa_egaed table, but at least for Finland
    # it is off by multiple percentage points
    stop("Getting other geo than FI is not yet implemented. TODO!")
  }
}

proper_skill_shares <- get_proper_skill_shares(geo, base_year)

get_n_employed_by_fingreen_industry <- function() {
  n_employed_file <- sprintf("results/inputs-economy/labour/hours-worked-and-employment-%s-%s.xlsx", tolower(geo), base_year)
  if(!file.exists(n_employed_file)) {
    cat("Labour data does not exist, running inpust-economy-labour.R\n")
    source("inputs-economy-labour.R", local = TRUE)
    cat("Done.\n")
  }
  n_employed_by_fingreen_industry <- readxl::read_xlsx(n_employed_file) |> 
    filter(variable == "employment") |> 
    tidyr::pivot_longer(cols = A1:ST, names_to = "fingreen_industry_code", values_to = "n_employed")
  return(n_employed_by_fingreen_industry)
}

n_employed_by_fingreen_industry <- get_n_employed_by_fingreen_industry()

n_employed_total <- sum(n_employed_by_fingreen_industry$n_employed)

n_employed_by_skill <- proper_skill_shares |> 
  mutate(n_employed_by_skill = n_employed_total * share_of_employed_persons)

n_employed_by_fingreen_industry_by_skill <- skill_share_by_fingreen_industry |> 
  left_join(n_employed_by_fingreen_industry, by = "fingreen_industry_code") |> 
  mutate(n_employed_by_skill = n_employed * values / 100) |> 
  select(-variable, -n_employed)

n_employed_matrix <- n_employed_by_fingreen_industry_by_skill |> 
  select(skill_level, fingreen_industry_code, n_employed_by_skill) |> 
  tidyr::pivot_wider(names_from = fingreen_industry_code, values_from = n_employed_by_skill) |> 
  select(-skill_level) |> 
  as.matrix()

desired_row_sums <- n_employed_by_skill$n_employed_by_skill
desired_column_sums <- n_employed_by_fingreen_industry$n_employed

n_employed_matrix_ipfp <- mipfp::Ipfp(
  seed = n_employed_matrix,
  target.list = list(1,2), # over 1=rows and 2=columns
  target.data = list(desired_row_sums, desired_column_sums)
)

skill_share_by_fingreen_industry_corrected <- n_employed_matrix_ipfp$x.hat %*% diag(1 / desired_column_sums) |> 
  as.data.frame()

names(skill_share_by_fingreen_industry_corrected) <- n_employed_by_fingreen_industry$fingreen_industry_code
skill_share_by_fingreen_industry_corrected$skill_level <- as.factor(c("low", "mid", "high"))

# write results ----------------------------------------------------------

res <- skill_share_by_fingreen_industry_corrected |> 
  relocate(skill_level)

writexl::write_xlsx(
  res,
  path = paste0(results_dir, "skill-share-by-industry-", tolower(geo), "-", base_year, ".xlsx")
)
