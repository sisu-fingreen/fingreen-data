# libraries ---------------------------------------------------------------

library(dplyr)
library(eurostat)
library(pxweb)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("mipfp"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/consumption")

ifelse(
  !dir.exists(file.path(graphs_dir)),
  dir.create(file.path(graphs_dir), recursive = TRUE),
  sprintf("Graphs directory '%s' exists", graphs_dir)
)

results_dir <- paste0(working_directory, "/results/inputs-economy/consumption")

ifelse(
  !dir.exists(file.path(results_dir)),
  dir.create(file.path(results_dir), recursive = TRUE),
  sprintf("Results directory '%s' exists", results_dir)
)

# source data --------------------------------------------------------------

# We get acceptable accuracy in the consumption shares by using
# more disaggregated data only for the coicop categories that are split in the fingreen categorization
# and using the one-to-one corresponding data for the other categories. Some disaggregated data
# was missing for the one-to-one corresponding categories, but this way, it is not an issue.

simple_coicop_categories <- c("CP01", "CP02", "CP03", "CP05", "CP06", "CP08", "CP09", "CP10", "CP11")

split_coicop_categories <- c("CP041_043", "CP044", "CP045", "CP071_072", "CP073", "CP121", "CP122_127")

expenditure_share_data_years <- c(2010L, 2015L, 2020L)

base_year <- 2010L

closest_ref_year <- 2012L # for the expenditures by income quintile data

expenditures_dataset_info <- search_eurostat("Structure of consumption expenditure by income quintile and COICOP consumption purpose", type = "dataset")

expenditures_by_income_quintile_query <- pxweb_query(
  list(
    Vuosi = as.character(closest_ref_year),
    Kulutusmenot = "0",
    Tuloviidennes = 1:5 %>% as.character(),
    Tiedot = "kulu_kt_hk_1_1")
)

expenditures_by_income_quintile <- pxweb_get(
  url = "https://pxdata.stat.fi:443/PxWeb/api/v1/en/StatFin/ktutk/statfin_ktutk_pxt_14pg.px",
  query = expenditures_by_income_quintile_query
) %>% 
  as.data.frame() %>% 
  fix_names() %>% 
  mutate(
    quantile = factor(
      income_quintile,
      levels = c("I (Lowest-income 20 %)", "II", "III", "IV", "V (Highest-income 20 %)"),
      labels = paste0("QUINTILE", 1:5)
    )
  )

expenditures_by_coicop_category_query <- pxweb_query(
  list(
    Taloustoimi = "P31DCK", # Households consumption expenditure in Finland
    Kestävyysluokka = "SSS", # Total over all durability classes
    Kulutusluokka = "*",
    Tiedot = "cp",
    Sektori = "S14", # Households
    Vuosi = as.character(base_year)
  )
)

expenditures_by_coicop_category <- pxweb_get(
  url = "https://pxdata.stat.fi:443/PxWeb/api/v1/en/StatFin/vtp/statfin_vtp_pxt_127s.px",
  query = expenditures_by_coicop_category_query
) %>% 
  as.data.frame() %>% 
  fix_names()

expenditure_shares <- get_eurostat(
  id = expenditures_dataset_info$code[1],
  filters = list(time = expenditure_share_data_years, geo = "FI"),
  time_format = "num",
  stringsAsFactors = FALSE
)

# expenditure data processing ---------------------------------------------------------

expenditures_by_fingreen_coicop <- expenditures_by_coicop_category %>%
  mutate(
    coicop_code_only = gsub("(^\\d.*?\\s).*", "\\1", x = consumption_class, perl = T),
    category_level = stringi::stri_count(coicop_code_only, fixed = ".")) %>%
  filter(category_level == 1L) %>% 
  mutate(
    year = as.integer(year),
    fingreen_coicop = statfin_coicop_to_fingreen_coicop(coicop_code_only)
  ) %>% 
  group_by(year, fingreen_coicop) %>% 
  summarise(expenditure = sum(current_prices_millions_of_euro, na.rm = T), .groups = "drop")

expenditure_shares_processed <- expenditure_shares %>%
  filter(quantile != "UNK") %>% 
  mutate(
    time = as.integer(time),
    fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)
  ) %>% 
  # choose the right level of coicop aggregation
  filter(
    (fingreen_coicop %in% simple_coicop_categories & stringi::stri_length(coicop) == 4L) |
      (fingreen_coicop %in% split_coicop_categories & stringi::stri_length(coicop) == 5L)
  ) %>% 
  group_by(geo, time, quantile, fingreen_coicop) %>% 
  summarise(expenditure_share = sum(values, na.rm = T) / 1000, .groups = "drop") %>% 
  mutate(quantile = as.factor(quantile))


# consumption by quintile for base year --------------------------------------------

consumption_share_by_quintile_ref_year <- expenditures_by_income_quintile %>% 
  mutate(
    share = household_s_consumption_expenditure_at_current_prices_per_year_eur /
      sum(household_s_consumption_expenditure_at_current_prices_per_year_eur)
  )

total_hh_consumption_base_year <- expenditures_by_coicop_category %>%
  filter(consumption_class == "Total") %>% 
  pull(current_prices_millions_of_euro)

consumption_by_quintile_normalized_to_base_year <- consumption_share_by_quintile_ref_year %>% 
  mutate(expenditure_current_price_base_year = share * total_hh_consumption_base_year)


# coicop consumption shares per quintile ----------------------------------

expenditure_shares_wide <- expenditure_shares_processed %>% 
  group_by(geo, quantile, fingreen_coicop) %>% 
  summarise(
    mean_expenditure_share = mean(expenditure_share),
    .groups = "drop"
  ) %>%
  select(-geo) %>% 
  tidyr::pivot_wider(names_from = quantile, values_from = mean_expenditure_share) %>% 
  arrange(fingreen_coicop)

expenditure_share_matrix <- expenditure_shares_wide %>% 
  select(-fingreen_coicop) %>% 
  as.matrix()

expenditure_matrix <- expenditure_share_matrix %*%
  diag(consumption_by_quintile_normalized_to_base_year$expenditure_current_price_base_year)

shares_of_sum <- function(x){return(x / sum(x))}

# TODO: Do the whole RAS thing with absolute values instead of proportions, and see what happens

desired_column_sums <- consumption_by_quintile_normalized_to_base_year$expenditure_current_price_base_year

# desired_column_sums <- rep(1, 5)

desired_row_sums <- expenditures_by_fingreen_coicop$expenditure #%>% 
  # shares_of_sum()

# This shows the difference between the hbs survey data and the official consumption expenditure data
# expenditures_by_fingreen_coicop %>% 
#   mutate(
#     expected = rowSums(expenditure_matrix),
#     rel_diff_to_expected = (expenditure - expected) / expected,
#     description = fingreen_coicop_to_description(fingreen_coicop)
#   ) %>% 
#   View()

expenditure_matrix_ipfp <- mipfp::Ipfp(
  seed = expenditure_matrix,
  target.list = list(1,2), # over 1=rows and 2=columns
  target.data = list(desired_row_sums, desired_column_sums)
)

# divide columns with column sums
expenditure_shares_corrected <- expenditure_matrix_ipfp$x.hat %*% diag(1 / desired_column_sums)


# write results -----------------------------------------------------------

res_coicop_consumption_shares_per_quintile <- expenditure_shares_corrected %>%
  as.data.frame() %>%
  mutate(
    fingreen_coicop = expenditure_shares_wide$fingreen_coicop,
    .before = V1 # set as the first column
  ) %>%
  # Just an ugly way to name the columns the same as in expenditure_shares_wide
  rename_with(.fn = function(x){return(colnames(expenditure_shares_wide))})

res_consumption_by_quintile <- consumption_by_quintile_normalized_to_base_year %>% 
  select(quantile, expenditure_current_price_base_year) %>% 
  tidyr::pivot_wider(names_from = quantile, values_from = expenditure_current_price_base_year)

writexl::write_xlsx(
  res_coicop_consumption_shares_per_quintile,
  path = "results/inputs-economy/consumption/coicop-consumption-shares-per-quintile.xlsx"
)

writexl::write_xlsx(
  res_consumption_by_quintile,
  path = "results/inputs-economy/consumption/consumption-by-quintile.xlsx"
)
