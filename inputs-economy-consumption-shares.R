# libraries ---------------------------------------------------------------

library(dplyr)
library(eurostat)
library(pxweb)
library(ggplot2)

source("fingreen-r-utils.R")
source("R/get-number-of-households.R")

# needed but not loaded to the namespace

stopifnot(is_installed("mipfp"))

# parameters -------------------------------------------------------------

base_year <- 2010L

geo <- "FI"

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

expenditure_share_by_coicop_by_quintile <- get_eurostat(
  "hbs_str_t223",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    quant_inc = paste0("QU", 1:5),
    unit = "PM"
  )
) |> 
  mutate(year = as.integer(time))

mean_expenditure_by_quintile <- get_eurostat(
  "hbs_exp_t133",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    quant_inc = paste0("QU", 1:5),
    unit = "PPS_HH" # Purchasing power standard per household
  )
) |> 
  mutate(year = as.integer(time))

expenditure_by_coicop <- get_eurostat(
  "nama_10_co3_p3",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    unit = "CP_MEUR" # Current prices millions of euros
  )
) |> 
  mutate(
    year = as.integer(time)
  )

n_households <- get_number_of_households(geo, base_year)

# processing ---------------------------------------------------------

expenditure_share_by_fingreen_coicop_by_quintile <- expenditure_share_by_coicop_by_quintile |>
  mutate(fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)) |> 
  # choose the right level of coicop aggregation
  filter(
    (fingreen_coicop %in% simple_coicop_categories & stringi::stri_length(coicop) == 4L) |
      (fingreen_coicop %in% split_coicop_categories & stringi::stri_length(coicop) == 5L)
  ) %>% 
  mutate(quintile = factor(quant_inc, levels = paste0("QU", 1:5))) |> 
  group_by(geo, year, quintile, fingreen_coicop) %>% 
  summarise(expenditure_share = sum(values, na.rm = T) / 1000, .groups = "drop") |> 
  # Normalize the shares to sum to one (instead some were like 0.999)
  group_by(geo, year, quintile) |> 
  mutate(expenditure_share = expenditure_share / sum(expenditure_share)) |> 
  ungroup()

expenditure_by_fingreen_coicop <- expenditure_by_coicop |> 
  mutate(fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)) |> 
  # choose the right level of coicop aggregation
  filter(
    (fingreen_coicop %in% simple_coicop_categories & stringi::stri_length(coicop) == 4L) |
      (fingreen_coicop %in% split_coicop_categories & stringi::stri_length(coicop) == 5L)
  ) %>% 
  group_by(geo, year, fingreen_coicop) |> 
  summarise(expenditure = sum(values, na.rm = T) * 1e6, .groups = "drop")

total_pps_expenditure = mean_expenditure_by_quintile |> 
  left_join(n_households, by = c("geo", "year")) |> 
  mutate(expenditure = n_households / 5 * values) |> 
  group_by(geo, year) |> 
  summarise(total_pps_expenditure = sum(expenditure)) |> 
  select(geo, year, total_pps_expenditure)

total_consumption_expenditure <- expenditure_by_coicop |> 
  filter(coicop == "TOTAL") |> 
  mutate(total_eur_expenditure = values * 1e6) |> 
  select(geo, year, total_eur_expenditure)

pps_to_eur_conversion_factor <- total_pps_expenditure |> 
  left_join(total_consumption_expenditure, by = c("geo", "year")) |> 
  mutate(pps_to_eur_conversion_factor = total_eur_expenditure / total_pps_expenditure) |> 
  select(geo, year, pps_to_eur_conversion_factor)

mean_expenditure_by_quintile_eur <- mean_expenditure_by_quintile |> 
  left_join(pps_to_eur_conversion_factor) |> 
  mutate(mean_expenditure_eur = values * pps_to_eur_conversion_factor) |> 
  mutate(quintile = factor(quant_inc, levels = paste0("QU", 1:5))) |> 
  select(geo, year, quintile, mean_expenditure_eur)

total_expenditure_by_quintile_eur <- mean_expenditure_by_quintile_eur |> 
  left_join(n_households, by = c("geo", "year")) |> 
  mutate(total_expenditure_eur = n_households / 5 * mean_expenditure_eur) |> 
  select(geo, year, quintile, total_expenditure_eur)

expenditures_by_fingreen_coicop_by_quintile <- expenditure_share_by_fingreen_coicop_by_quintile |> 
  left_join(total_expenditure_by_quintile_eur, by = c("geo", "year", "quintile")) |> 
  mutate(expenditure_eur = expenditure_share * total_expenditure_eur)

# correction of expenditure shares ----------------------------------

# Since the expenditure shares by coicop by quintile are gathered by survey, they don't necessarily add
# up to sensible shares of total expenditure by coicop. Therefore we normalize the shares with the
# RAS algorithm

expenditure_wide <- expenditures_by_fingreen_coicop_by_quintile |> 
  select(quintile, fingreen_coicop, expenditure_eur) |> 
  tidyr::pivot_wider(names_from = "quintile", values_from = expenditure_eur) |> 
  arrange(fingreen_coicop)

expenditure_matrix <- expenditure_wide |> 
  select(-fingreen_coicop) |> 
  as.matrix()

desired_column_sums <- total_expenditure_by_quintile_eur$total_expenditure_eur

desired_row_sums <- expenditure_by_fingreen_coicop$expenditure

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
    fingreen_coicop = expenditure_wide$fingreen_coicop,
    .before = V1 # set as the first column
  ) %>%
  # Just an ugly way to name the columns the same as in expenditure_shares_wide
  rename_with(.fn = function(x){return(colnames(expenditure_wide))})

res_coicop_consumption_shares_per_quintile |> 
  tidyr::pivot_longer(cols = QU1:QU5, names_to = "quintile") |> 
  left_join(expenditure_share_by_fingreen_coicop_by_quintile, by = c("fingreen_coicop", "quintile")) |> 
  mutate(change = value - expenditure_share) |> 
  View()

res_consumption_by_quintile <- total_expenditure_by_quintile_eur |> 
  left_join(mean_expenditure_by_quintile_eur, by = c("geo", "year", "quintile")) |> 
  tidyr::pivot_longer(cols = c("mean_expenditure_eur", "total_expenditure_eur"), names_to = "measure") |> 
  tidyr::pivot_wider(names_from = "quintile", values_from = "value")

writexl::write_xlsx(
  res_coicop_consumption_shares_per_quintile,
  path = "results/inputs-economy/consumption/coicop-consumption-shares-per-quintile.xlsx"
)

writexl::write_xlsx(
  res_consumption_by_quintile,
  path = "results/inputs-economy/consumption/consumption-by-quintile.xlsx"
)
