##_______________________________________________________________
##
## Calculating the changes in labour productivity
## 
##  This script calulates the distribution of positive and negative labour
##  productivity changes in Finland, normalized by the share of new capital.
##  Source data is from EUKLEMS.
##  
##  By Topi-Matti Heikkola
##  Email: topi-matti@heikkola.fi
##  Updated:  2025-05-12  (y-m-d)
##_______________________________________________________________


# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(pxweb)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace
stopifnot(is_installed("readxl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-technology/labour-productivity")

create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-technology/labour-productivity")

create_dir_if_not_exists(results_dir, "results")

# source data -------------------------------------------------------------

# check if there is a dir for EUKLEMS data, and create one if not

euklems_dir <- paste0(working_directory, "/source-data/euklems")

create_dir_if_not_exists(euklems_dir, "EUKLEMS")

# If they do not exist, we download FI national accounts and capital accounts EUKLEMS data

euklems_fi_national_accounts_filepath <- file.path(euklems_dir, "/FI_national_accounts.xlsx")

if(!file.exists(euklems_fi_national_accounts_filepath)){
  # The below url should be found from https://euklems-intanprod-llee.luiss.it/download/
  euklems_fi_national_accounts_url <- "https://www.dropbox.com/s/nqm8zmziu7o15f4/FI_national%20accounts.xlsx?dl=1"
  download.file(euklems_fi_national_accounts_url, destfile = euklems_fi_national_accounts_filepath)
  catn("Downloaded EUKLEMS FI_national_accounts.xlsx to", euklems_fi_national_accounts_filepath)
} else{
  catn("Source data file ", euklems_fi_national_accounts_filepath, " already exists")
}

hours_worked <- readxl::read_xlsx(euklems_fi_national_accounts_filepath, sheet = "H_EMP") 

# Use the linked chained volumes for inflation corrected data
gross_output <- readxl::read_xlsx(euklems_fi_national_accounts_filepath, sheet = "GO_Q")

euklems_fi_capital_accounts_filepath <- file.path(euklems_dir, "/FI_capital_accounts.xlsx")

if(!file.exists(euklems_fi_capital_accounts_filepath)){
  # The below url should be found from https://euklems-intanprod-llee.luiss.it/download/
  euklems_fi_capital_accounts_url <- "https://www.dropbox.com/s/3k1aslw0jdxmujz/FI_capital%20accounts.xlsx?dl=1"
  download.file(euklems_fi_capital_accounts_url, destfile = euklems_fi_capital_accounts_filepath)
  catn("Downloaded EUKLEMS FI_capital_accounts.xlsx to", euklems_fi_capital_accounts_filepath)
} else{
  catn("Source data file ", euklems_fi_capital_accounts_filepath, " already exists")
}

# Again chained volumes, 2020 eur
gfcf <- readxl::read_xlsx(euklems_fi_capital_accounts_filepath, sheet = "Iq_GFCF")

# Chained volumes, 2020 eur
capital_stock <- readxl::read_xlsx(euklems_fi_capital_accounts_filepath, "Kq_GFCF")

euklems_industries_to_fingreen_industries_map <- readxl::read_xlsx(
  "source-data/euklems/euklems-industries-to-fingreen-industries-map.xlsx",
  sheet = "mapping"
)


# labour productivity changes ---------------------------------------------

# While mangling all the data to long format, we also convert the inudstry categorization,
# using convert_data_from_euklem_fingreen_industry from fingreen-r-utils

gross_output_long <- gross_output %>% 
  tidyr::pivot_longer(
    cols = matches("\\d{4}"),
    names_to = "year",
    values_to = "gross_output_meur",
    names_transform = as.integer
  ) %>% 
  mutate(gross_output_eur = 1e6 * gross_output_meur) %>% 
  select(nace_r2_code, year, gross_output_eur) %>%
  convert_data_from_euklem_to_fingreen_industry(
    mapping = euklems_industries_to_fingreen_industries_map,
    id_vars = "year",
    vars_to_transform = "gross_output_eur"
  )

# gross_output_long %>% 
#   ggplot(aes(year, gross_output, group = nace_r2_code)) +
#   geom_line(aes(color = nace_r2_code))
# 
# plotly::ggplotly()

hours_worked_long <- hours_worked %>% 
  tidyr::pivot_longer(
    cols = matches("\\d{4}"),
    names_to = "year",
    values_to = "thousands_of_hours_worked",
    names_transform = as.integer
  ) %>% 
  mutate(hours_worked = 1e3 * thousands_of_hours_worked) %>% 
  select(nace_r2_code, year, hours_worked) %>% 
  convert_data_from_euklem_to_fingreen_industry(
    mapping = euklems_industries_to_fingreen_industries_map,
    id_vars = "year",
    vars_to_transform = "hours_worked"
  )

labour_productivity <- gross_output_long %>%
  left_join(hours_worked_long, by = c("year", "fingreen_industry_code")) %>% 
  group_by(fingreen_industry_code) %>% 
  arrange(year) %>% 
  mutate(
    labour_productivity_output_eur_per_hour = gross_output_eur / hours_worked,
    d_labour_productivity_output_eur_per_hour = labour_productivity_output_eur_per_hour -
      lag(labour_productivity_output_eur_per_hour)
  ) %>% 
  ungroup()

labour_productivity %>% 
  ggplot(aes(year, labour_productivity_output_eur_per_hour)) +
  geom_line() +
  facet_wrap(~fingreen_industry_code)

labour_productivity %>% 
  ggplot(aes(year, d_labour_productivity_output_eur_per_hour)) +
  geom_line() +
  facet_wrap(~fingreen_industry_code)

gfcf_long <- gfcf %>% 
  tidyr::pivot_longer(
    cols = matches("\\d{4}"),
    names_to = "year",
    values_to = "gfcf_meur",
    names_transform = as.integer
  ) %>% 
  select(nace_r2_code, year, gfcf_meur) %>%
  convert_data_from_euklem_to_fingreen_industry(
    mapping = euklems_industries_to_fingreen_industries_map,
    id_vars = "year",
    vars_to_transform = "gfcf_meur"
  )

capital_stock_long <- capital_stock %>% 
  tidyr::pivot_longer(
    cols = matches("\\d{4}"),
    names_to = "year",
    values_to = "net_capital_stock_meur",
    names_transform = as.integer
  ) %>% 
  select(nace_r2_code, year, net_capital_stock_meur) %>% 
  # Capital stock in T is missing, we impute 0 as the investment is also 0
  mutate(net_capital_stock_meur = if_else(nace_r2_code == "T", 0, net_capital_stock_meur)) %>% 
  convert_data_from_euklem_to_fingreen_industry(
    mapping = euklems_industries_to_fingreen_industries_map,
    id_vars = "year",
    vars_to_transform = "net_capital_stock_meur"
  )


# Relevant for productivity changes is the investment of the previous period
gfcf_previous_year <- gfcf_long %>% 
  mutate(year = year + 1L)

shares_of_new_capital <- capital_stock_long %>% 
  left_join(gfcf_previous_year, by = c("fingreen_industry_code", "year")) %>% 
  mutate(share_of_new_capital = gfcf_meur / net_capital_stock_meur) %>% 
  # Don't allow negative values or too tiny shares of new capital,
  # because they would have weird effects in the normalization done next
  filter(share_of_new_capital > 0.001)

pos_labour_productivity_changes <- labour_productivity %>% 
  filter(d_labour_productivity_output_eur_per_hour > 0) %>% 
  left_join(shares_of_new_capital, by = c("fingreen_industry_code", "year")) %>% 
  mutate(
    psi_lambda = d_labour_productivity_output_eur_per_hour / share_of_new_capital
  ) %>% 
  filter(year >= 1996L & !is.na(psi_lambda))

neg_labour_productivity_changes <- labour_productivity %>% 
  filter(d_labour_productivity_output_eur_per_hour < 0) %>% 
  left_join(shares_of_new_capital, by = c("fingreen_industry_code", "year")) %>% 
  mutate(
    psi_lambda = d_labour_productivity_output_eur_per_hour / share_of_new_capital
  ) %>% 
  filter(year >= 1996L & !is.na(psi_lambda))

pos_labour_productivity_changes %>%
  ggplot(aes(year, psi_lambda)) +
  geom_point() +
  geom_line() +
  facet_wrap(~fingreen_industry_code, scales = "free_y")

neg_labour_productivity_changes %>% 
  ggplot(aes(year, psi_lambda)) +
  geom_point() +
  geom_line() +
  facet_wrap(~fingreen_industry_code, scales = "free_y")

# Convert the values to 2010 euros
eur_variables <- c(
  "gross_output_eur",
  "labour_productivity_output_eur_per_hour",
  "d_labour_productivity_output_eur_per_hour",
  "net_capital_stock_meur",
  "gfcf_meur",
  "psi_lambda"
)

pos_labour_productivity_changes_2010eur <- pos_labour_productivity_changes %>% 
  mutate(across(all_of(eur_variables), .fns = ~ convert_eur_value_between_years(.x, 2020L, 2010L)))

neg_labour_productivity_changes_2010eur <- neg_labour_productivity_changes %>% 
  mutate(across(all_of(eur_variables), .fns = ~ convert_eur_value_between_years(.x, 2020L, 2010L)))
# results -----------------------------------------------------------------

# Extract the distribution statistics from the data and export to excel
lp_pos_norm_klems <- pos_labour_productivity_changes_2010eur %>% 
  group_by(fingreen_industry_code) %>% 
  summarise(
    mean = mean(psi_lambda),
    median = median(psi_lambda),
    sd = sd(psi_lambda)
  ) %>%
  data.table::transpose(keep.names = "measure")

lp_pos <- pos_labour_productivity_changes_2010eur %>% 
  group_by(fingreen_industry_code) %>% 
  summarise(
    mean = mean(d_labour_productivity_output_eur_per_hour),
    median = median(d_labour_productivity_output_eur_per_hour),
    sd = sd(d_labour_productivity_output_eur_per_hour)
  ) %>%
  data.table::transpose(keep.names = "measure")

