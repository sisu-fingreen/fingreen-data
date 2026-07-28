# This is probably not used or needed anymore.

# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
# library(pxweb)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace
# stopifnot(is_installed("readxl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

results_dir <- paste0(working_directory, "/results")


# source data -------------------------------------------------------------

euklems_dir <- paste0(working_directory, "/source-data/euklems")
euklems_fi_capital_accounts_filepath <- file.path(euklems_dir, "/EU_KLEMS 1995-2020 FI_capital accounts.xlsx")

K_i <- readxl::read_xlsx(euklems_fi_capital_accounts_filepath, sheet = "K_GFCF") 


# Use the linked chained volumes for inflation corrected data
# GFCF_price_index_2015 <- readxl::read_xlsx(euklems_fi_national_accounts_filepath, sheet = "Ip_GFCF")


euklems_industries_to_fingreen_industries_map <- readxl::read_xlsx(
  "source-data/mappings/euklems-industries-to-fingreen-industries-map.xlsx",
  sheet = "mapping"
)



# Mapping ---------------------------------------------

# While mangling all the data to long format, we also convert the inudstry categorization,
# using convert_data_from_euklem_fingreen_industry from fingreen-r-utils

K_long <- K_i %>% 
  tidyr::pivot_longer(
    cols = matches("\\d{4}"),  #selecting columns whose names match a 4-digit year pattern
    names_to = "year",
    values_to = "K_meur",
    names_transform = as.integer
  ) %>% 
  mutate(K_eur = 1e6 * K_meur) %>% 
  select(nace_r2_code, year, K_eur) %>%
  convert_data_from_euklems_to_fingreen_industry(
    mapping = euklems_industries_to_fingreen_industries_map,
    join_var = "nace_r2_code",
    id_vars = "year",
    vars_to_transform = "K_eur",
    aggregation_function = 
  )


K_i_2010 <- K_long %>%
  dplyr::filter(year == 2010)

writexl::write_xlsx(lp_pos_norm_klems, "results/inputs-technology/labour-productivity/lp_pos_norm_klems.xlsx", col_names = F)


