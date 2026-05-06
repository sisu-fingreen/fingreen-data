# libraries ---------------------------------------------------------------

library(dplyr)
library(eurostat)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace
stopifnot(is_installed("readxl"))
stopifnot(is_installed("writexl"))
stopifnot(is_installed("tidyr"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

results_dir <- paste0(working_directory, "/results/raw-data/")

create_dir_if_not_exists(results_dir, "results")

# source data -------------------------------------------------------------

output_per_industry_df_info <- search_eurostat("nama_10_a64", column = "code")

# When the function asks if you want to stop downloading, input n for no in the r console
output_per_industry_df <- get_eurostat(
  "nama_10_a64",
  time_format = "num",
  filters = list(
    geo = "FI",
    time = c(2010L:2023L),
    unit = c("CP_MEUR", "PYP_MEUR"), # current price meur and previous year price meur
    na_item = "P1" # P1 = output
  )
)

eurostat_to_fingreen_industry_nama_map <- readxl::read_xlsx("source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx", sheet = "nama")

# transform industry structure --------------------------------------------

# Transform the ind_ava
output_per_industry_transform <- output_per_industry_df %>%
  inner_join(
    filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
    by = c("nace_r2" = "eurostat_nace_r2"),
    relationship = "many-to-many"
  ) %>%
  group_by(unit, geo, time, fingreen_industry_code) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

output_per_industry_cp <- output_per_industry_transform %>% 
  filter(unit == "CP_MEUR") %>% 
  rename(cp_value = values) %>% 
  select(-unit)

output_per_industry_pyp <- output_per_industry_transform %>% 
  filter(unit == "PYP_MEUR") %>% 
  rename(pyp_value = values) %>% 
  select(-unit)

industry_deflators <- output_per_industry_cp %>% 
  left_join(
    output_per_industry_pyp,
    by = c("geo", "time", "fingreen_industry_code")
  ) %>% 
  mutate(deflator = cp_value / pyp_value)

# metadata ----------------------------------------------------------------

metadata <- tibble(
  variable = c("geo", "time", "fingreen_industry_code", "cp_value", "pyp_value", "deflator"),
  source = c(
    "Eurostat nama_10_a64",
    "Eurostat nama_10_a64",
    "Added in fingreen-data repo",
    "Modified from Eurostat nama_10_a64",
    "Modified from Eurostat nama_10_a64",
    "Calculated from eurostat data in fingreen-data repo"
  ),
  explanation = c(
    "Country code",
    "Year",
    "Fingreen industry code",
    "Industry output in current prices (cp)",
    "Industry output in previous year prices (pyp)",
    "Deflator = cp / pyp"
  )
)

sheet_info <- tibble(
  sheet = c("wide", "long"),
  description = c(
    "Industry deflators in wide format for each year, FI 2010-2023",
    "Industry deflators and output in cp and pyp, in long format, FI 2010-2023"
  )
)

# results -----------------------------------------------------------------

res_wide <- industry_deflators %>% 
  select(-cp_value, -pyp_value) %>% 
  tidyr::pivot_wider(names_from = time, values_from = deflator)

res_list <- list()

res_list[["wide"]] <- res_wide
res_list[["long"]] <- industry_deflators
res_list[["metadata"]] <- metadata
res_list[["sheet_info"]] <- sheet_info

writexl::write_xlsx(res_list, path = "results/raw-data/price-shocks-fi-2010-2023.xlsx")
