# libraries ---------------------------------------------------------------

library(dplyr)
library(eurostat)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("writexl"))
stopifnot(is_installed("config"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/labour/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
create_dir_if_not_exists(results_dir, "results")

# source data --------------------------------------------------------------

global_params <- config::get(file = "global-params.yml")

data_year <- global_params$base_year

# EMP_DC is employment domestic concept which means all employed in the
# industry, not just employee status
hours_worked_and_n_workers <- get_eurostat(
  "nama_10_a64_e",
  time_format = "num",
  filters = list(
    geo = global_params$geo,
    na_item = "EMP_DC",
    unit = c("THS_HW", "THS_PER"),
    time = data_year
  )
)

eurostat_to_fingreen_industry_nama_map <- readxl::read_xlsx("source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx", sheet = "nama")

# transform to fingreen industry structure

hours_worked_and_n_workers_transformed <- hours_worked_and_n_workers |> 
  inner_join(
    filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
    by = c("nace_r2" = "eurostat_nace_r2"),
    relationship = "many-to-many"
  ) |> 
  group_by(geo, time, fingreen_industry_code, unit) |>
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

results <- hours_worked_and_n_workers_transformed |>
  tidyr::pivot_wider(names_from = "unit", values_from = "values") |>
  mutate(
    hours_worked = THS_HW * 1000,
    employment = THS_PER * 1000,
    hours_per_year_per_employee = hours_worked / employment
  ) %>%
  select(-THS_HW, -THS_PER) %>% 
  tidyr::pivot_longer(
    cols = c("hours_worked", "employment", "hours_per_year_per_employee"),
    names_to = "variable",
    values_to = "values"
  ) %>%
  tidyr::pivot_wider(names_from = "fingreen_industry_code", values_from  = "values") |> 
  mutate(
    variable = factor(
      variable,
      levels = c("employment", "hours_worked", "hours_per_year_per_employee"),
      labels = c("employment", "hours/year per industry", "hours/year per employee")
    )
  ) |> 
  arrange(geo, time, variable)

# results ----------------------------------------------------------------

writexl::write-xlsx(results, path = paste0(results_dir, "hours-worked-and-employment.xlsx"))


