pull_raw_data_inputs_economy_labour_holiday_pay <- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/labour/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  structure_of_labour_costs_by_industry_all_years <- eurostat::get_eurostat(
    id = "lc_nstruc_r2",
    time_format = "num",
    filters = list(
      geo = "FI",
      unit = "PC",
      sizeclas = "GE10", # >= 10 employees, since there is no better data, total is missing
      lcstruct = c(
        # Documentation of these: Commission Regulation (EC) No 1737/2005 of 21 October 2005
        # https://eur-lex.europa.eu/LexUriServ/LexUriServ.do?uri=OJ:L:2005:279:0011:0031:EN:PDF
        # We only pick the categories that are relevant in the modelling context
        "D11111", # Direct remuneration, bonuses and allowances paid in each pay period
        "D11112", # Direct remuneration, bonuses and allowances not paid in each pay period
        "D1113" # Payments for days not worked // this does not include sick leave or family leave
        
      )
    )
  )
  
  available_years <- structure_of_labour_costs_by_industry_all_years$time |>
    unique() |>
    sort(decreasing = TRUE)
  # Since we sorted decreasingly, in case of draws we get the later year, which is desired
  closest_available_year <- available_years[which.min(abs(available_years - base_year))]
  
  structure_of_labour_costs_by_industry <- structure_of_labour_costs_by_industry_all_years |> 
    filter(time == closest_available_year)
  
  wages_and_salaries_by_industry <- eurostat::get_eurostat(
    "lc_ncostot_r2",
    time_format = "num",
    filters = list(
      geo = "FI",
      currency = "EUR",
      lcstruct = "D11", # Wages and salaries (total)
      sizeclas = "GE10", # >= 10 employees to match above data
      time = closest_available_year,
      freq = "A",
      unit = "TOTAL"
    )
  )

  datasets_to_write <- c("structure_of_labour_costs_by_industry", "wages_and_salaries_by_industry")
  
  output_path <- paste0(raw_data_dir, "holiday-pay.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}

create_inputs_economy_labour_holiday_pay <- function(raw_data_path, global_params) {
  # libraries ---------------------------------------------------------------

  library(dplyr)

  source("fingreen-r-utils.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/labour/")
  create_dir_if_not_exists(results_dir, "results")

  # parameters ---------------------------------------------------------------

  base_year <- global_params$base_year
  geo <- global_params$geo

  # source data -------------------------------------------------------------- 
  
  structure_of_labour_costs_by_industry <- readODS::read_ods(
    path = raw_data_path,
    sheet = "structure_of_labour_costs_by_industry"
  ) |> 
    select(-unit, -freq)

  structure_of_labour_costs_by_industry_wide <- structure_of_labour_costs_by_industry |> 
    tidyr::pivot_wider(names_from = lcstruct, values_from = values) |> 
    mutate(
      # These are not exact because some categories are omitted,
      # but good enough for our purpose
      regular_direct_pay_share = D11111 / (D11111 + D11112 + D1113),
      irregular_direct_pay_share = D11112 / (D11111 + D11112 + D1113),
      holiday_pay_share = D1113 / (D11111 + D11112 + D1113),
    )
  
  wages_and_salaries_by_industry <- readODS::read_ods(
    path = raw_data_path,
    sheet = "wages_and_salaries_by_industry"
  ) |> 
    select(-unit, -freq, -lcstruct) |> 
    rename(wages_and_salaries = values)
  
  eurostat_lc_to_fingreen_industry_map <- readODS::read_ods(
    "source-data/mappings/eurostat-lc-industry-to-fingreen-industry-map.ods",
    sheet = "lc"
  )

  structure_of_labour_costs_by_fingreen_industry <- structure_of_labour_costs_by_industry_wide |> 
    left_join(
      wages_and_salaries_by_industry,
      by = c("time", "geo", "nace_r2", "sizeclas")
    ) |> 
    mutate(
      regular_direct_pay = regular_direct_pay_share * wages_and_salaries,
      irregular_direct_pay = irregular_direct_pay_share * wages_and_salaries,
      holiday_pay = holiday_pay_share * wages_and_salaries
    ) |> 
    inner_join(
      eurostat_lc_to_fingreen_industry_map |> filter(relationship != "extra"),
      by = "nace_r2"
    ) |> 
    group_by(time, geo, fingreen_industry_code) |> 
    summarise(
      wages_and_salaries = sum(wages_and_salaries * coalesce(disaggregation_coefficient, 1)),
      regular_direct_pay = sum(regular_direct_pay * coalesce(disaggregation_coefficient, 1)),
      irregular_direct_pay = sum(irregular_direct_pay * coalesce(disaggregation_coefficient, 1)),
      holiday_pay = sum(holiday_pay * coalesce(disaggregation_coefficient, 1)),
      .groups = "drop"
    ) |> 
    mutate(
      irregular_direct_pay_ratio = irregular_direct_pay / regular_direct_pay,
      holiday_pay_ratio = holiday_pay / regular_direct_pay
    )

  structure_of_labour_costs_c <- structure_of_labour_costs_by_industry_wide |> 
    filter(nace_r2 == "C") |> 
    mutate(
      irregular_direct_pay_ratio = irregular_direct_pay_share / regular_direct_pay_share,
      holiday_pay_ratio = holiday_pay_share / regular_direct_pay_share
    )
  
  structure_of_labour_costs_avg <- structure_of_labour_costs_by_fingreen_industry |> 
    na.omit() |> 
    summarise(
      irregular_direct_pay_ratio = sum(irregular_direct_pay) / sum(regular_direct_pay),
      holiday_pay_ratio = sum(holiday_pay) / sum(regular_direct_pay)
    )

  structure_of_labour_costs_by_fingreen_industry_imputed <- structure_of_labour_costs_by_fingreen_industry |> 
    mutate(
      irregular_direct_pay_ratio = coalesce(
        irregular_direct_pay_ratio,
        if_else(
          substr(fingreen_industry_code, 1, 1) == "C",
          structure_of_labour_costs_c$irregular_direct_pay_ratio,
          structure_of_labour_costs_avg$irregular_direct_pay_ratio
        )
      ),
      holiday_pay_ratio = coalesce(
        holiday_pay_ratio,
        if_else(
          substr(fingreen_industry_code, 1, 1) == "C",
          structure_of_labour_costs_c$holiday_pay_ratio,
          structure_of_labour_costs_avg$holiday_pay_ratio
        )
      )
    )
  
  # write results ----------------------------------------------------------

  datasets_to_write <- c("structure_of_labour_costs_by_fingreen_industry_imputed")
  
  output_path <- paste0(results_dir, "holiday-pay.ods")
  
  readODS::write_ods(mget(datasets_to_write), path = output_path)
  
  return(output_path)

}
