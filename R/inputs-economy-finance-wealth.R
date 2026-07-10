pull_raw_data_inputs_economy_finance <- function(global_params) {
  
  library(dplyr)
  
  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/finance/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  capitalist_wealth_share <- wid::download_wid(
    indicators = c("shweal"), # s = share, hweal = net personal wealth
    area = geo,
    years = base_year,
    perc = "p99p100", # top 1% = capitalists
    age = "999", # all ages
    pop = "j", # j = equal-split-adults, wealth equally between spouses
    include_extrapolations = FALSE
  )
  capitalist_wealth_share_schema <- structure(
    list(
      column_name = c('country', 'variable', 'percentile', 'year', 'value'),
      column_type = c('character', 'character', 'character', 'integer', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('country', 'variable', 'percentile', 'year', 'value')
  )
  validate_schema(capitalist_wealth_share, capitalist_wealth_share_schema, "capitalist_wealth_share")

  per_adult_wealth_last_year_price <- wid::download_wid(
    indicators = c("anweal"), # a = average, nweal = national wealth
    area = geo,
    year = base_year,
    perc = "p0p100",
    age = "992", # adults 20+, the best applicable category
    pop = "i", # individuals
    include_extrapolations = FALSE
  )
  per_adult_wealth_last_year_price_schema <- structure(
    list(
      column_name = c('country', 'variable', 'percentile', 'year', 'value'),
      column_type = c('character', 'character', 'character', 'integer', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('country', 'variable', 'percentile', 'year', 'value')
  )
  validate_schema(per_adult_wealth_last_year_price, per_adult_wealth_last_year_price_schema, "per_adult_wealth_last_year_price_schema")

  if(geo != "FI"){
    stop("Wealth by skill not implemented for other countries than Finland")
  }

  statfin_wealth_data_years <- c(2004, 2009, 2013, 2016, 2019, 2023)
  statfin_closest_wealth_year <- statfin_wealth_data_years[which.min(abs(base_year - statfin_wealth_data_years))]
  catn("Closest year in statfin wealth data is ", statfin_closest_wealth_year, ", using that")
  wealth_by_skill <- pxweb::pxweb_get_data(
    url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/vtutk/151w.px",
    query = list(
      vtutk_varlaji_6_20200101 = "nettoae_DN3001", # net personal wealth
      koulutusaste_16_20160101 = c("9", "3", "5-8"),
      timeperiod_y = as.character(statfin_closest_wealth_year),
      contentscode = c("vtutk_keskiarvo_n", "vtutk_kotitalouksia_perusjoukko")
    )
  )
  wealth_by_skill_schema <- structure(
    list(
      column_name = c('Asset type', 'Koulutusaste', 'Year', 'Mean of asset type, in nominal euros', 'Households in population'),
      column_type = c('character', 'character', 'character', 'numeric', 'numeric')
    ),
    class = 'data.frame',
    row.names = c('Asset type', 'Koulutusaste', 'Year', 'Mean of asset type, in nominal euros', 'Households in population')
  )
  validate_schema(wealth_by_skill, wealth_by_skill_schema, "wealth_by_skill")
  
  datasets_to_write <- c("capitalist_wealth_share", "per_adult_wealth_last_year_price", "wealth_by_skill")
  
  output_path <- paste0(raw_data_dir, "finance.ods")
  
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)

}

create_inputs_economy_finance_wealth <- function(raw_data_path, global_params) {

  # We need wealth distribution between workers of each skill and capitalists.
  # From statfin we get wealth by "skill" (education) and from World
  # Inequality Database (WID) we get wealth share of capitalists.
  # To get the final shares, we allocate the capitalist wealth from
  # all skill classes equally. Additionally, we get national wealth per adult
  # from WID.

  # libraries ---------------------------------------------------------------

  library(dplyr)

  source("R/fingreen-r-utils.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/finance/")
  create_dir_if_not_exists(results_dir, "results")

  # parameters ---------------------------------------------------------------

  base_year <- global_params$base_year
  geo <- global_params$geo

  # source data --------------------------------------------------------------

  capitalist_wealth_share <- readODS::read_ods(raw_data_path, sheet = "capitalist_wealth_share")

  per_adult_wealth_last_year_price <- readODS::read_ods(raw_data_path, sheet = "per_adult_wealth_last_year_price")

  wealth_by_skill <- readODS::read_ods(raw_data_path, sheet = "wealth_by_skill") |> 
    fix_names()

  # processing -------------------------------------------------------------

  wealth_share_by_skill <- wealth_by_skill |> 
    mutate(
      skill_level = factor(
        koulutusaste,
        levels = c("Basic education or unknown", "Upper secondary education", "Tertiary education"),
        labels = c("low", "mid", "high")
      ),
      total_wealth = mean_of_asset_type_in_nominal_euros * households_in_population,
      wealth_share = total_wealth / sum(total_wealth)
    ) |> 
    select(skill_level, wealth_share)

  wealth_by_skill_and_capitalists <- wealth_share_by_skill |> 
    mutate(wealth_share = wealth_share - capitalist_wealth_share$value / 3) |> 
    bind_rows(tibble(skill_level = "capitalist", wealth_share = capitalist_wealth_share$value)) |> 
    mutate(skill_level = factor(skill_level, levels = c("low", "mid", "high", "capitalist"))) |> 
    arrange(skill_level)

  # WID is "updated around July" and uses the previous years price, but don't offer a way to query the year.
  # So do some inference and output a message to the user to verify the year.
  current_month <- Sys.Date() |> strftime(format = "%m") |> as.integer()
  current_date_is_before_july <- current_month < 7L
  current_year <- Sys.Date() |> strftime(format = "%Y") |> as.integer()
  wid_year <- ifelse(current_date_is_before_july, current_year - 2L, current_year - 1L)
  catn("WID data is in last year currency, which is then converted to base year currency value")
  catn(wid_year, " is the inferred currency value year for WID data. Verify this is correct by visiting https://wid.world/data/")
  per_adult_wealth_base_year_price <- per_adult_wealth_last_year_price |> 
    mutate(value = convert_eur_value_between_years(value, from = wid_year, to = base_year))

  # write results ----------------------------------------------------------

  wealth_share_by_skill_and_capitalists <- wealth_by_skill_and_capitalists |> 
    tidyr::pivot_wider(names_from = skill_level, values_from  = wealth_share)

  national_wealth_per_adult <- per_adult_wealth_base_year_price |> 
    select(national_wealth_per_adult = value)
  
  datasets_to_write <- c("wealth_share_by_skill_and_capitalists", "national_wealth_per_adult")
  
  output_path <- paste0(results_dir, "wealth.ods")
  
  readODS::write_ods(mget(datasets_to_write), path = output_path)
  
  return(output_path)

}