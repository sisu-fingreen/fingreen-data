# We need wealth distribution between workers of each skill and capitalists.
# From statfin we get wealth by "skill" (education) and from World
# Inequality Database (WID) we get wealth share of capitalists.
# To get the final shares, we allocate the capitalist wealth from
# all skill classes equally. Additionally, we get national wealth per adult
# from WID.

# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/finance/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/finance/")
create_dir_if_not_exists(results_dir, "results")

# parameters ---------------------------------------------------------------
global_params <- config::get(file = "global-params.yml")

base_year <- global_params$base_year
geo <- global_params$geo

# source data --------------------------------------------------------------

capitalist_wealth_share <- wid::download_wid(
  indicators = c("shweal"), # s = share, hweal = net personal wealth
  area = geo,
  years = base_year,
  perc = "p99p100", # top 1% = capitalists
  age = "999", # all ages
  pop = "j", # j = equal-split-adults, wealth equally between spouses
  include_extrapolations = FALSE
)

per_adult_wealth_last_year_price <- wid::download_wid(
  indicators = c("anweal"), # a = average, nweal = national wealth
  area = geo,
  year = base_year,
  perc = "p0p100",
  age = "992", # adults 20+, the best applicable category
  pop = "i", # individuals
  include_extrapolations = FALSE
)

if(geo != "FI"){
  stop("Wealth by skill not implemented for other countries than Finland")
}

statfin_wealth_data_years <- c(2004, 2009, 2013, 2016, 2019, 2023)
statfin_closest_wealth_year <- statfin_wealth_data_years[which.min(abs(base_year - statfin_wealth_data_years))]
catn("Closest year in statfin wealth data is ", statfin_closest_wealth_year, ", using that")
wealth_by_skill <- pxweb::pxweb_get(
  url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/vtutk/statfin_vtutk_pxt_151w.px",
  query = list(
    Varallisuuslaji = "nettoae_DN3001", # net personal wealth
    Koulutusaste = c("9", "3", "5-8"),
    Vuosi = as.character(statfin_closest_wealth_year),
    Tiedot = c("vtutk_keskiarvo_n", "vtutk_kotitalouksia_perusjoukko")
  )
) |> 
  as.data.frame() |> 
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
catn(wid_year, " is the inferred year for WID data. Verify this is correct by visiting https://wid.world/data/")
per_adult_wealth_base_year_price <- per_adult_wealth_last_year_price |> 
  mutate(value = convert_eur_value_between_years(value, from = wid_year, to = base_year))

# write results ----------------------------------------------------------

res_wealth_by_skill_and_capitalists <- wealth_by_skill_and_capitalists |> 
  tidyr::pivot_wider(names_from = skill_level, values_from  = wealth_share)

writexl::write_xlsx(
  res_wealth_by_skill_and_capitalists,
  path = sprintf("%swealth-share-by-skill-and-capitalists-%s-%s.xlsx", results_dir, tolower(geo), base_year)
)

res_national_wealth_per_adult <- per_adult_wealth_base_year_price |> 
  select(national_wealth_per_adult = value)

writexl::write_xlsx(
  res_national_wealth_per_adult,
  path = sprintf("%snational-wealth-per-adult-%s-%s.xlsx", results_dir, tolower(geo), base_year)
)
