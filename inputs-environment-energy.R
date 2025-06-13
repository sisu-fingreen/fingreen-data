# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(eurostat)


source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed("tidyr"))
stopifnot(is_installed("writexl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-environment/energy")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-environment/energy")
create_dir_if_not_exists(results_dir, "results")

# source data --------------------------------------------------------------

base_year <- 2011L

pefa <- get_eurostat(
  "env_ac_pefasu",
  time_format = "num",
  filters = list(
    geo = "FI",
    time = base_year
  )
)

energy_balance <- get_eurostat(
  "nrg_bal_c",
  time_format = "num",
  filters = list(
    geo = "FI",
    unit = "TJ",
    time = base_year
  )
)

nrg_bal_vocabulary <- read.csv(
  file = "https://dd.eionet.europa.eu/vocabulary/eurostat/nrg_bal/csv"
) %>%
  filter(Status == "valid") %>% 
  select(nrg_bal = Notation, nrg_bal_description = Label)

siec_vocabulary <- read.csv(
  file = "https://dd.eionet.europa.eu/vocabulary/eurostat/siec/csv"
) %>% 
  filter(Status == "valid") %>% 
  select(siec = Notation, siec_description = Label) # %>% 
  # mutate(
  #   siec_lvl = case_when(
  #     grepl("000$", siec, perl = T)
  #   )
  # )

prod_nrg_vocabulary <- read.csv(
  file = "https://dd.eionet.europa.eu/vocabulary/eurostat/prod_nrg/csv"
) %>% 
  filter(Status == "valid") %>% 
  select(prod_nrg = Notation, prod_nrg_description = Label)

foo <- energy_balance %>% 
  left_join(nrg_bal_vocabulary, by = "nrg_bal") %>% 
  left_join(siec_vocabulary, by = "siec") %>% 
  filter(time == 2022)

siec_lvl1_to_lvl2 <- function(x){
  res <- case_when(
    x %in% c('Anthracite','Coking coal','Other bituminous coal') ~ "hard_coal_SIEC",
    x %in% c('Sub-bituminous coal', 'Lignite', 'Peat') ~ "brown_coal_SIEC",
    # Basic oxygen steel furnace gas not in E balance, but in PEFA
    x %in% c('Coke oven gas','Gas works gas','Blast furnace gas') ~ "derived_gas_SIEC",
    x %in% c('Coke oven coke', 'Gas coke', 'Coal tar', 'Brown coal briquettes', 'Patent fuel', 'Peat products') ~ "sec_coal_prod_SIEC",
    x %in% c('Oil shale and oil sands', 'Crude oil', 'Natural gas liquids', 'Other hydrocarbons') ~ "crude_oil_SIEC",
    x %in% c('Natural gas') ~ "nat_gas_SIEC",
    x %in% c('Motor gasoline (excluding biofuel portion)', 'Aviation gasoline') ~ "motor_spirit_SIEC",
    x %in% c('Gasoline-type jet fuel', 'Kerosene-type jet fuel (excluding biofuel portion)', 'Other kerosene') ~ "kerosene_SIEC",
    x %in% c('Naphtha') ~ "naphta_SIEC",
    # This is also heating and other gasoil in PEFA, i.e. we have to merge these in E products
    x %in% c('Gas oil and diesel oil (excluding biofuel portion)') ~ "diesel_SIEC",
    x %in% c('Fuel oil') ~ "residual_SIEC",
    x %in% c('Refinery gas', 'Ethane', 'Liquefied petroleum gases') ~ "refinery_gas_SIEC",
    x %in% c('White spirit and special boiling point industrial spirits', 'Lubricants', 'Paraffin waxes', 'Petroleum coke', 'Bitumen', 'Other oil products n.e.c.', 'Refinery feedstocks', 'Additives and oxygenates (excluding biofuel portion)') ~ "other_petro_SIEC",
    # note the SIEC also includes animal waste, PEFA does not
    x %in% c('Primary solid biofuels', 'Charcoal') ~ "solid_bio_SIEC",
    x %in% c('Pure biogasoline','Blended biogasoline','Pure biodiesels', 'Blended biodiesels', 'Pure bio jet kerosene', 'Blended bio jet kerosene', 'Other liquid biofuels') ~ "liquid_bio_SIEC",
    # Biogases from thermal processes inclded in E balance but not in PEFA
    x %in% c('Biogases') ~ "gas_bio_SIEC",
    x %in% c('Electricity') ~ "elec_SIEC",
    x %in% c('Heat') ~ "heat_SIEC"
  )
  return(res)
}

siec_lvl2_to_pefa_category <- function(x){
  res <- case_match(
    x,
    "hard_coal_SIEC" ~ 'Hard coal',
    "brown_coal_SIEC" ~ 'Brown coal and peat',
    "derived_gas_SIEC" ~ 'Derived gases (= manufactured gases excl. biogas)',
    "sec_coal_prod_SIEC" ~ 'Secondary coal products (coke, coal tar, patent fuel, BKB and peat products)',
    "crude_oil_SIEC" ~ 'Crude oil, NGL, and other hydrocarbons (excl. bio)',
    "nat_gas_SIEC" ~ 'Natural gas (without bio)',
    "motor_spirit_SIEC" ~ 'Motor spirit (without bio)',
    "kerosene_SIEC" ~ 'Kerosenes and jet fuels (without bio)',
    "naphta_SIEC" ~ 'Naphtha',
    "diesel_SIEC" ~ 'Gas oil and diesel oil',
    "residual_SIEC" ~ 'Residual fuel oil',
    "refinery_gas_SIEC" ~ 'Refinery gas, ethane and LPG',
    "other_petro_SIEC" ~ 'Other petroleum products incl. additives/oxygenates and refinery feedstocks',
    #'Nuclear fuel':[],
    "solid_bio_SIEC" ~ 'Wood, wood waste and other solid biomass, charcoal',
    "liquid_bio_SIEC" ~ 'Liquid biofuels',
    "gas_bio_SIEC" ~ 'Biogas',
    "elec_SIEC" ~ 'Electrical energy',
    "heat_SIEC" ~ 'Heat'
  )
  return(res)
}

siec_to_pefa_category <- function(x){
  res <- siec_lvl1_to_lvl2(x) %>% siec_lvl2_to_pefa_category()
  return(res)
}

obs_counts_by_siec <- energy_balance %>% 
  filter(!is.na(values)) %>% 
  group_by(siec) %>% 
  summarise(n_energy_balance = n())

obs_counts_by_prod_nrg <- pefa %>% 
  filter(!is.na(values)) %>% 
  group_by(prod_nrg) %>% 
  summarise(n_pefa = n())

foo <- siec_vocabulary %>% 
  mutate(prod_nrg_description = siec_to_pefa_category(siec_description)) %>% 
  left_join(prod_nrg_vocabulary, by = "prod_nrg_description") %>% 
  full_join(obs_counts_by_siec) %>% 
  full_join(obs_counts_by_prod_nrg) %>% 
  select(-prod_nrg_description) %>% 
  left_join(prod_nrg_vocabulary)

# TODO: Check P17-P22 energy products, why are they not mapped in the Pisa python code