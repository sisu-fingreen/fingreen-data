##_______________________________________________________________
##
## Calculating beta elasticities for Finland 
## Change in expenditure share of consumption category per unit of change in relative price, 
## per consumption category and income quintile. The script creates interactive plots in
## subdirectory "graphs" in the working directory, and a result xlsx-file in subdirectory
## "results" in the working directory. These subdirectories are created automatically, if they
## don't already exist.
##  
##  By Topi-Matti Heikkola
##  Email: topi-matti@heikkola.fi
##  Updated:  2025-05-07  (y-m-d)
##_______________________________________________________________

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/labour")

ifelse(
  !dir.exists(file.path(graphs_dir)),
  dir.create(file.path(graphs_dir), recursive = TRUE),
  sprintf("Graphs directory '%s' exists", graphs_dir)
)

results_dir <- paste0(working_directory, "/results/inputs-economy/labour")

ifelse(
  !dir.exists(file.path(results_dir)),
  dir.create(file.path(results_dir), recursive = TRUE),
  sprintf("Results directory '%s' exists", results_dir)
)

# libraries ---------------------------------------------------------------

library(dplyr)
library(eurostat)
library(ggplot2)
library(pxweb)
library(fpp3)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace


# source data --------------------------------------------------------------

labour_productivity <- pxweb_get(
  url = "https://statfin.stat.fi:443/PxWeb/api/v1/en/StatFin_Passiivi/vtp/statfinpas_vtp_pxt_123u_2020.px",
  query = pxweb_query("queries/inputs-economy/labour/statfin_123u_labour_productivity.json")
) %>% 
  as.data.frame() %>%
  fix_names() %>% 
  mutate(year = as.integer(year))

statfin_industry_to_fingreen_industry_map <- read.csv(
  file = "source-data/inputs-economy/labour/statfin-industry-to-fingreen-industry-map.csv"
) %>% 
  fix_names()

dim_fingreen_industry <- read.csv(
  file = "source-data/inputs-economy/labour/dim-fingreen-industry.csv"
) %>% 
  fix_names()

labour_productivity_by_fingreen_industry <- labour_productivity %>% 
  inner_join(
    statfin_industry_to_fingreen_industry_map,
    by = c("industry" = "statfin_industry"),
    relationship = "many-to-many"  
  ) %>% 
  group_by(fingreen_industry_code, year) %>% 
  summarise(
    labour_productivity = mean(labour_productivity_gross_output_per_working_hour_index_2010_100),
    .groups = "drop"
  )

labour_productivity_by_fingreen_industry %>%
  ggplot(aes(year, labour_productivity, group = fingreen_industry_code)) +
  geom_line(aes(color = fingreen_industry_code))

labour_productivity_by_fingreen_industry %>% 
  group_by(fingreen_industry_code) %>% 
  slice_max(order_by = year, n = 20) %>% 
  mutate(labour_productivity = labour_productivity / first(labour_productivity, order_by = year) * 100) %>% 
  ungroup() %>% 
  left_join(dim_fingreen_industry, by = c("fingreen_industry_code" = "code")) %>% 
  ggplot(aes(year, labour_productivity, group = fingreen_industry_code, fontface = short_industry_name)) +
  geom_line(aes(color = fingreen_industry_code))

# TODO: try to make a tsibble, look at possible cyclicity in the data

