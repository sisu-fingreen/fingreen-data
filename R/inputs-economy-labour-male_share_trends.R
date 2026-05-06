##__________________________________________________
##
## Code for calculating male share trends from microdata output 
##  
##  By Teemu Koskimäki
##  Email: teemu.koskimaki@live.fi
##  Started:  2026-02-02  (y-m-d)
##_________________________________________________
##


### PART 1 - SETUP ###

## Remove all existing objects from the environment
rm(list=ls()) 

## Install libraries if needed 
# install.packages("readxl") 

#Read libraries
library(dplyr)
library(readxl)
library(tidyr)
library(writexl)

# library(tidyverse)

#Define wdir
setwd("C:/GIT_ROOT/fingreen-data/")
working_directory <- getwd()

results_dir <- paste0(working_directory, "/results/labour/")

#load data
male_shares <- readxl::read_xlsx("source-data/inputs-economy/labour/Male-share-by-Skill-2010_2022-(KUN-VAL-YKS).xlsx") #, sheet = "nama"


#fit a separate linear regression of male_share on Year for each Code–Skill group and returns the estimated slope. 
# The slope measures the yearly change in male_share within each group.

male_share_trend <- male_shares %>%
  group_by(Code, Skill) %>%
  do({
    m <- lm(male_share ~ Year, data = .)
    out <- coef(m)
    data.frame(trend = out["Year"])
  }) %>%
  ungroup()



male_share_trend_wide <- male_shares %>%
  group_by(Code, Skill) %>%
  summarise(trend = coef(lm(male_share ~ Year))[["Year"]], .group = "drop") %>%
  pivot_wider(names_from = Code, values_from = trend)


#Export
writexl::write_xlsx(male_share_trend_wide, path = paste0(results_dir, "male_share_trend_wide.xlsx"))



