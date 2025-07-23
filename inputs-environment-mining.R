# libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace

stopifnot(is_installed())

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/")
create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/")
create_dir_if_not_exists(results_dir, "results")

# source data --------------------------------------------------------------

