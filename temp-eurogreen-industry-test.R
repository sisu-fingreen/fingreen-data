# libraries ---------------------------------------------------------------

library(dplyr)
library(eurostat)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace
stopifnot(is_installed("readxl"))

# directory setup ---------------------------------------------------------

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-technology/labour-productivity")

create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-technology/labour-productivity")

create_dir_if_not_exists(results_dir, "results")

# source data -------------------------------------------------------------

io_table <- readxl::read_xlsx(
  "~/Downloads/Inter-industry IO table at current basic prices 2010-2022 Eurostat naio_10_cp1750__custom_16672803_spreadsheet.xlsx",
  sheet = "Sheet 1",
  skip = 9
) %>% 
  fix_names() %>% 
  rename(year = ind_use_labels_1, ind_ava = ind_use_labels_2) %>% 
  slice_tail(n = -1L)

io_df_info <- search_eurostat("naio_10_cp1750", column = "code")

io_df <- get_eurostat(
  io_df_info$code[1],
  time_format = "num",
  filters = list(
    geo = "FI",
    time = c(2010L:2022L)
  )
)

io_df %>% select(ind_ava) %>% distinct() %>% writexl::write_xlsx("source-data/mappings/eurostat-to-fingreen-industry-map.xlsx")
