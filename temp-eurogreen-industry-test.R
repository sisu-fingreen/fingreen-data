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

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/inputoutput")

create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-economy/inputoutput")

create_dir_if_not_exists(results_dir, "results")

# source data -------------------------------------------------------------

# io_table <- readxl::read_xlsx(
#   "~/Downloads/Inter-industry IO table at current basic prices 2010-2022 Eurostat naio_10_cp1750__custom_16672803_spreadsheet.xlsx",
#   sheet = "Sheet 1",
#   skip = 9
# ) %>% 
#   fix_names() %>% 
#   rename(year = ind_use_labels_1, ind_ava = ind_use_labels_2) %>% 
#   slice_tail(n = -1L)

io_df_info <- search_eurostat("naio_10_cp1750", column = "code")

# When the function asks if you want to stop downloading, input n for no in the r console
io_df <- get_eurostat(
  io_df_info$code[1],
  time_format = "num",
  filters = list(
    geo = "FI",
    time = c(2010L:2022L),
    unit = "MIO_EUR"
  )
)

# io_df %>% select(ind_ava) %>% distinct() %>% writexl::write_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx")
# io_df %>% select(ind_use) %>% distinct() %>%
#   left_join(eurostat_to_fingreen_industry_map, by = c("ind_use" = "eurostat_industry_code")) %>% writexl::write_xlsx("source-data/mappings/eurostat-to-fingreen-industry-map2.xlsx")

eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "ava")
eurostat_to_fingreen_industry_use_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "use")

# Transform the ind_ava
io_transform_ava <- io_df %>%
  inner_join(
    filter(eurostat_to_fingreen_industry_ava_map),
    by = c("ind_ava" = "eurostat_industry_code"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    fingreen_industry_code_ava = coalesce(fingreen_industry_code, ind_ava),
    industry_code_type_ava = industry_code_type
  ) %>% 
  group_by(geo, time, industry_code_type_ava, fingreen_industry_code_ava, ind_use, stk_flow) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

# Transform the ind_use
io_transform_use <- io_transform_ava %>% 
  inner_join(
    filter(eurostat_to_fingreen_industry_use_map),
    by = c("ind_use" = "eurostat_industry_code"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    fingreen_industry_code_use = coalesce(fingreen_industry_code, ind_use),
    industry_code_type_use = industry_code_type
  ) %>% 
  group_by(geo, time, industry_code_type_ava, fingreen_industry_code_ava, industry_code_type_use, fingreen_industry_code_use, stk_flow) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

# A validation data frame
# check_df <- io_transform_use %>%
#   filter(industry_code_type_use == "nace-rev-2") %>% 
#   group_by(geo, time, fingreen_industry_code_ava, stk_flow) %>%
#   summarise(values = sum(values, na.rm = T), .groups = "drop") %>%
#   inner_join(eurostat_to_fingreen_industry_map, by = c("fingreen_industry_code_ava" = "fingreen_industry_code")) %>%
#   left_join(
#     filter(io_df, ind_use == "TOTAL"),
#     by = c("geo", "time", "stk_flow", "eurostat_industry_code" = "ind_ava")
#   ) %>%
#   mutate(difference = values.x - values.y)
# 
# c29_2018_imp_orig <- filter(io_df, ind_ava == "C29" & time == 2018L & stk_flow == "IMP")
# c29_2018_imp_ava <- filter(io_transform_ava, fingreen_industry_code_ava == "C29" & time == 2018L & stk_flow == "IMP")
# c29_2018_imp_use <- filter(io_transform_use, fingreen_industry_code_ava == "C29" & time == 2018L & stk_flow == "IMP")


# add total growth --------------------------------------------------------

io_growth <- filter(io_transform_use, stk_flow == "TOTAL") %>% 
  group_by(geo, industry_code_type_ava, fingreen_industry_code_ava, industry_code_type_use, fingreen_industry_code_use) %>% 
  arrange(time) %>% 
  mutate(relative_growth = (values - lag(values)) / lag(values)) %>%
  ungroup() %>% 
  arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, desc(industry_code_type_use), fingreen_industry_code_use) %>% 
  select(-industry_code_type_use, -values) %>%
  tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = relative_growth) %>% 
  filter(time > min(time))

# results -----------------------------------------------------------------

prepare_io_results <- function(df, stock_or_flow) {
  res <- df %>%
    filter(stk_flow == !! stock_or_flow) %>% 
    arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, desc(industry_code_type_use), fingreen_industry_code_use) %>% 
    select(-industry_code_type_use) %>% 
    tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = values)
}

res_list <- lapply(c("TOTAL", "DOM", "IMP"), FUN = prepare_io_results, df = io_transform_use)

names(res_list) <- c("total", "domestic", "imports")

res_list[["total_growth"]] <- io_growth

writexl::write_xlsx(res_list, path = "results/inputs-economy/inputoutput/io-annual-fi-2010-2022.xlsx")
