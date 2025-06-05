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

graphs_dir <- paste0(working_directory, "/graphs/inputs-technology/u")

create_dir_if_not_exists(graphs_dir, "graphs")

results_dir <- paste0(working_directory, "/results/inputs-technology/u")

create_dir_if_not_exists(results_dir, "results")

# source data -------------------------------------------------------------

data_years <- 2010L:2022L

# When the function asks if you want to stop downloading, input n for no in the r console
io_df <- get_eurostat(
  "naio_10_cp1750",
  time_format = "num",
  filters = list(
    geo = "FI",
    time = data_years,
    unit = "MIO_EUR",
    stk_flow = "TOTAL"
  )
)

eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
  sheet = "ava"
)
eurostat_to_fingreen_industry_use_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
  sheet = "use"
)
# The codes in the eurostat nama tables are a bit different from the ones in the io tables
eurostat_to_fingreen_industry_nama_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
  sheet = "nama"
)

gfcf_eurostat <- get_eurostat(
  "nama_10_a64_p5",
  time_format = "num",
  filters = list(
    geo = "FI",
    unit = "CLV20_MEUR",
    na_item = "P51G", # P51G 	Gross fixed capital formation, https://fgeerolf.com/data/eurostat/nama_10_a64_p5.html
    asset10 = "N11G", # N11G Total fixed assets (gross)
    time = data_years - 1L
  )
)

capital_stock_eurostat <- get_eurostat(
  "nama_10_nfa_st",
  time_format = "num",
  filters = list(
    geo = "FI",
    unit = "CLV20_MEUR",
    asset10 = "N11N", # N11N Total fixed assets (net)
    time = data_years
  )
)

  
# data transformations ----------------------------------------------------

# Transform the ind_ava
io_transform_ava <- io_df %>%
  inner_join(
    eurostat_to_fingreen_industry_ava_map,
    by = c("ind_ava" = "eurostat_industry_code"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    fingreen_industry_code_ava = coalesce(fingreen_industry_code, ind_ava),
    industry_code_type_ava = industry_code_type
  ) %>% 
  group_by(geo, time, industry_code_type_ava, fingreen_industry_code_ava, ind_use) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

# Transform the ind_use
io_transform_use <- io_transform_ava %>% 
  inner_join(
    filter(eurostat_to_fingreen_industry_use_map, industry_code_type == "nace-rev-2"),
    by = c("ind_use" = "eurostat_industry_code"),
    relationship = "many-to-many"
  ) %>% 
  mutate(
    fingreen_industry_code_use = coalesce(fingreen_industry_code, ind_use),
    industry_code_type_use = industry_code_type
  ) %>% 
  group_by(geo, time, industry_code_type_ava, fingreen_industry_code_ava, fingreen_industry_code_use) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

gfcf_transform <- gfcf_eurostat %>% 
  inner_join(
    filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
    by = c("nace_r2" = "eurostat_nace_r2"),
    relationship = "many-to-many"
  ) %>% 
  group_by(geo, time, fingreen_industry_code) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

# Validations
# gfcf_eurostat %>% 
#   left_join(
#     filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
#     by = c("nace_r2" = "eurostat_nace_r2"),
#     relationship = "many-to-many"
#   ) %>%
#   filter(time == 2000L) %>% 
#   View("q")
# 
# The accuracy is over 99 percent even at worst years, and 99.999 for the best
# gfcf_transform %>% 
#   group_by(geo, time) %>% 
#   summarise(values = sum(values, na.rm = T)) %>% 
#   left_join(
#     filter(gfcf_eurostat, nace_r2 == "TOTAL"),
#     by = c("geo", "time")
#   ) %>% 
#   mutate(differ = values.x - values.y) %>% 
#   View("gfcf_comparison")

capital_stock_transform <- capital_stock_eurostat %>% 
  inner_join(
    filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
    by = c("nace_r2" = "eurostat_nace_r2"),
    relationship = "many-to-many"
  ) %>% 
  group_by(geo, time, fingreen_industry_code) %>% 
  summarise(
    values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
    .groups = "drop"
  )

# Validations
# capital_stock_eurostat %>%
#   left_join(
#     filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
#     by = c("nace_r2" = "eurostat_nace_r2"),
#     relationship = "many-to-many"
#   ) %>%
#   filter(time == 2000L) %>%
#   View("q")
# 
# Again, accuracy even at worst is over 99 pct
# capital_stock_transform %>%
#   group_by(geo, time) %>%
#   summarise(values = sum(values, na.rm = T)) %>%
#   left_join(
#     filter(capital_stock_eurostat, nace_r2 == "TOTAL"),
#     by = c("geo", "time")
#   ) %>%
#   mutate(differ = values.x - values.y) %>%
#   View("cs_comparison")

# A validation data frame
# check_df <- io_transform_use %>%
#   group_by(geo, time, fingreen_industry_code_ava) %>%
#   summarise(values = sum(values, na.rm = T), .groups = "drop") %>%
#   inner_join(eurostat_to_fingreen_industry_ava_map, by = c("fingreen_industry_code_ava" = "fingreen_industry_code")) %>%
#   left_join(
#     filter(io_df, ind_use == "TOTAL"),
#     by = c("geo", "time", "eurostat_industry_code" = "ind_ava")
#   ) %>%
#   mutate(difference = values.x - values.y)
# 
# c29_2018_imp_orig <- filter(io_df, ind_ava == "C29" & time == 2018L & stk_flow == "IMP")
# c29_2018_imp_ava <- filter(io_transform_ava, fingreen_industry_code_ava == "C29" & time == 2018L & stk_flow == "IMP")
# c29_2018_imp_use <- filter(io_transform_use, fingreen_industry_code_ava == "C29" & time == 2018L & stk_flow == "IMP")


# calculate which code is what
io_transform_use %>% 
  #group_by(geo, time, fingreen_industry_code_use) %>% 
  #mutate(column_sum_of_nace_or_esa = sum(values)) %>% 
  filter(time == 2012L) %>% 
  arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, fingreen_industry_code_use) %>% 
  tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = values) %>% 
  clipr::write_clip()

# calculate technical coefficients ----------------------------------------

column_totals <- io_transform_use %>% 
  filter(fingreen_industry_code_ava == "TS_BP") %>% # TS_BP gives the io-matrix column total, with the value of labour as well
  select(geo, time, fingreen_industry_code_use, column_total = values)

technical_coefficients <- io_transform_use %>% 
  left_join(column_totals, by = c("geo", "time", "fingreen_industry_code_use")) %>% 
  mutate(technical_coefficient = values / column_total)

# calculate growth distribution --------------------------------------------------------

intermediate_input_changes <- filter(technical_coefficients, industry_code_type_ava == "nace-rev-2") %>%
  group_by(geo, time, fingreen_industry_code_use) %>%
  summarise(sum_technical_coefficients = sum(technical_coefficient))
  arrange(time) %>% 
  mutate(relative_growth = (values - lag(values)) / lag(values)) %>%
  ungroup()


foo %>% 
  arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, desc(industry_code_type_use), fingreen_industry_code_use) %>% 
  select(-industry_code_type_use, -values) %>%
  tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = relative_growth) %>% 
  filter(time > min(time))

# results -----------------------------------------------------------------


res_list[["total_growth"]] <- io_growth

writexl::write_xlsx(res_list, path = "results/inputs-economy/inputoutput/io-annual-fi-2010-2022.xlsx")
