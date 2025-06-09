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
) %>% 
  mutate(time = as.integer(time))

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
) %>% 
  mutate(time = as.integer(time))

capital_stock_eurostat <- get_eurostat(
  "nama_10_nfa_st",
  time_format = "num",
  filters = list(
    geo = "FI",
    unit = "CLV20_MEUR",
    asset10 = "N11N", # N11N Total fixed assets (net)
    time = data_years
  )
) %>% 
  mutate(time = as.integer(time))

  
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
    gfcf_meur = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
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
    net_capital_stock_meur = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = T),
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


# table to help check which esa-2010 code is what
# io_transform_use %>% 
#   #group_by(geo, time, fingreen_industry_code_use) %>% 
#   #mutate(column_sum_of_nace_or_esa = sum(values)) %>% 
#   filter(time == 2012L) %>% 
#   arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, fingreen_industry_code_use) %>% 
#   tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = values) %>% 
#   clipr::write_clip()

# calculate technical coefficients ----------------------------------------

column_totals <- io_transform_use %>% 
  filter(fingreen_industry_code_ava == "TS_BP") %>% # TS_BP gives the io-matrix column total, with the value of labour as well
  select(geo, time, fingreen_industry_code_use, column_total = values)

technical_coefficients <- io_transform_use %>% 
  left_join(column_totals, by = c("geo", "time", "fingreen_industry_code_use")) %>% 
  mutate(technical_coefficient = values / column_total)

bad_data_years <- technical_coefficients %>%
  filter(industry_code_type_ava == "nace-rev-2" & technical_coefficient > 1) %>%
  pull(time) %>% 
  unique()

if(!identical(bad_data_years, 2012L)){
  stop("Unexpected years with technical coefficients > 1, check the data")
}

# growth in intermediate inputs --------------------------------------------------------

technical_coefficient_growth <- filter(technical_coefficients, industry_code_type_ava == "nace-rev-2") %>%
  group_by(geo, fingreen_industry_code_use, fingreen_industry_code_ava) %>%
  arrange(time) %>% 
  mutate(
    g_technical_coefficient = if_else(
      condition = technical_coefficient == 0 & lag(technical_coefficient) == 0,
      true = 0,
      false = (technical_coefficient - lag(technical_coefficient)) / lag(technical_coefficient)
    )
  ) %>%
  ungroup() %>% 
  filter(
    !between(time, bad_data_years, bad_data_years + 1) &
      time > min(time)
  ) %>% 
  # Filter out infinite relative growth
  filter(!is.infinite(g_technical_coefficient))

total_intermediate_input_changes <- filter(technical_coefficients, industry_code_type_ava == "nace-rev-2") %>%
  group_by(geo, time, fingreen_industry_code_use) %>%
  summarise(
    sum_technical_coefficients = sum(technical_coefficient),
    .groups = "drop"
  ) %>%
  group_by(geo, fingreen_industry_code_use) %>% 
  arrange(time) %>% 
  mutate(
    change_in_intermediate_inputs = (sum_technical_coefficients - lag(sum_technical_coefficients)) / lag(sum_technical_coefficients)
  ) %>%
  ungroup() %>% 
  filter(
    !between(time, bad_data_years, bad_data_years + 1) &
      time > min(time)
  )


# plots -------------------------------------------------------------------

technical_coefficients %>% 
  filter(time != 2012 & industry_code_type_ava == "nace-rev-2") %>% 
  mutate(
    description = fingreen_industry_code_to_abbreviation(fingreen_industry_code_ava),
    fingreen_industry_use = fingreen_industry_code_to_abbreviation(fingreen_industry_code_use)
    ) %>% 
  ggplot(aes(time, technical_coefficient, fill = fingreen_industry_code_ava, fontface = description)) +
  geom_line(aes(color = fingreen_industry_code_ava), position = "stack") +
  geom_area(position = "stack", stat = "identity", alpha = 0.5) +
  facet_wrap(~fingreen_industry_use, nrow = 4)

total_intermediate_input_changes %>% 
  mutate(description = fingreen_industry_code_to_abbreviation(fingreen_industry_code_use)) %>% 
  ggplot(aes(time, change_in_intermediate_inputs, group = description)) +
  geom_line(aes(color = description))
  # geom_area(position = "stack", stat = "identity", alpha = 0.5)

filter(total_intermediate_input_changes) %>% 
  group_by(fingreen_industry_code_use) %>% 
  summarise(max_abs_change = max(abs(change_in_intermediate_inputs))) %>%
  slice_max(n = 5, order_by = max_abs_change) %>% 
  left_join(technical_coefficients, by = "fingreen_industry_code_use") %>% 
  filter(time != 2012 & industry_code_type_ava == "nace-rev-2") %>% 
  mutate(
    description = fingreen_industry_code_to_abbreviation(fingreen_industry_code_ava),
    fingreen_industry_use = fingreen_industry_code_to_abbreviation(fingreen_industry_code_use)
  ) %>% 
  ggplot(aes(time, technical_coefficient, fill = fingreen_industry_code_ava, fontface = description)) +
  geom_line(aes(color = fingreen_industry_code_ava), position = "stack") +
  geom_area(position = "stack", stat = "identity", alpha = 0.5) +
  facet_wrap(~fingreen_industry_use)
  
filter(total_intermediate_input_changes) %>% 
  group_by(fingreen_industry_code_use) %>% 
  summarise(max_abs_change = max(abs(change_in_intermediate_inputs))) %>%
  slice_max(n = 5, order_by = max_abs_change) %>% 
  left_join(technical_coefficients, by = "fingreen_industry_code_use") %>% 
  filter(time != 2012 & industry_code_type_ava == "nace-rev-2") %>% 
  mutate(
    description = fingreen_industry_code_to_abbreviation(fingreen_industry_code_ava),
    fingreen_industry_use = fingreen_industry_code_to_abbreviation(fingreen_industry_code_use)
  ) %>% 
  ggplot(aes(time, technical_coefficient, fill = fingreen_industry_code_ava, fontface = description)) +
  geom_col(aes(color = fingreen_industry_code_ava), position = "stack") +
  facet_wrap(~fingreen_industry_use)

# normalization -----------------------------------------------------------

gfcf_previous_year <- gfcf_transform %>% 
  mutate(time = time + 1L)

shares_of_new_capital <- capital_stock_transform %>% 
  left_join(gfcf_previous_year, by = c("geo", "time", "fingreen_industry_code")) %>% 
  mutate(share_of_new_capital = gfcf_meur / net_capital_stock_meur) %>% 
  # Don't allow negative values or too tiny shares of new capital,
  # because they would have weird effects in the normalization done next
  filter(share_of_new_capital > 0.001) %>% 
  select(-gfcf_meur, -net_capital_stock_meur)

total_intermediate_input_changes_normalized <- total_intermediate_input_changes %>% 
  inner_join(shares_of_new_capital, by = c("geo", "time", "fingreen_industry_code_use" = "fingreen_industry_code")) %>% 
  mutate(change_in_intermediate_inputs_normalized = change_in_intermediate_inputs / share_of_new_capital)

technical_coefficient_growth_normalized <- technical_coefficient_growth %>% 
  inner_join(shares_of_new_capital, by = c("geo", "time", "fingreen_industry_code_use" = "fingreen_industry_code")) %>% 
  mutate(psi_a = g_technical_coefficient / share_of_new_capital)

total_intermediate_input_changes_normalized_neg <- total_intermediate_input_changes_normalized %>% 
  filter(psi_a < 0)

total_intermediate_input_changes_normalized_pos <- total_intermediate_input_changes_normalized %>% 
  filter(psi_a > 0)

min_neg_n_obs <- total_intermediate_input_changes_normalized_neg %>% 
  count(fingreen_industry_code_use) %>% 
  summarise(min_n_obs = min(n)) %>% 
  pull(min_n_obs)

min_pos_n_obs <- total_intermediate_input_changes_normalized_pos %>% 
  count(fingreen_industry_code_use) %>% 
  summarise(min_n_obs = min(n)) %>% 
  pull(min_n_obs)

if(min_neg_n_obs < 2 | min_pos_n_obs < 2){
  stop("There are fewer than 2 observations for positive or negative changes in some industry. Cannot calculate distribution. Please check data.")
}

# TODO: filter outliers using the performance package, as per Theriault et al

technical_coefficient_growth_filtered <- technical_coefficient_growth_normalized %>% 
  mutate(z_score = (psi_a - mean(psi_a)) / sd(psi_a)) %>% 
  filter(abs(z_score) > 8)


# plot normalized coefficients

technical_coefficient_growth_normalized %>% 
  filter(time != 2012 & industry_code_type_ava == "nace-rev-2") %>% 
  mutate(
    description = fingreen_industry_code_to_abbreviation(fingreen_industry_code_ava),
    fingreen_industry_use = fingreen_industry_code_to_abbreviation(fingreen_industry_code_use)
  ) %>% 
  ggplot(aes(time, psi_a, fill = fingreen_industry_code_ava, fontface = description)) +
  geom_line(aes(color = fingreen_industry_code_ava)) +
  # geom_area(position = "stack", stat = "identity", alpha = 0.5) +
  facet_wrap(~fingreen_industry_use, nrow = 4)

# TODO: filter outliers?

# calculate distributions -------------------------------------------------

u_neg_norm_long <- technical_coefficients_normalized %>%
  semi_join(total_intermediate_input_changes_normalized_neg, by = c("geo", "time", "fingreen_industry_code_use")) %>% 
  
  

# results -----------------------------------------------------------------

u_neg_norm <- intermediate_input_changes_normalized_neg %>%
  inner_join()
  arrange(geo, time, desc(industry_code_type_ava), fingreen_industry_code_ava, desc(industry_code_type_use), fingreen_industry_code_use) %>% 
  select(-industry_code_type_use, -values) %>%
  tidyr::pivot_wider(names_from = fingreen_industry_code_use, values_from = psi_a) %>% 
  filter(time > min(time))

res_list[["total_growth"]] <- io_growth

writexl::write_xlsx(res_list, path = "results/inputs-economy/inputoutput/io-annual-fi-2010-2022.xlsx")
