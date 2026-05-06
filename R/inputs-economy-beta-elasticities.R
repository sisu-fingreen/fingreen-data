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
##  Updated:  2025-05-05  (y-m-d)
##_______________________________________________________________

working_directory <- getwd()

graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/beta-elasticities")

ifelse(
  !dir.exists(file.path(graphs_dir)),
  dir.create(file.path(graphs_dir), recursive = TRUE),
  sprintf("Graphs directory '%s' exists", graphs_dir)
)

results_dir <- paste0(working_directory, "/results/inputs-economy/beta-elasticities")

ifelse(
  !dir.exists(file.path(results_dir)),
  dir.create(file.path(results_dir), recursive = TRUE),
  sprintf("Results directory '%s' exists", results_dir)
)

# libraries ---------------------------------------------------------------

library(dplyr)
library(readxl)
library(eurostat)
library(tidyr)
library(ggplot2)

source("fingreen-r-utils.R")

# needed but not loaded to the namespace
stopifnot(is_installed("data.table"))
stopifnot(is_installed("broom"))
stopifnot(is_installed("stringi"))
stopifnot(is_installed("plotly"))
stopifnot(is_installed("writexl"))
stopifnot(is_installed("htmlwidgets"))

# source data --------------------------------------------------------------

# We get acceptable accuracy in the consumption shares by using
# more disaggregated data only for the coicop categories that are split in the fingreen categorization
# and using the one-to-one corresponding data for the other categories. Some disaggregated data
# was missing for the one-to-one corresponding categories, but this way, it is not an issue. These are used
# later in fetching the right level hicp data, as well as choosing the right expenditure data.

simple_coicop_categories <- c("CP01", "CP02", "CP03", "CP05", "CP06", "CP08", "CP09", "CP10", "CP11")

split_coicop_categories <- c("CP041_043", "CP044", "CP045", "CP071_072", "CP073", "CP121", "CP122_127")

data_years <- c(2005L, 2010L, 2015L, 2020L)

expenditures_dataset_info <- search_eurostat("Structure of consumption expenditure by income quintile and COICOP consumption purpose", type = "dataset")

expenditures <- get_eurostat(
  id = expenditures_dataset_info$code[1],
  filters = list(time = data_years),
  time_format = "num",
  stringsAsFactors = FALSE
)

# hicp indicates the price development of consumption categories

hicp_dataset_info <- search_eurostat("HICP - annual data (average index and rate of change)", type = "dataset")

hicp <- get_eurostat(
  id = hicp_dataset_info$code[1],
  filters = list(time = data_years, unit = "INX_A_AVG"), # we want annual average index
  time_format = "num",
  stringsAsFactors = FALSE
)

hicp_high_lvl <- get_eurostat(
  id = hicp_dataset_info$code[1],
  filters = list(time = data_years, unit = "INX_A_AVG", coicop = simple_coicop_categories),
  time_format = "num",
  stringsAsFactors = FALSE
)

hicp_item_weights <- get_eurostat(
  id = "prc_hicp_inw",
  filters = list(time = data_years),
  time_format = "num",
  stringsAsFactors = FALSE
) %>% 
  mutate(cat_weight = values / 1000) %>% 
  select(-freq, -values)

results_italy <- read_xlsx("source-data/inputs-economy/beta-elasticities/beta-elasticities-italy.xlsx") %>% 
  rename(quantile = Group) %>% 
  pivot_longer(-quantile, values_to = "elasticity", names_to = "fingreen_coicop") %>% 
  mutate(
    quantile = case_match(
      quantile,
      "I" ~ "QUINTILE1",
      "II" ~ "QUINTILE2",
      "III" ~ "QUINTILE3",
      "IV" ~ "QUINTILE4",
      "V" ~ "QUINTILE5"
    ),
    countries = "Italy"
  )

# expenditure data processing ---------------------------------------------------------

expenditures_processed <- expenditures %>%
  filter(quantile != "UNK") %>% 
  mutate(
    time = as.integer(time),
    fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)
  ) %>% 
  # choose the right level of coicop aggregation
  filter(
    (fingreen_coicop %in% simple_coicop_categories & stringi::stri_length(coicop) == 4L) |
      (fingreen_coicop %in% split_coicop_categories & stringi::stri_length(coicop) == 5L)
  ) %>% 
  group_by(geo, time, quantile, fingreen_coicop) %>% 
  summarise(expenditure_share = sum(values, na.rm = T) / 1000, .groups = "drop")


# Find out geo+time combinations with too much missing data

bad_cases <- expenditures_processed %>%
  # Summarise on quantile level
  group_by(geo, time, quantile) %>%
  summarise(sum_of_shares = sum(expenditure_share), .groups = "drop") %>% 
  filter(sum_of_shares < 0.98)

expenditures_processed <- expenditures_processed %>%
  # But filter out on geo+time level
  anti_join(bad_cases, by = c("geo", "time"))

# Find the countries that match Finland closest by consumption shares

potential_references <- expenditures_processed %>% 
  group_by(geo, quantile, fingreen_coicop) %>% 
  summarise(avg_share = mean(expenditure_share), .groups = "drop") %>% 
  tidyr::pivot_wider(id_cols = geo, names_from = c("quantile", "fingreen_coicop"), values_from = avg_share)

potential_reference_countries <- potential_references %>% pull(geo)

country_distances <- potential_references %>% 
  select(-geo) %>% 
  as.matrix() %>% 
  dist() %>% 
  data.matrix()

distance_to_fi <- tibble(
  geo = potential_reference_countries,
  distance_to_fi = country_distances[ ,which(potential_reference_countries == "FI")]
)

reference_countries <- distance_to_fi %>% 
  filter(!grepl("\\d", x = geo, perl = T) & !(geo %in% c("FI", "EA"))) %>% 
  arrange(distance_to_fi) %>% 
  head(6) %>% 
  pull(geo)

analysis_countries <- c("FI", reference_countries)

# Calculate consumption basket weights for each quintile in each geo
consumption_basket_weights_by_income_group <- expenditures_processed %>%
  group_by(geo, quantile, fingreen_coicop) %>% 
  summarise(consumption_category_weight = mean(expenditure_share), .groups = "drop") %>% 
  group_by(geo, quantile) %>% 
  mutate(consumption_category_weight = consumption_category_weight / sum(consumption_category_weight)) %>% 
  ungroup()

consumption_basket_weights_by_income_group %>% 
  mutate(description = fingreen_coicop_to_description(fingreen_coicop)) %>% 
  filter(geo %in% reference_countries | geo == "FI") %>% 
  ggplot(aes(fingreen_coicop, consumption_category_weight, group = geo, fontface = description)) +
  geom_col(aes(fill = geo), position = "dodge") +
  facet_wrap(~quantile) +
  ggtitle("The reference countries look reasonable")

# Overview of the time series

expenditures_processed %>% 
  filter(geo %in% reference_countries | geo == "FI") %>% 
  ggplot(aes(time, expenditure_share, group = geo)) +
  geom_line(aes(color = geo)) +
  facet_wrap(~fingreen_coicop + quantile, scales = "free_y")

# Some changes in expenditure shares are uncredibly big. Filter out cases
# where an expenditure share at least doubles or halves, by removing any observations
# after such changes
expenditures_to_filter_out <- expenditures_processed %>%
  filter(geo %in% reference_countries | geo == "FI") %>% 
  group_by(geo, quantile, fingreen_coicop) %>% 
  arrange(time) %>% 
  mutate(
    d_rel_expenditure_share = abs(expenditure_share - lag(expenditure_share)) / pmin(expenditure_share, lag(expenditure_share)),
    is_big_jump = coalesce(d_rel_expenditure_share, 0) > 1
  ) %>% 
  ungroup() %>% 
  # Filter out observations after big jumps
  group_by(geo, quantile, fingreen_coicop) %>% 
  arrange(time) %>% 
  mutate(observation_after_big_jump = cumsum(is_big_jump)) %>% 
  ungroup() %>% 
  filter(observation_after_big_jump == 1L)

expenditures_filtered <- expenditures_processed %>% 
  anti_join(expenditures_to_filter_out, by = c("geo", "time", "fingreen_coicop")) %>% 
  filter(geo %in% analysis_countries)

# Check to see that it looks better
expenditures_filtered %>% 
  ggplot(aes(time, expenditure_share, group = geo)) +
  geom_line(aes(color = geo)) +
  facet_wrap(~fingreen_coicop + quantile, scales = "free_y")


# inflation data processing -----------------------------------------------

hicp <- hicp %>%
  # assure the right level of coicop aggregation, although length 5 seems to be the default
  filter(stringi::stri_length(coicop) == 5L) %>% 
  mutate(
    time = as.integer(time),
    fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)
  )

hicp_high_lvl <- hicp_high_lvl %>%
  mutate(
    time = as.integer(time),
    fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)
  ) %>% 
  rename(hicp_index = values) %>% 
  select(-freq, -unit)

# Check that hicp item weight data has the relevant data

relevant_keys <- expenditures_filtered %>% 
  select(time, geo, fingreen_coicop) %>% 
  distinct()

bad_hicp_keys <- hicp_item_weights %>% 
  mutate(fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)) %>% 
  inner_join(relevant_keys, by = c("time", "geo", "fingreen_coicop")) %>% 
  filter(
    stringi::stri_length(coicop) == 5L &
      is.na(cat_weight)
  ) %>% 
  select(time, geo, fingreen_coicop) %>% 
  distinct() %>% 
  # There are some holes but they are covered by the higher level data
  anti_join(hicp_high_lvl, by = c("time", "geo", "fingreen_coicop"))

# Filter out the bad keys from the expenditure data (does nothing if there are 0 bad keys)
expenditures_filtered <- expenditures_filtered %>% 
  anti_join(bad_hicp_keys)

hicp_item_weights_na_omitted <- hicp_item_weights %>% 
  filter(!is.na(cat_weight))

# Check that the weights sum to reasonable values
hicp_weight_sums <- hicp %>% 
  filter(geo %in% analysis_countries) %>% 
  inner_join(hicp_item_weights_na_omitted, by = c("time", "geo", "coicop")) %>% 
  group_by(time, geo) %>% 
  summarise(sum(cat_weight), .groups = "drop")

# Since they don't sum to one, we correct the weights
hicp_item_weights_corrected <- hicp_item_weights_na_omitted %>% 
  filter(stringi::stri_length(coicop) == 5L) %>% 
  group_by(time, geo) %>% 
  mutate(corrected_weight = cat_weight / sum(cat_weight, na.rm = T)) %>% 
  ungroup()

# Then calculate the weights to calculate inflation for fingreen coicop categories
hicp_item_weights_fingreen_coicop <- hicp_item_weights_corrected %>% 
  mutate(fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)) %>% 
  group_by(geo, time, fingreen_coicop) %>% 
  mutate(cat_weight = corrected_weight / sum(corrected_weight)) %>% 
  select(-corrected_weight)

# Finally, aggregate relevant inflation data to the fingreen coicop level. For the bad keys defined above
# this is inaccurate, but it does not matter since we filtered them out from the actual expenditure
# data
hicp_aggregated <- hicp %>% 
  filter(geo %in% analysis_countries) %>% 
  inner_join(hicp_item_weights_fingreen_coicop, by = c("time", "geo", "coicop", "fingreen_coicop")) %>% 
  group_by(time, geo, fingreen_coicop) %>% 
  summarise(
    hicp_index = sum(cat_weight * values, na.rm = T), # Omitting the missing values is safe, they have 0 weights in the data
    missed_weight = sum(if_else(is.na(values), cat_weight, 0)),
    .groups = "drop"
  )

stopifnot(all(hicp_aggregated$missed_weight == 0))

# Select to use either the data aggregated from the more granular level, or the ready higher level data,
# according to fingreen coicop categorization need
hicp_selected <- filter(hicp_aggregated, fingreen_coicop %in% split_coicop_categories) %>% 
  bind_rows(
    filter(
      hicp_high_lvl,
      fingreen_coicop %in% simple_coicop_categories &
        geo %in% analysis_countries
    )
  )

# Index HICP data to 100 for the first year (2005)

hicp_indexed <- hicp_selected %>% 
  group_by(geo, fingreen_coicop) %>% 
  arrange(time) %>% 
  mutate(hicp_index_from_beginning = hicp_index / first(hicp_index) * 100) %>% 
  ungroup() %>% 
  select(-missed_weight)

cumulative_inflation <- hicp_indexed %>% 
  mutate(cumulative_inflation = (hicp_index_from_beginning - 100) / 100)

inflation_faced_by_income_groups <- cumulative_inflation %>%
  inner_join(
    consumption_basket_weights_by_income_group,
    by = c("geo", "fingreen_coicop"),
    relationship = "many-to-many"
  ) %>% 
  group_by(geo, time, quantile) %>% 
  summarise(
    cumulative_inflation_faced_by_group = sum(consumption_category_weight * cumulative_inflation),
    .groups = "drop"
  )

# Compute differences in cumulative inflation
cumulative_inflation_differences <- cumulative_inflation %>%
  inner_join(inflation_faced_by_income_groups, by = c("geo", "time"), relationship = "many-to-many") %>%
  mutate(
    difference_in_cumulative_inflation = cumulative_inflation - cumulative_inflation_faced_by_group
  ) %>%
  select(geo, time, fingreen_coicop, quantile, difference_in_cumulative_inflation)

cumulative_inflation_differences %>% 
  filter(time %in% data_years) %>%
  mutate(description = fingreen_coicop_to_description(fingreen_coicop)) %>% 
  ggplot(aes(time, difference_in_cumulative_inflation, group = fingreen_coicop, fontface = description)) +
  geom_col(aes(fill = fingreen_coicop), position = "dodge") +
  facet_wrap(~geo) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Inflation varies by category: especially dwelling, communications, clothing, culture",
    subtitle = "2005 = 0"
  )

p_cum_inflation_by_coicop_category <- plotly::ggplotly()

save_plotly_plot(p_cum_inflation_by_coicop_category, paste0(graphs_dir, "/cum-inflation-by-coicop-category.html"))

# training dataset for the model -------------------------------------------------------------

training <- expenditures_filtered %>% 
  inner_join(cumulative_inflation_differences, by = c("geo", "time", "quantile", "fingreen_coicop"))

# exploration -------------------------------------------------------------

fit_elasticity_model <- function(fingreen_coicop, quantile, data, include_geo = T){
  formula_to_fit <- formula(
    ifelse(
      include_geo,
      "log(expenditure_share) ~ difference_in_cumulative_inflation + geo",
      "log(expenditure_share) ~ difference_in_cumulative_inflation"
    )
  )
  res <- lm(
    formula = formula_to_fit,
    data = filter(data, fingreen_coicop == !! fingreen_coicop & quantile == !! quantile)
  )
  return(res)
}

# example model
lm_cp01_i <- fit_elasticity_model("CP01", "QUINTILE1", training)

summary(lm_cp01_i)

lm_cp01_i_nogeo <- fit_elasticity_model("CP01", "QUINTILE1", training, include_geo = F)

summary(lm_cp01_i_nogeo)

lm_cp03_iii <- fit_elasticity_model("CP03", "QUINTILE3", training)

summary(lm_cp03_iii)

lm_cp03_iii_nogeo <- fit_elasticity_model("CP03", "QUINTILE3", training, include_geo = F)

summary(lm_cp03_iii_nogeo)

# including the country in the model seems to give better results

# one more example

lm_cp071_072_v <- fit_elasticity_model("CP071_072", "QUINTILE5", training)
summary(lm_cp071_072_v)

lm_cp071_072_v_nogeo <- fit_elasticity_model("CP071_072", "QUINTILE5", training, include_geo = F)
summary(lm_cp071_072_v_nogeo)

# fit elasticites -------------------------------------------------------------

elasticities_all <- training %>% 
  nest_by(fingreen_coicop) %>% 
  summarise(
    mod = list(lm(
      formula = "log(expenditure_share) ~ difference_in_cumulative_inflation + quantile + geo",
      data = data
    )),
    lm_results = list(broom::tidy(mod))
  ) %>% 
  select(fingreen_coicop, lm_results) %>% 
  unnest(lm_results) %>% 
  filter(term == "difference_in_cumulative_inflation") %>% 
  mutate(quantile = "over_all")

elasticities_by_quantile <- training %>% 
  nest_by(fingreen_coicop, quantile) %>% 
  summarise(
    mod = list(lm(
      formula = "log(expenditure_share) ~ difference_in_cumulative_inflation + geo",
      data = data
    )),
    lm_results = list(broom::tidy(mod))
  ) %>% 
  select(fingreen_coicop, quantile, lm_results) %>% 
  unnest(lm_results) %>% 
  filter(term == "difference_in_cumulative_inflation")

elasticities_all %>% 
  bind_rows(elasticities_by_quantile) %>% 
  mutate(
    p_value = signif(p.value, 2),
    description = fingreen_coicop_to_description(fingreen_coicop)
  ) %>% 
  mutate(quantile = factor(quantile, levels = rev(c("QUINTILE1", "QUINTILE2", "QUINTILE3", "QUINTILE4", "QUINTILE5", "over_all")))) %>% 
  ggplot(aes(fingreen_coicop, estimate, group = quantile, label = p_value, fontface = description)) +
  geom_col(aes(fill = quantile), position = "dodge") +
  # geom_text(position = position_dodge(width = 1)) +
  # scale_y_continuous(labels = scales::label_percent()) +
  # scale_colour_discrete(guide = "none") +
  scale_fill_discrete(
    name = "Income quantile",
    breaks = rev,
    direction = 1
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    title = "Beta elasticity estimates by income quantile and consumption category",
    subtitle = paste0("Eurostat data. Analysis countries: ", paste0(analysis_countries, collapse = ", "))
  )

p_elasticity_estimates <- plotly::ggplotly()

print(p_elasticity_estimates)

save_plotly_plot(p_elasticity_estimates, paste0(graphs_dir, "/consumption-elasticity-estimates.html"))

# Undefined p values for CP10 for quantiles 4 and 5, let's check

lm_cp10_v <- fit_elasticity_model("CP10", "QUINTILE5", training)
summary(lm_cp10_v)

results <- elasticities_by_quantile %>% 
  select(
    quantile,
    fingreen_coicop,
    elasticity = estimate
  ) %>% 
  mutate(countries = "analysis group")

results %>% 
  bind_rows(results_italy) %>% 
  mutate(description = fingreen_coicop_to_description(fingreen_coicop)) %>% 
  ggplot(aes(fingreen_coicop, elasticity, fontface = description)) +
  geom_col(aes(fill = countries), position = "dodge") +
  facet_wrap(~quantile) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Beta elasticity comparison to Pisa results (with Eurostat data)")
  
p_comparison_to_pisa <- plotly::ggplotly()

print(p_comparison_to_pisa)

save_plotly_plot(p_comparison_to_pisa, paste0(graphs_dir, "/consumption-elasticity-comparison-to-pisa.html"))

# save results ------------------------------------------------------------

res_to_write <- results %>%
  # adhere to naming conventions in the FINGREEN inputs economy excel
  mutate(
    fingreen_coicop = tolower(gsub("P", "_", x = fingreen_coicop, fixed = T)),
    quantile = gsub("QUINTILE(\\d)", "q\\1_WHOLE_COUNTRY", x = quantile, perl = T)
  ) %>%
  tidyr::pivot_wider(names_from = fingreen_coicop, values_from = elasticity) %>% 
  select(-countries)

writexl::write_xlsx(res_to_write, path = paste0(results_dir, "/beta-elasticities.xlsx"))

