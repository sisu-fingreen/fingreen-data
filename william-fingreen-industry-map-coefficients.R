library(dplyr)
library(eurostat)

io_df <- get_eurostat(
  "naio_10_cp1750",
  time_format = "num",
  filters = list(
    geo = "FI",
    time = 2015L,
    unit = "MIO_EUR"
  )
)

eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
  sheet = "ava"
)

william_to_fingreen_industry_map_disaggregation <- readODS::read_ods(
  path = "source-data/mappings/william-industry-to-fingreen-industry-map.ods",
  sheet = "disaggregate"
) |> 
  select(william_old, nace_rev_2, fingreen_industry_code)

william_to_fingreen_industry_map <- readODS::read_ods(
  path = "source-data/mappings/william-industry-to-fingreen-industry-map.ods",
  sheet = "william"
) |> 
  select(-disaggregation_coefficient)

cats_to_disaggregate_total_use <- io_df |> filter(
  stk_flow == "TOTAL",
  ind_use == "TU"
) |> 
  inner_join(william_to_fingreen_industry_map_disaggregation, by = c("ind_ava" = "nace_rev_2"))

disaggregation_coefficients <- cats_to_disaggregate_total_use |> 
  group_by(william_old, fingreen_industry_code) |> 
  summarise(sum_by_fingreen_industry = sum(values, na.rm = T), .groups = "drop_last") |> 
  mutate(sum_by_william_industry = sum(sum_by_fingreen_industry, na.rm = T)) |> 
  ungroup() |> 
  select(
    fingreen_industry_code,
    sum_by_fingreen_industry,
    william_old,
    sum_by_william_industry
  ) |> 
  mutate(disaggregation_coefficient = sum_by_fingreen_industry / sum_by_william_industry)

result <- william_to_fingreen_industry_map |> 
  left_join(
    y = select(disaggregation_coefficients, william_old, fingreen_industry_code, disaggregation_coefficient),
    by = c("william_old", "fingreen_industry_code")
  )

readODS::write_ods(
  result,
  path = "source-data/mappings/william-industry-to-fingreen-industry-map.ods",
  sheet = "william",
  update = TRUE
)
