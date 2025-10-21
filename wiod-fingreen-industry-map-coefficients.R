library(dplyr)
library(eurostat)

io_df <- get_eurostat(
  "naio_10_cp1750",
  time_format = "num",
  filters = list(
    geo = "FI",
    time = 2011L,
    unit = "MIO_EUR"
  )
)

r_s_total_use <- io_df |> filter(
  stk_flow == "TOTAL",
  ind_ava %in% c("R90-92", "R93", "S94", "S95", "S96"),
  ind_use == "TU"
)

eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
  sheet = "ava"
)

wiod_to_fingreen_industry_map <- readODS::read_ods(
  path = "source-data/mappings/wiod-industry-to-fingreen-industry-map.ods",
  sheet = "wiod"
) |> 
  select(wiod_nace_r2, fingreen_industry_code, relationship)


r_s_total_use <- r_s_total_use |>
  left_join(
    eurostat_to_fingreen_industry_ava_map |> select(-relationship, -disaggregation_coefficient),
    by = c("ind_ava" = "eurostat_industry_code")
  ) |> 
  left_join(
    wiod_to_fingreen_industry_map |> filter(relationship == "disaggregation") |> select(-relationship),
    by = "fingreen_industry_code",
    relationship = "many-to-many"
  )

disaggregation_coefficients <- r_s_total_use |> 
  group_by(fingreen_industry_code) |> 
  mutate(sum_by_fingreen_industry = sum(values, na.rm = T)) |> 
  ungroup() |> 
  group_by(wiod_nace_r2) |> 
  mutate(sum_by_wiod_industry = sum(values, na.rm = T)) |> 
  ungroup() |> 
  select(
    fingreen_industry_code,
    sum_by_fingreen_industry,
    wiod_nace_r2,
    sum_by_wiod_industry
  ) |> 
  distinct() |> 
  mutate(disaggregation_coefficient = sum_by_fingreen_industry / sum_by_wiod_industry)

result <- wiod_to_fingreen_industry_map |> 
  left_join(
    disaggregation_coefficients |> select(wiod_nace_r2, fingreen_industry_code, disaggregation_coefficient),
    by = c("wiod_nace_r2", "fingreen_industry_code")
  )

readODS::write_ods(
  result,
  path = "source-data/mappings/wiod-industry-to-fingreen-industry-map.ods",
  sheet = "wiod"
)