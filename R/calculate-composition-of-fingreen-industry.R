library(dplyr)

# If you have a fingreen industry that consists of
# several, more disaggregated industries in some other mapping,
# use this function to calculate the weight of the
# more disaggregated industries within a given fingreen industry
# based on total use. Country, year (time) and stk_flow to be used
# as a basis for the calculation can be selected.

calculate_composition_of_fingreen_industry <- function(
  fingreen_industry_code,
  fingreen_industry_to_eurostat_naio_map,
  eurostat_naio_to_desired_industry_map,
  desired_industry_mapping_name,
  geo = "FI",
  time = 2010L,
  stk_flow = "TOTAL"
) {

  io_industries <- tibble(fingreen_industry_code) |> 
    left_join(fingreen_industry_to_eurostat_naio_map, by = "fingreen_industry_code")

  io <- eurostat::get_eurostat(
    "naio_10_cp1750",
    time_format = "num",
    filters = list(
      geo = geo,
      time = time,
      unit = "MIO_EUR",
      stk_flow = stk_flow,
      ind_use = "TU",
      ind_ava = io_industries |> pull(eurostat_industry_code)
    )
  )

  res <- io |> 
    left_join(io_industries, by = c("ind_ava" = "eurostat_industry_code")) |> 
    left_join(eurostat_naio_to_desired_industry_map, by = c("ind_ava" = "nace_r2")) |> 
    group_by(fingreen_industry_code, .data[[desired_industry_mapping_name]]) |> 
    summarise(total_use = sum(values, na.rm = T), .groups = "drop_last") |> 
    mutate(share_of_fingreen_industry = total_use / sum(total_use)) |> 
    ungroup()

  return(res)
}

# debugonce(calculate_composition_of_fingreen_industry)

# eurostat_naio_to_desired_industry_map <- readxl::read_xlsx(
#   "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
#   sheet = "ava"
# ) |> 
#   filter(relationship != "extra") |> 
#   transmute(
#     nace_r2 = eurostat_industry_code,
#     nace_r2_lvl_1 = substr(eurostat_industry_code, 1, 1)
#   ) |> 
#   distinct()

# fingreen_industry_to_eurostat_naio_map <- readxl::read_xlsx(
#   "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
#   sheet = "ava"
# ) |> 
#   mutate(relationship != "extra")

# foo <- calculate_composition_of_fingreen_industry(
#   fingreen_industry_code = "MN",
#   fingreen_industry_to_eurostat_naio_map = fingreen_industry_to_eurostat_naio_map,
#   eurostat_naio_to_desired_industry_map = eurostat_naio_to_desired_industry_map,
#   desired_industry_mapping_name = "nace_r2_lvl_1"
# )

