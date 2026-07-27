source("../../R/transform-categorization.R")

library(dplyr)

test_df <- tibble(
  eurostat_nace_r2 = rep(c("A01", "C16", "C17", "D", "Q", "S94", "S96", "T"), 2),
  geo = "FI",
  var_to_group_by = c(rep("G1", 8), rep("G2",8)),
  values = rnorm(16),
  values2 = runif(16)
)

eurostat_nama_to_fingreen_industry_map <- readxl::read_xlsx(
  path = paste0(
    here::here(), # this will always result in project root
    "/source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx"
  ),
  sheet = "nama"
) |> 
  filter(relationship != "extra")

test_that("sum is preserved", {
 expect_equal(
  transform_categorization(
    df = test_df,
    new_mapping = eurostat_nama_to_fingreen_industry_map,
    old_mapping_name = "eurostat_nace_r2",
    new_mapping_name = "fingreen_industry_code",
    value_vars = c("values", "values2"),
    grouping_vars = "var_to_group_by",
    unmatched = c("x" = "error", "y" = "drop")
  ) |> 
    pull(values2) |> 
    sum(),
  sum(test_df$values2)
 ) 
})

test_that("sum is preserved w no explicit groups and one value var", {
 expect_equal(
  transform_categorization(
    df = test_df,
    new_mapping = eurostat_nama_to_fingreen_industry_map,
    old_mapping_name = "eurostat_nace_r2",
    new_mapping_name = "fingreen_industry_code",
    value_vars = "values",
    unmatched = c("x" = "error", "y" = "drop")
  ) |> 
    pull(values) |> 
    sum(),
  sum(test_df$values)
 ) 
})

test_that("duplicate rows results in error",{
  expect_error(
    transform_categorization(
      df = bind_rows(test_df, test_df),
      new_mapping = eurostat_nama_to_fingreen_industry_map,
      old_mapping_name = "eurostat_nace_r2",
      new_mapping_name = "fingreen_industry_code",
      value_vars = "values",
      unmatched = "drop"
    ),
    regexp = "df has duplicate rows not accounted for by the grouping variables"
  )
})

test_that("unmatched error behaviour works", {
  expect_error(
    transform_categorization(
      df = test_df,
      new_mapping = eurostat_nama_to_fingreen_industry_map,
      old_mapping_name = "eurostat_nace_r2",
      new_mapping_name = "fingreen_industry_code",
      value_vars = "values",
      unmatched = c("x" = "error", "y" = "error")
    )
  )
})