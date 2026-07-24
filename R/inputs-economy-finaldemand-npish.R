create_inputs_economy_finaldemand_npish <- function(raw_data_path, global_params) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  source("R/fingreen-r-utils.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/finaldemand/")
  create_dir_if_not_exists(results_dir, "results")
  graphs_dir <- paste0(working_directory, "/graphs/inputs-economy/finaldemand/")
  create_dir_if_not_exists(graphs_dir, "graphs")

  # parameters -------------------------------------------------------------

  base_year <- global_params$base_year
  geo <- global_params$geo

  # source data --------------------------------------------------------------

  io_annual <- readODS::read_ods(
    path = raw_data_path, sheet = "io_annual"
  )

  ind_ava_to_fingreen_industry_map <- readxl::read_xlsx(
    path = "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
    sheet = "ava"
  )

  # process ------------------------------------------------------------------

  final_demand_by_industry <- io_annual |> 
    filter(
      ind_use %in% c(
        "P3_S13", # Final consumption expenditure by government
        "P3_S14", # Final consumption expenditure by households
        "P3_S15" # Final consumption expenditure by non-profit institutions serving households (NPISH)
      ),
      stk_flow == "DOM"
    ) |> 
    mutate(year = as.integer(time))

  final_demand_by_fingreen_industry <- final_demand_by_industry |>
    left_join(
      ind_ava_to_fingreen_industry_map,
      by = c("ind_ava" = "eurostat_industry_code"),
      relationship = "many-to-many"
    ) |> 
    filter(industry_code_type == "nace-rev-2" & relationship != "extra") |> 
    group_by(geo, year, fingreen_industry_code, ind_use) |> 
    summarise(
      values = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(values = convert_eur_value_between_years(x = values, from  = year, to = 2010L)) |> 
    tidyr::pivot_wider(names_from = ind_use, values_from = values) |> 
    rename(
      fd_gov = P3_S13,
      fd_hh = P3_S14,
      fd_npish = P3_S15
    )
  
  # Include only industries where npish demand is at least 2 % of the total
  relevant_fingreen_industries <- final_demand_by_fingreen_industry |> 
    mutate(is_npish_relevant = fd_npish > (fd_hh + fd_gov) * 0.02) |> 
    group_by(fingreen_industry_code) |> 
    summarise(is_npish_relevant = any(coalesce(is_npish_relevant, FALSE)))
  
  library(ggplot2)
  p1 <- final_demand_by_fingreen_industry |>
    inner_join(relevant_fingreen_industries, by = "fingreen_industry_code") |>
    filter(is_npish_relevant) |> 
    tidyr::pivot_longer(cols = c("fd_gov", "fd_hh", "fd_npish")) |> 
    ggplot(aes(year, value, group = name)) + 
    geom_line(aes(color = name)) +
    facet_wrap(~fingreen_industry_code, scales = "free_y") +
    ggtitle(
      label = "Final demand components for npish relevant industries",
      subtitle = paste0("Country: ", geo, ", data in ", base_year, " euros")
    )

  npish_graph_filename <- paste0(graphs_dir, "npish-final-demand.jpeg")
  ggsave(
    filename = npish_graph_filename,
    plot = p1,
    width = 8,
    height = 5
  )
  catn(
    "Wrote npish graph to", npish_graph_filename,
    "\nCompare to the graph that the npish demand relationships in the code make sense ",
    "if creating data for another country than Finland"
  )

  npish_hh_gov_fd_ratio <- final_demand_by_fingreen_industry |> 
    inner_join(relevant_fingreen_industries, by = "fingreen_industry_code") |> 
    group_by(fingreen_industry_code) |> 
    summarise(
      npish_per_hh_fd = mean(fd_npish / na_if(fd_hh, 0), na.rm = TRUE),
      npish_per_gov_fd = mean(fd_npish / na_if(fd_gov, 0), na.rm = TRUE),
      .groups = "drop"
    ) |> 
    # These should be set according to best judgement based on the data
    mutate(
      npish_per_hh_fd = case_when(
        fingreen_industry_code == "MN_72" ~ 0, # hh demand for science is negligible
        fingreen_industry_code == "P" ~ 0, # hh demand for education is very small
        fingreen_industry_code == "Q" ~ npish_per_hh_fd, # hh and npish demand for health are probably linked
        fingreen_industry_code == "R" ~ npish_per_hh_fd, # npish seems to link to this
        fingreen_industry_code == "ST" ~ npish_per_hh_fd, # npish seems to link to this
        .default = 0
      ),
      npish_per_gov_fd = case_when(
        fingreen_industry_code == "MN_72" ~ npish_per_gov_fd, # best available link
        fingreen_industry_code == "P" ~ npish_per_gov_fd, # # best available link
        fingreen_industry_code == "Q" ~ 0, # relate to hh fd instead
        fingreen_industry_code == "R" ~ 0, # relate to hh fd instead, this relationship seems inverse
        fingreen_industry_code == "ST" ~ 0, # gov demand for ST is negligible
        .default = 0
      )
    )

  # export -------------------------------------------------------------------
  
  datasets_to_write <- c("npish_hh_gov_fd_ratio")

  output_path <- paste0(results_dir, "npish.ods")

  readODS::write_ods(mget(datasets_to_write), path = output_path)

  return(output_path)

}