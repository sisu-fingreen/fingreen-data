create_inputs_technology_u <- function(raw_data_path, na_data_path, global_params) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  library(ggplot2)
  source("R/fingreen-r-utils.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-technology/")
  create_dir_if_not_exists(results_dir, "results")
  graphs_dir <- paste0(working_directory, "/graphs/inputs-technology/")
  create_dir_if_not_exists(graphs_dir, "graphs")

  # source data --------------------------------------------------------------

  io_annual <- readODS::read_ods(
    path = raw_data_path, sheet = "io_annual"
  ) |> 
    mutate(time = as.integer(time))

  gfcf_by_industry <- readODS::read_ods(
    path = na_data_path, sheet = "gfcf_by_industry"
  ) |> 
    mutate(time = as.integer(time))

  capital_stock_by_industry <- readODS::read_ods(
    path = na_data_path, sheet = "capital_stock_by_industry"
  ) |> 
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

  # data transformations ----------------------------------------------------

  # Transform the ind_ava
  io_transform_ava <- io_annual %>%
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

  gfcf_by_fingreen_industry <- gfcf_by_industry %>% 
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
  # gfcf_by_fingreen_industry %>% 
  #   group_by(geo, time) %>% 
  #   summarise(values = sum(values, na.rm = T)) %>% 
  #   left_join(
  #     filter(gfcf_eurostat, nace_r2 == "TOTAL"),
  #     by = c("geo", "time")
  #   ) %>% 
  #   mutate(differ = values.x - values.y) %>% 
  #   View("gfcf_comparison")

  capital_stock_by_fingreen_industry <- capital_stock_by_industry %>% 
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
  # capital_stock_by_industry %>%
  #   left_join(
  #     filter(eurostat_to_fingreen_industry_nama_map, relationship != "extra"),
  #     by = c("nace_r2" = "eurostat_nace_r2"),
  #     relationship = "many-to-many"
  #   ) %>%
  #   filter(time == 2000L) %>%
  #   View("q")
  # 
  # Again, accuracy even at worst is over 99 pct
  # capital_stock_by_fingreen_industry %>%
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

  if(length(bad_data_years > 0)){
    if(!identical(bad_data_years, 2012L)){
      stop("Unexpected years with technical coefficients > 1, check the data")
    }
  }

  # growth of technical coefficients --------------------------------------------------------

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

  # plot technical coefficients ---------------------------------------------

  p_technical_coefficients_by_industry <- technical_coefficients %>% 
    filter(time != 2012 & industry_code_type_ava == "nace-rev-2") %>% 
    mutate(
      description = fingreen_industry_code_to_abbreviation(fingreen_industry_code_ava),
      fingreen_industry_use = fingreen_industry_code_to_abbreviation(fingreen_industry_code_use)
      ) %>% 
    ggplot(aes(time, technical_coefficient, fill = fingreen_industry_code_ava, fontface = description)) +
    # geom_line(aes(color = fingreen_industry_code_ava), position = "stack") +
    # geom_area(position = "stack", stat = "identity", alpha = 0.5) +
    geom_col() +
    facet_wrap(~fingreen_industry_use, nrow = 4)

  pl_technical_coefficients_by_industry <- plotly::ggplotly(p_technical_coefficients_by_industry)

  save_plotly_plot(pl_technical_coefficients_by_industry, file = paste0(graphs_dir, "/technical-coefficients-by-industry.html"))

  # normalization -----------------------------------------------------------

  gfcf_previous_year <- gfcf_by_fingreen_industry %>% 
    mutate(time = time + 1L)

  # impute gfcf for fishing A3, since the data is missing for 2010-2016. The capital stock
  # seems to be decreasing, so let us impute the smallest observed gfcf value for A3.
  min_gfcf_a3 <- gfcf_previous_year %>%
    filter(fingreen_industry_code == "A3" & gfcf_meur != 0) %>% 
    pull(gfcf_meur) %>% 
    min()

  gfcf_previous_year_imputed <- gfcf_previous_year %>% 
    mutate(
      gfcf_meur = case_when(
        fingreen_industry_code == "A3" & gfcf_meur == 0 ~ min_gfcf_a3,
        T ~ gfcf_meur
      )
    )

  shares_of_new_capital <- capital_stock_by_fingreen_industry %>% 
    left_join(gfcf_previous_year_imputed, by = c("geo", "time", "fingreen_industry_code")) %>% 
    mutate(share_of_new_capital = gfcf_meur / net_capital_stock_meur) |> 
    # Exceptions here, something wrong with the data and need to be filtered out
    filter(
      !(geo == "FI" & time == 1998L & fingreen_industry_code == "K")
    )

  # Don't allow negative values or too tiny shares of new capital,
  # because they would have weird effects in the normalization done next
  n_too_little_new_capital <- shares_of_new_capital %>% 
    filter(share_of_new_capital < 0.001) %>% 
    count() %>% 
    pull(n)

  if(n_too_little_new_capital > 0) {
    stop(
      "There is an unexpected observation with very low share of new capital.",
      " Please check data, and if required, add exception to shares_of_new_capital in the code."
    )
  }

  technical_coefficient_growth_normalized <- technical_coefficient_growth %>% 
    inner_join(shares_of_new_capital, by = c("geo", "time", "fingreen_industry_code_use" = "fingreen_industry_code")) %>% 
    mutate(psi_a = g_technical_coefficient / share_of_new_capital)

  # distribution plots ------------------------------------------------------

  # technical_coefficient_growth_normalized %>% 
  #   ggplot(aes(psi_a)) +
  #   geom_density() +
  #   coord_cartesian(xlim = c(-100, 1000))

  # technical_coefficient_growth_normalized %>% 
  #   ggplot(aes(log(technical_coefficient))) +
  #   geom_density()
  #   # coord_cartesian(xlim = c(0, 0.05))

  # technical_coefficient_growth_normalized %>% 
  #   ggplot(aes(g_technical_coefficient)) +
  #   geom_density() +
  #   scale_x_log10(labels = scales::label_number()) +
  #   ggtitle("Välituotekäytön suhteelliset muutokset, jakauma log-asteikolla")

  # technical_coefficient_growth_normalized %>% 
  #   ggplot(aes(g_technical_coefficient)) +
  #   geom_density() +
  #   ggtitle("Välituotekäytön suhteelliset muutokset, jakauma lineaarisella asteikolla")

  # technical_coefficient_growth_normalized %>% 
  #   ggplot(aes(technical_coefficient, share_of_new_capital)) +
  #   geom_point(alpha = 0.7, color = "blue")

  # technical_coefficient_growth_normalized %>% 
  #   ggplot(aes(technical_coefficient, share_of_new_capital)) +
  #   geom_point(alpha = 0.7, color = "blue") +
  #   scale_x_log10()

  # technical_coefficient_growth_normalized %>% 
  #   ggplot(aes(g_technical_coefficient, share_of_new_capital)) +
  #   geom_point(alpha = 0.7, color = "blue") +
  #   scale_x_log10()

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

  # filter outliers ---------------------------------------------------------

  outlier_rows <- technical_coefficient_growth_normalized %>%
    select(psi_a) %>% 
    performance::check_outliers(
      method = "zscore_robust",
      threshold = 250 # It's fairly ad hoc, seems to reasonably filter out the worst
    ) %>% 
    attributes(.) %>% 
    getElement("data") %>% 
    filter(Outlier == 1L) %>% 
    pull(Row)

  outlier_ids <- technical_coefficient_growth_normalized %>% 
    slice(outlier_rows) %>% 
    select(geo, time, fingreen_industry_code_use) %>%
    distinct()

  technical_coefficient_growth_filtered <- technical_coefficient_growth_normalized %>% 
    anti_join(outlier_ids, by = c("geo", "time", "fingreen_industry_code_use"))

  # total intermediate input changes ----------------------------------------

  total_intermediate_input_changes <- technical_coefficient_growth_filtered %>%
    group_by(geo, time, fingreen_industry_code_use) %>%
    summarise(
      sum_technical_coefficients = sum(technical_coefficient),
      .groups = "drop"
    ) %>%
    group_by(geo, fingreen_industry_code_use) %>% 
    arrange(time) %>% 
    mutate(
      change_in_intermediate_inputs = (sum_technical_coefficients - lag(sum_technical_coefficients)) / lag(sum_technical_coefficients),
      # To check we only count changes between observations from consecutive years
      is_consecutive = (time - lag(time) == 1L)
    ) %>%
    ungroup() %>% 
    filter(
      !between(time, bad_data_years, bad_data_years + 1),
      time > min(time),
      is_consecutive
    ) |> 
    select(-is_consecutive)

  min_n_obs <- total_intermediate_input_changes %>% 
    count(fingreen_industry_code_use) %>% 
    summarise(min_n_obs = min(n)) %>% 
    pull(min_n_obs)

  if(min_n_obs < 3){
    stop("There are fewer than 3 observations of changes in some industry. Cannot calculate distribution. Please check data.")
  }

  # total intermediate input change plots

  p_change_in_intermediate_inputs <- total_intermediate_input_changes %>% 
    mutate(description = fingreen_industry_code_to_abbreviation(fingreen_industry_code_use)) %>% 
    ggplot(aes(time, change_in_intermediate_inputs, group = description)) +
    geom_line(aes(color = description))
  # geom_area(position = "stack", stat = "identity", alpha = 0.5)
  
  save_plotly_plot(
    plot = plotly::ggplotly(p = p_change_in_intermediate_inputs),
    file = paste0(graphs_dir, "change-in-intermediate-inputs.html")
  )

  ggsave(
    paste0(graphs_dir, "change-in-intermediate-inputs.jpeg"),
    plot = p_change_in_intermediate_inputs,
    width = 8.5,
    height = 6
  )

  p_technical_coefficients_in_time <- filter(total_intermediate_input_changes) %>% 
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
    facet_wrap(~fingreen_industry_use) +
    ggtitle("Top 5 industries in terms of change")

  save_plotly_plot(
    plot = plotly::ggplotly(p = p_technical_coefficients_in_time),
    file = paste0(graphs_dir, "technical-coefficients-in-time.html")
  )

  ggsave(
    paste0(graphs_dir, "technical-coefficients-in-time.jpeg"),
    plot = p_technical_coefficients_in_time,
    width = 8.5,
    height = 6
  )

  p_technical_coefficients_in_time_barplot <- filter(total_intermediate_input_changes) %>% 
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
    facet_wrap(~fingreen_industry_use) +
    ggtitle("Top 5 industries in terms of change")

  save_plotly_plot(
    plot = plotly::ggplotly(p = p_technical_coefficients_in_time_barplot),
    file = paste0(graphs_dir, "technical-coefficients-in-time-barplot.html")
  )

  ggsave(
    paste0(graphs_dir, "technical-coefficients-in-time-barplot.jpeg"),
    plot = p_technical_coefficients_in_time_barplot,
    width = 8.5,
    height = 6
  )

  # calculate distribution stats -------------------------------------------------

  calculate_u_stats <- function(df){
    res <- df %>%
      group_by(geo, fingreen_industry_code_ava, fingreen_industry_code_use) %>% 
      summarise(
        mean = mean(psi_a),
        median = median(psi_a),
        sample_sd = sd(psi_a),
        n = n(),
        .groups = "drop"
      ) %>% 
      tidyr::pivot_longer(cols = mean:n, names_to = "stat", values_to = "value") %>% 
      mutate(
        stat = factor(
          stat,
          levels = c("mean", "median", "sample_sd", "n"),
          labels = c("Mean", "Median", "Sample SD", "n")
        )
      )
    return(res)
  }

  u_norm_long <- calculate_u_stats(technical_coefficient_growth_filtered)

  # results -----------------------------------------------------------------

  prepare_results <- function(df){
    res <- df %>% 
      arrange(geo, stat, fingreen_industry_code_ava, fingreen_industry_code_use) %>%
      relocate(stat, .after = geo) %>% 
      tidyr::pivot_wider(names_from = "fingreen_industry_code_use", values_from = "value")
    return(res)
  }

  res_list <- list()

  res_list[["u_norm"]] <- prepare_results(u_norm_long)

  output_path <- paste0(results_dir, "u-norm.ods")

  readODS::write_ods(res_list, path = output_path)

  return(output_path)

}
