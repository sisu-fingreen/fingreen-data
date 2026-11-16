pull_raw_data_inputs_economy_investments_matrix<- function(global_params) {
  
  source("R/fingreen-r-utils.R")
  library(dplyr)

  base_year <- global_params$base_year
  geo <- global_params$geo

  working_directory <- getwd()

  raw_data_dir <- paste0(working_directory, "/raw-data/inputs-economy/investments/")
  create_dir_if_not_exists(raw_data_dir, "raw data")
  
  current_year <- Sys.Date() |> format.Date(format = "%Y") |> as.integer()
  if(base_year + 4 > current_year - 2){
    stop("The base year is too recent, adjust base year or year selection within the function")
  }

  gfcf_detailed <- eurostat::get_eurostat(
    "nama_10_a64_p5",
    time_format = "num",
    filters = list(
      geo = geo,
      time = base_year:(base_year + 4),
      unit = "CP_MEUR",
      na_item = "P51G" # gfcf
    )
  )
  
  datasets_to_write <- c("gfcf_detailed")
  
  output_path <- paste0(raw_data_dir, "investment_matrix.ods")
  
  # write all in one go
  readODS::write_ods(x = mget(datasets_to_write), path = output_path)

  return(output_path)
}

create_inputs_economy_investments_matrix <- function(
  raw_data_path,
  io_raw_data_path,
  global_params
) {

  # libraries ---------------------------------------------------------------

  library(dplyr)
  source("R/fingreen-r-utils.R")
  source("R/transform-categorization.R")

  # directory setup ---------------------------------------------------------

  working_directory <- getwd()

  results_dir <- paste0(working_directory, "/results/inputs-economy/investments/")
  create_dir_if_not_exists(results_dir, "results")

  # source data --------------------------------------------------------------
  
  base_year <- global_params$base_year

  io_annual <- readODS::read_ods(
    path = io_raw_data_path,
    sheet = "io_annual"
  )
  
  gfcf_detailed <- readODS::read_ods(
    path = raw_data_path,
    sheet = "gfcf_detailed"
  )

  eurostat_nama_to_fingreen_industry_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-nama-industry-to-fingreen-industry-map.xlsx",
    sheet = "nama"
  ) |> 
    filter(relationship != "extra") |> 
    # We're not modelling U
    filter(fingreen_industry_code != "U")
   
  eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx(
    "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
    sheet = "ava"
  ) |> 
    filter(relationship != "extra") |> 
    # We're not modelling U
    filter(fingreen_industry_code != "U")
  
  # This only has mapped those assets and industries that have a relevant direct mapping
  asset10_to_fingreen_industry_map <- readODS::read_ods(
    "source-data/mappings/investment-asset10-to-fingreen-industry-map.ods",
    sheet = "asset10"
  ) |> 
    filter(!is.na(fingreen_industry_code))
  
  fingreen_industries <- eurostat_nama_to_fingreen_industry_map$fingreen_industry_code |> unique() |> sort()
  n_industries <- length(fingreen_industries)

  # Create a matrix for the R&D supply and demand shares, with 81% coming from each industry itself and the
  # rest split equally between others. This assumption aligns with what Eurogreen Pisa team used, except
  # we remove industries A3, D1, D2, D3, H49_51, O, P, Q, R, S95, ST from providing R & D for other sectors
  # than themselves
  r_d_supplying_industries <- c(
    "A1",
    "A2",
    "B",
    "C10_12",
    "C13_15",
    "C16_17",
    "C18",
    "C19",
    "C20_22",
    "C23",
    "C24_25",
    "C26_27 ",
    "C28",
    "C29",
    "C30",
    "C31_32",
    "C33",
    "E",
    "F",
    "G",
    "H52_53",
    "I",
    "J",
    "K",
    "L",
    "MN",
    "MN_72",
    "MN_73"
  )
  n_r_d_supplying_industries <- length(r_d_supplying_industries)
  r_d_shares <- expand.grid(
    fingreen_industry_code_supply = fingreen_industries,
    fingreen_industry_code_demand = fingreen_industries
  ) |> 
    mutate(
      r_d_supply_share = case_when(
        fingreen_industry_code_supply == fingreen_industry_code_demand ~ 0.81,
        fingreen_industry_code_supply %in% r_d_supplying_industries ~ (1 - 0.81)/(n_r_d_supplying_industries - 1),
        .default = 0
      )
    )

  # allocate gfcf demand and supply ----------------------------------------

  gfcf_demand <- gfcf_detailed |> 
    # ignore negative values
    mutate(values = pmax(values, 0))

  gfcf_demand_by_fingreen_industry <- gfcf_demand |> 
    inner_join(
      eurostat_nama_to_fingreen_industry_map,
      by = c("nace_r2"  = "eurostat_nace_r2"),
      relationship = "many-to-many"
    ) |> 
    group_by(time, fingreen_industry_code, asset10) |> 
    summarise(
      gfcf_meur = sum(values * coalesce(disaggregation_coefficient, 1), na.rm = TRUE),
      .groups = "drop"
    ) |> 
    rename(fingreen_industry_code_demand = fingreen_industry_code)

  gfcf_demand_by_fingreen_industry_5yr <- gfcf_demand_by_fingreen_industry |> 
    group_by(fingreen_industry_code_demand, asset10) |> 
    summarise(gfcf_meur = sum(gfcf_meur, na.rm = TRUE), .groups = "drop")

  gfcf_supply_direct_allocation <- gfcf_demand_by_fingreen_industry_5yr |>
    # Join the supplying industries based on the mapping
    inner_join(
      select(asset10_to_fingreen_industry_map, asset10, fingreen_industry_code_supply = fingreen_industry_code, share),
      by = "asset10",
      relationship = "many-to-many"
    ) |> 
    mutate(
      gfcf_meur = gfcf_meur * share
    ) 
  
  # R & D will include some residual other intellectual products as well
  # N1173 is directly allocated so not considered here
  r_d_supply_and_demand <- gfcf_demand_by_fingreen_industry_5yr |>
    filter(asset10 %in% c("N117G", "N1173G")) |> # Intellectual property products (gross), Computer software and databases (gross)
    tidyr::pivot_wider(names_from = asset10, values_from = gfcf_meur) |> 
    mutate(gfcf_meur = N117G - N1173G) |> 
    select(-N117G, -N1173G) |> 
    left_join(r_d_shares, by = "fingreen_industry_code_demand") |> 
    mutate(
      gfcf_meur = gfcf_meur * r_d_supply_share,
      asset10 = "N117G - N1173G"
    )
  
  gfcf_supply_and_demand <- gfcf_supply_direct_allocation |> 
    bind_rows(r_d_supply_and_demand) |> 
    group_by(fingreen_industry_code_supply, fingreen_industry_code_demand) |> 
    summarise(gfcf_meur = sum(gfcf_meur), .groups = "drop")

  gfcf_supply_by_fingreen_industry <- io_annual |> 
    filter(
      between(time, base_year, base_year + 4L),
      ind_use == "P51G", # Gross fixed capital formation
      stk_flow == "TOTAL"
    ) |> 
    rename(eurostat_industry_code = ind_ava) |> 
    transform_categorization(
      new_mapping = eurostat_to_fingreen_industry_ava_map,
      old_mapping_name = "eurostat_industry_code",
      new_mapping_name = "fingreen_industry_code",
      value_vars = "values",
      grouping_vars = c("geo", "time")
    )
  
  gfcf_supply_by_fingreen_industry_5yr <- gfcf_supply_by_fingreen_industry |> 
    group_by(fingreen_industry_code) |> 
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop")

  # RAS ------------------------------------------------------------------
  
  initial_matrix_5yr <- gfcf_supply_and_demand |> 
    tidyr::pivot_wider(names_from  = fingreen_industry_code_demand, values_from = gfcf_meur) |> 
    select(-fingreen_industry_code_supply) |> 
    as.matrix()

  # At this point, we have a correspondence matrix between gfcf supply and demand
  # but the row and col sums are the 5 yr ones, so we want to change it to a share matrix
  # so that we can create another correspondence matrix with the base year sums

  # This creates a matrix that can yield supply (rowsum-vector) from demand (colsum-vector).
  # Divide each column with corresponding element from the colsum-vector:
  initial_matrix_shares <- sweep(
    initial_matrix_5yr,
    2, # by columns
    colSums(initial_matrix_5yr),
    FUN = "/"
  )

  # Now we have
  all.equal(
    initial_matrix_shares %*% colSums(initial_matrix_5yr),
    as.matrix(rowSums(initial_matrix_5yr))
  )

  # Now create a correspondence matrix that matches the total investment demand for base year

  gfcf_demand_base_year <- gfcf_demand_by_fingreen_industry |> 
    filter(time == base_year, asset10 == "N11G") |> # N11G = Total fixed assets
    arrange(fingreen_industry_code_demand) |> 
    pull(gfcf_meur)

  initial_matrix_base_year_d <- sweep(
    initial_matrix_shares,
    2, # by columns
    gfcf_demand_base_year,
    FUN = "*"
  )

  # For RAS-adjustment, we would like the rowsums to match the gfcf supply
  # But, first we need to scale the gfcf supply to have the same total sum as gfcf demand

  gfcf_supply_base_year <- gfcf_supply_by_fingreen_industry |> 
    filter(time == base_year) |> 
    pull(values)
  
  rescale_coefficient <- sum(gfcf_demand_base_year) / sum(gfcf_supply_base_year)

  desired_row_sums <- gfcf_supply_base_year * rescale_coefficient
  desired_col_sums <- gfcf_demand_base_year
  
  # And now the RAS-adjustment itself

  convergence_criterion <- 1e-8 # if the Ipfp is not converging, adjust convergence criterion

  # Handle warnings as errors for the Ipfp fitting so we don't continue if something weird happens,
  # Eg. the function warns and shifts to probabilities in the output if row totals and col totals don't match
  ras_result <- tryCatch(
    expr = {
      mipfp::Ipfp(
        initial_matrix_base_year_d,
        list(1,2),
        list(desired_row_sums, desired_col_sums),
        iter = 1e4,
        tol = convergence_criterion,
        tol.margins = 1e-8
      )
    },
    warning = function(w) {
      stop("Converted from warning: ", conditionMessage(w))
    }
  )
  
  adjusted_matrix <- ras_result$x.hat

  
  # evaluate results -------------------------------------------------------
  
  diffs <- abs(adjusted_matrix - initial_matrix_base_year_d)
  rownames(diffs) <- fingreen_industries

  relative_diffs <- sweep(diffs, 1, rowSums(initial_matrix_base_year_d), FUN = "/")
  
  relative_diffs_long <- relative_diffs |>
    as.data.frame() |> 
    tibble::rownames_to_column(var = "fingreen_industry_code_supply") |> 
    tidyr::pivot_longer(cols = A1:ST, names_to = "fingreen_industry_code_demand") |> 
    mutate(
      supply_desc = fingreen_industry_code_to_abbreviation(fingreen_industry_code_supply),
      demand_desc = fingreen_industry_code_to_abbreviation(fingreen_industry_code_demand),
    )

  diffs_relative_to_starting_point <- tibble(
    fingreen_industry_code = fingreen_industries,
    change_magnitude = apply(diffs, 2, max) / rowSums(initial_matrix_base_year_d)
  )

  # export -------------------------------------------------------------------
  
  res_investment_matrix <- sweep(
    adjusted_matrix,
    MARGIN = 2,
    colSums(adjusted_matrix),
    FUN = "/"
  ) |> 
    as.data.frame() |> 
    mutate(fingreen_industry_code_supply = fingreen_industries, .before = 1)

  datasets_to_write <- c("res_investment_matrix")

  output_path <- paste0(results_dir, "investment_matrix.ods")

  readODS::write_ods(mget(datasets_to_write), path = output_path)

  return(output_path)

}