##__________________________________________________
##
## Code for reformatting Cazcarro et al. bridge 
## to match our industry and consumption categories, 
## and to use the RAS algorithm for matrix balancing
##  
##  By Teemu Koskimäki & Topi-Matti Heikkola
##  Email: teemu.koskimaki@live.fi
##  Started:  2026-01-09  (y-m-d)
##  Updated:  
##_________________________________________________
##

## Remove all existing objects from the environment
rm(list=ls()) 

# libraries ---------------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(mipfp)
library(readxl)

source("fingreen-r-utils.R")
source("R/bp-pp-convert.R")

# directory setup ---------------------------------------------------------
working_directory <- getwd()

results_dir <- paste0(working_directory, "/results/inputs-economy/consumption/")
create_dir_if_not_exists(results_dir, "results")

data_file <- "source-data/inputs-economy/consumption/cazcarro-et-al-2022-Annex_1_From_HBS_to_HFCE__COICOP__and_COICOP-CPA_pp_Contingency_tables.ods"

# parameters ---------------------------------------------------------------
global_params <- config::get(file = "global-params.yml")

base_year <- global_params$base_year
geo <- global_params$geo

# Load data -------------------------------------------------------------
orig_bridge <- readODS::read_ods(data_file, sheet = geo, range = "A1:AV65")
names(orig_bridge)[1] <- "CPA"

coicop_map <- readODS::read_ods("source-data/mappings/coicop-48-to-fingreen-coicop-map.ods", sheet = "COICOP_map")
cpa_map <- readODS::read_ods("source-data/mappings/cpa-cazcarro-to-fingreen-industry-map.ods", sheet = "NACE_map")

# transform bridge structure ----------------------------------------------

# reshape CP (COICOP) columns to long format
bridge_cp_transform <- orig_bridge %>%
  # reshape the data from wide to long format for the COICOP dimension
  tidyr::pivot_longer(cols = -CPA, names_to = "CP_48", values_to = "values") %>%
  # join COICOP 48 → 16 mapping table
  dplyr::inner_join(
    dplyr::filter(coicop_map),
    by = "CP_48",
    relationship = "many-to-many"
) %>%
  # aggregate values to CP_16 level by industry
  dplyr::group_by(CPA, CP_16) %>%
  dplyr::summarise(
    values = sum(values, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # reshape back to wide format with CP_16 columns
  tidyr::pivot_wider(names_from = CP_16, values_from = values)

# remap CPA industry rows to FINGREEN classification
bridge_transform <- bridge_cp_transform %>%
  # join CPA → FINGREEN industry mapping
  dplyr::inner_join(
    dplyr::filter(cpa_map),
    by = "CPA",
    relationship = "many-to-many"
  ) %>%
  # aggregate rows to NACE industry level using coefficients
  dplyr::group_by(fingreen_industry_code) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::where(is.numeric) & !dplyr::all_of("disaggregation_coefficient"),
      ~ sum(.x * dplyr::coalesce(disaggregation_coefficient, 1), na.rm = TRUE) * 1e6
    ),
    .groups = "drop")

#Export table 
writexl::write_xlsx(bridge_transform, paste0(results_dir,"coicop-nace-bridge-remapped-cazcarro.xlsx"))

# #Calculate shares by column 
# bridge_col_shares <- bridge_transform %>%
#   mutate(across(-1, ~ .x / sum(.x, na.rm = TRUE)))


#Calculate shares by row
bridge_row_shares <- bridge_transform %>%
  #For all except industry code column, divide each row's values by that row's total
  mutate(across(-fingreen_industry_code, ~ .x / rowSums(across(-fingreen_industry_code), na.rm = TRUE))) %>% 
  #Replace all NaN with 0
  mutate(across(-fingreen_industry_code, ~ replace(.x, is.nan(.x), 0)))

#Allocate all fd of industry MN_72 to the last coicop category
bridge_row_shares[bridge_row_shares$fingreen_industry_code == "MN_72", "CP122_127"] <- 1

# targets for RAS --------------------------------------------------------

expenditure_by_coicop <- eurostat::get_eurostat(
  "nama_10_co3_p3",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    unit = "CP_MEUR" # Current prices millions of euros
  )
) |> 
  mutate(
    year = as.integer(time)
  )

# These are used to choose convenient eurostat data
simple_coicop_categories <- c("CP01", "CP02", "CP03", "CP05", "CP06", "CP08", "CP09", "CP10", "CP11")

split_coicop_categories <- c("CP041_043", "CP044", "CP045", "CP071_072", "CP073", "CP121", "CP122_127")
  
expenditure_by_fingreen_coicop <- expenditure_by_coicop |> 
  mutate(fingreen_coicop = eurostat_coicop_to_fingreen_coicop(coicop)) |> 
  # choose the right level of coicop aggregation
  filter(
    (fingreen_coicop %in% simple_coicop_categories & stringi::stri_length(coicop) == 4L) |
      (fingreen_coicop %in% split_coicop_categories & stringi::stri_length(coicop) == 5L)
  ) %>% 
  group_by(geo, year, fingreen_coicop) |> 
  summarise(expenditure = sum(values, na.rm = T) * 1e6, .groups = "drop")

col_totals <- expenditure_by_fingreen_coicop$expenditure

hh_fd_bp <- eurostat::get_eurostat(
  "naio_10_cp1750",
  time_format = "num",
  filters = list(
    geo = geo,
    time = base_year,
    ind_use = "P3_S14",
    stk_flow = "TOTAL",
    unit = "MIO_EUR"
  )
) |> 
  mutate(
    year = as.integer(time)
  )

geo_dict_data <- readODS::read_ods(
  "source-data/inputs-economy/consumption/cazcarro_et_al_2022_Annex_2_From_CPA_pp_to_bp_converter_tool.ods",
  sheet = "%m",
  skip = 11L
)
geo_dict <- geo_dict_data[1, ] |> as.character()
names(geo_dict) <- colnames(geo_dict_data)
geo_long <- geo_dict[geo]

ttm_data <- readODS::read_ods(
  "source-data/inputs-economy/consumption/cazcarro_et_al_2022_Annex_2_From_CPA_pp_to_bp_converter_tool.ods",
  sheet = "%m",
  skip = 12L
) |> 
  filter(PROD_NA != "TOTAL")

ttm_margins <- ttm_data |> pull(geo_long)
ttm_margins_positive <- if_else(ttm_margins < 0, 0, ttm_margins)
ttm_shares <- if_else(ttm_margins < 0, -ttm_margins, 0)

tls_margins <- readODS::read_ods(
  "source-data/inputs-economy/consumption/cazcarro_et_al_2022_Annex_2_From_CPA_pp_to_bp_converter_tool.ods",
  sheet = "%t",
  skip = 12L
) |> 
  filter(PROD_NA != "TOTAL") |> 
  pull(geo_long)

cazcarro_etal_cpas <- ttm_data |> pull(PROD_NA) |> stringi::stri_trim_both()
cazcarro_etal_cpas_as_industries <- gsub("^CPA_", "", cazcarro_etal_cpas)

hh_fd_bp_selected <- hh_fd_bp |> 
  filter(ind_ava %in% cazcarro_etal_cpas_as_industries) |> 
  mutate(values = if_else(ind_ava == "U", 0, values))

hh_fd_pp <- hh_fd_bp_selected |> 
  pull(values) |> 
  bp_pp_convert(
    bp = _,
    ttm_margins = ttm_margins_positive,
    ttm_shares = ttm_shares,
    tls_margins = tls_margins
  )

hh_fd_pp_df <- tibble(
  nace_r2 = cazcarro_etal_cpas_as_industries,
  hh_fd_pp = hh_fd_pp
)

eurostat_industry_to_fingreen_industry_map <- readxl::read_xlsx(
  "source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx",
  sheet = "ava"
)

hh_fd_pp_by_fingreen_industry <- hh_fd_pp_df |> 
  left_join(eurostat_industry_to_fingreen_industry_map, by = c("nace_r2" = "eurostat_industry_code")) |> 
  filter(nace_r2 != "U") |> # U not needed, 0 anyway
  group_by(fingreen_industry_code) |> 
  summarise(hh_fd_pp = sum(hh_fd_pp * coalesce(disaggregation_coefficient, 1)))
  

# check that all industries in the mapping get a match from the data
fd_mapping_has_misses <- hh_fd_pp_df |>
  right_join(
    filter(eurostat_industry_to_fingreen_industry_map, relationship != "extra"),
    by = c("nace_r2" = "eurostat_industry_code")
  ) |> 
  pull(hh_fd_pp) |> 
  anyNA()

if(fd_mapping_has_misses){
  stop("Final demand data (hh_fd_pp) was not mapped correctly, check the mapping")
}

row_totals_unscaled <- hh_fd_pp_by_fingreen_industry$hh_fd_pp * 1e6

# scale the row totals to match the col totals, as was done by Pisa team
# The difference is small, so it is not too significant which one we scale

row_totals <- row_totals_unscaled * sum(col_totals) / sum(row_totals_unscaled)

# Prepare data for RAS ----------------------------------------------

matrix_to_adjust <- bridge_transform %>%
  dplyr::select(-fingreen_industry_code) |> 
  as.matrix()

# Check if dimensions match
if (length(row_totals) != nrow(matrix_to_adjust) || length(col_totals) != ncol(matrix_to_adjust)) {
  stop("Mismatch between the dimensions of the matrix and the target margins.")
} else {catn("Bridge matrix dimensions correct.")}

# Check for negative values
if (any(matrix_to_adjust < 0) || any(row_totals < 0) || any(col_totals < 0)) {
  stop("Negative values found. The RAS method requires non-negative data.")
} else {catn("Success - no negative values found")}

# Perform RAS ----------------------------------------------

## matrix balancing using the Iterative Proportional Fitting Procedure (IPFP), also known as the RAS method

#Matrix balancing with example data
# data_matrix <- matrix(c(1,2,3,4), nrow=2)
# row_totals <- c(10, 20)
# col_totals <- c(15, 15)
# result <- Ipfp(data_matrix, list(1,2), list(row_totals, col_totals))
# balanced_matrix <- result$x.hat

# Ipfp(seed, target.list, target.data, print = FALSE, iter = 1000, tol = 1e-10,
#      tol.margins = 1e-10, na.target = FALSE)

convergence_criterion <- 5e-7 # if the Ipfp is not converging, adjust convergence criterion

# handle warnings as errors for the Ipfp fitting
orig_opts <- options()
options(warn=2) # throw error for warnings

ras_result <- Ipfp(matrix_to_adjust, list(1,2), list(row_totals, col_totals), iter = 1e4, tol = convergence_criterion)

options(orig_opts) # back to original options and warning behaviour

#Extract the balanced matrix 
balanced_matrix <- ras_result$x.hat

iterations <- length(ras_result$evol.stp.crit) #Number of iterations taken to converge
error_margins <- ras_result$error.margins #maximum absolute differences between the computed margins and the target margins for each dimension
catn("The algorithm converged when the maximum absolute deviation was less than approximately ",error_margins[1])
max_deviation <- max(error_margins)

#Define variables for cat 
max_deviation_formatted <- formatC(max_deviation, format = "e", digits = 2)

catn(
  "The IPFP was configured to balance row and column margins (list(1,2)) using a convergence tolerance of ",
  convergence_criterion,
  ". The algorithm terminated after ",
  iterations,
  " iterations. The resulting matrix reproduced the specified margins with a maximum absolute deviation of ",
  max_deviation_formatted,
  "."
)

# results ----------------------------------------------------------------

#Calculate shares by column 
balanced_matrix_shares <- sweep(balanced_matrix, 2, colSums(balanced_matrix), "/")

#Calculate col totals 
totals <- as.data.frame(t(colSums(balanced_matrix, na.rm = TRUE)))
balanced_matrix_export <- rbind(balanced_matrix, totals)

share_totals <- as.data.frame(t(colSums(balanced_matrix_shares, na.rm = TRUE)))
balanced_matrix_shares_export <- rbind(balanced_matrix_shares, share_totals)


# Add Industry information back in 
balanced_matrix_export <- data.frame(
  fingreen_industry_code = c(bridge_transform$fingreen_industry_code, "Colsums"),
  balanced_matrix_export,
  check.names = FALS>E
)

balanced_matrix_shares_export <- data.frame(
  fingreen_industry_code = c(bridge_transform$fingreen_industry_code, "Colsums"),
  balanced_matrix_shares_export,
  check.names = FALSE
)

#Export  result 
export_path <- paste0(results_dir,"coicop-nace-ras-balanced-bridge-matrix.ods")
export_list <- list(
  "Balanced Matrix Shares" = balanced_matrix_shares_export,
  "Balanced Matrix Values" = balanced_matrix_export
)
readODS::write_ods(export_list, path = export_path)
