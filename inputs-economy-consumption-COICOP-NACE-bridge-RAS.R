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

# directory setup ---------------------------------------------------------
working_directory <- getwd()

results_dir <- paste0(working_directory, "/results/ras-coicop-nace-bridge/")

#results_dir <- working_directory #paste0(working_directory, "/results/")
#create_dir_if_not_exists(results_dir, "results")

# data file name
data_dir <- paste0(working_directory, "/source-data/inputs-economy/consumption/")
data_file <- paste0(data_dir, "COICOP-NACE input from Cazcarro et al 2022 Annex 1",".xlsx") 

# Load data -------------------------------------------------------------
orig_bridge <- read_xlsx(data_file, sheet = "Original_data")
row_total_for_ras <- read_xlsx(data_file, sheet = "Row_total_for_RAS")
col_total_for_ras <- read_xlsx(data_file, sheet = "Col_total_for_RAS")
coicop_map <- read_xlsx(data_file, sheet = "COICOP_map")
nace_map <- read_xlsx(data_file, sheet = "NACE_map")

# Assign row and col totals from input data
row_totals <- as.numeric(row_total_for_ras$HH_Fd_Dom)
col_totals <- as.numeric(col_total_for_ras$HH_d_CP)

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
    dplyr::filter(nace_map),
    by = "CPA",
    relationship = "many-to-many"
  ) %>%
  # aggregate rows to NACE industry level using coefficients
  dplyr::group_by(fingreen_industry_code) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::where(is.numeric) & !dplyr::all_of("disaggregation_coefficient"),
      ~ sum(.x * dplyr::coalesce(disaggregation_coefficient, 1), na.rm = TRUE)
    ),
    .groups = "drop") %>%
  dplyr::mutate(
    dplyr::across(
      dplyr::where(is.numeric),
      ~ .x * 1e6
    )
  )


#Export table 
writexl::write_xlsx(bridge_transform, paste0(results_dir,"COICOP-NACE-bridge_remapped_Cazcarro.xlsx"))


# #Calculate shares by column 
# bridge_col_shares <- bridge_transform %>%
#   mutate(across(-1, ~ .x / sum(.x, na.rm = TRUE)))


#Calculate shares by row
bridge_row_shares <- bridge_transform %>%
  #For all except first row, divide each row's values by that row's total
  mutate(across(-1, ~ .x / rowSums(across(-1), na.rm = TRUE))) %>% 
  #Replace all NaN with 0
  mutate(across(-1, ~ replace(.x, is.nan(.x), 0)))

#Allocate all fd of industry MN_72 to the last coicop category
bridge_row_shares[32, ncol(bridge_row_shares)] <- 1


#Export table 
# writexl::write_xlsx(bridge_row_shares, paste0(results_dir,"COICOP-NACE-bridge_Cazcarro_row_shares.xlsx"))


#Multiply shares by row targets to allocate the Finnish HH fd to each COICOP according the the cell shares 
bridge_row_fd <- bridge_row_shares %>%
  mutate(across(-1, ~ .x * row_totals))  


# # Calculate column sums of shares (should all be 1)
# colsums <- bridge_row_shares %>%
#   summarise(across(-1, sum), .groups = "drop") %>%
#   mutate(!!names(bridge_row_shares)[1] := "Colsums")
# 
# # Combine
# bridge_transform_shares <- bind_rows(bridge_row_shares, colsums)
# 
# #Export table 
# writexl::write_xlsx(bridge_transform_shares, paste0(results_dir,"COICOP-NACE-bridge_transform_shares.xlsx"))



#Check values aggregated correctly 

# orig_bridge |>
#   dplyr::filter(CPA == "CPA_C13-15") |>
#   dplyr::select(CP031, CP032)
# 
# bridge_cp_transform |>
#   dplyr::filter(CPA == "CPA_C13-15") |>
#   dplyr::select(CP03)



# Prepare data for RAS ----------------------------------------------

# Calculate row and col totals from data
# # row totals as a numeric vector
# row_totals <- rowSums(as.matrix(bridge_transform[, -1]), na.rm = TRUE)
# # column totals as a numeric vector
# col_totals <- colSums(as.matrix(bridge_transform[, -1]), na.rm = TRUE)


# str(col_totals)
matrix <- bridge_row_fd %>%
  dplyr::select(-fingreen_industry_code) 

# Check if dimensions match
if (length(row_totals) != nrow(matrix) || length(col_totals) != ncol(matrix)) {
  stop("Mismatch between the dimensions of the matrix and the target margins.")
} else {"No mismatch found"}

# Check for negative values
if (any(matrix < 0) || any(row_totals < 0) || any(col_totals < 0)) {
  stop("Negative values found. The RAS method requires non-negative data.")
} else {"No negative values found"}



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

result <- Ipfp(matrix, list(1,2), list(row_totals, col_totals))
# warnings()

#Extract the balanced matrix 
balanced_matrix <- result$x.hat


#Export the balanced matrix before share calculations  
writexl::write_xlsx(balanced_matrix, paste0(results_dir,"COICOP-NACE-bridge-balanced.xlsx"))



#Check result contents 
str(result)

#Save relevant information 
iterations <- length(result$evol.stp.crit) #Number of iterations taken to converge
error_margins <- result$error.margins #maximum absolute differences between the computed margins and the target margins for each dimension
cat("The algorithm converged when the maximum absolute deviation was less than approximately",error_margins[1])
max_deviation <- max(error_margins)

#Define variables for cat 
tol <- "1e-5"  # Default convergence tolerance
max_deviation_formatted <- formatC(max_deviation, format = "e", digits = 2)


# Print the statement using cat()
cat(
  "The IPFP was configured to balance row and column margins (list(1,2)) using a convergence tolerance of",
  tol, ". The algorithm terminated after", iterations,
  "iterations, with a maximum of 1,000 iterations allowed. The resulting matrix reproduced the specified margins with a maximum absolute deviation of",
  max_deviation_formatted, "."
) # Check for accuracy! 





#Calculate shares by column 
balanced_matrix_shares <- sweep(balanced_matrix, 2, colSums(balanced_matrix), "/")

#Calculate col totals 
totals <- as.data.frame(t(colSums(balanced_matrix, na.rm = TRUE)))
balanced_matrix_export <- rbind(balanced_matrix, totals)

share_totals <- as.data.frame(t(colSums(balanced_matrix_shares, na.rm = TRUE)))
balanced_matrix_shares_export <- rbind(balanced_matrix_shares, share_totals)


#Add Industry information back in 
balanced_matrix_export <- data.frame(
  fingreen_industry_code = c(bridge_transform$fingreen_industry_code, "Colsums"),
  balanced_matrix_export,
  check.names = FALSE
)

balanced_matrix_shares_export <- data.frame(
  fingreen_industry_code = c(bridge_transform$fingreen_industry_code, "Colsums"),
  balanced_matrix_shares_export,
  check.names = FALSE
)


#Export  result 
export_name <- paste0(results_dir,"COICOP-NACE RAS balanced bridge matrix.xlsx")
export_list <- list(
  "Balanced Matrix Shares" = balanced_matrix_shares_export,
  "Balanced Matrix Values" = balanced_matrix_export
)
writexl::write_xlsx(export_list, path = export_name)
