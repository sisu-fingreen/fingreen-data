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


### PART 1 - SETUP ###

## Remove all existing objects from the environment
rm(list=ls()) 
# gc()


## Install libraries if needed 
# install.packages("openxlsx") #For opening and exporting excel xlsx files     #### <-- IF NEEDED, REMOVE HASHTAGS TO INSTALL WHEN CODE IS RUN
# install.packages("readxl") 
# install.packages("writexl") 
# install.packages("ipfp") 

# libraries ---------------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(mipfp)
library(openxlsx)


# library(tidyr)
# library(ipfp)
# library(tidyverse)
# library(ggplot2) 

#source("fingreen-r-utils.R")

# needed but not loaded to the namespace
# stopifnot(is_installed("readxl"))
# stopifnot(is_installed("writexl"))
# stopifnot(is_installed("tidyr"))

# directory setup ---------------------------------------------------------
working_directory <- getwd()

results_dir <- paste0(working_directory, "/results/ras-coicop-nace-bridge/")

#results_dir <- working_directory #paste0(working_directory, "/results/")
#create_dir_if_not_exists(results_dir, "results")

# data file name
data_dir <- paste0(working_directory, "/source-data/inputs-economy/ras-coicop-nace-bridge/")
data_file <- paste0(data_dir, "COICOP-NACE input from Cazcarro et al 2022 Annex 1",".xlsx") 

# Load data -------------------------------------------------------------
orig_bridge <- read_xlsx(data_file, sheet = "Original_data")  #Note file type needs to be .xlsx
Row_total_for_RAS <- read_xlsx(data_file, sheet = "Row_total_for_RAS")
Col_total_for_RAS <- read_xlsx(data_file, sheet = "Col_total_for_RAS")
COICOP_map <- read_xlsx(data_file, sheet = "COICOP_map")
NACE_map <- read_xlsx(data_file, sheet = "NACE_map")


# eurostat_to_fingreen_industry_ava_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "ava")
# eurostat_to_fingreen_industry_use_map <- readxl::read_xlsx("source-data/mappings/eurostat-io-industry-to-fingreen-industry-map.xlsx", sheet = "use")


# transform bridge structure ----------------------------------------------

# reshape CP (COICOP) columns to long format
bridge_cp_transform <- orig_bridge %>%
  # reshape the data from wide to long format for the COICOP dimension
  tidyr::pivot_longer(cols = -CPA, names_to = "CP_48", values_to = "values") %>%
  # join COICOP 48 → 16 mapping table
  dplyr::inner_join(
    dplyr::filter(COICOP_map),
    by = "CP_48",
    relationship = "many-to-many"
  ) %>%
  # define target COICOP code with fallback
  dplyr::mutate(
    CP_16 = dplyr::coalesce(CP_16, CP_48)
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
    dplyr::filter(NACE_map),
    by = "CPA",
    relationship = "many-to-many"
  ) %>%
  # define harmonised industry code with CPA fallback
  dplyr::mutate(
    NACE_code = dplyr::coalesce(fingreen_industry_code, CPA)
  ) %>%
  # aggregate rows to NACE industry level using coefficients
  dplyr::group_by(NACE_code) %>%
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



#Export table before RAS 
writexl::write_xlsx(bridge_transform, paste0(results_dir,"COICOP-NACE-bridge_transform_values.xlsx"))



#Calculate shares by column 
shares <- bridge_transform %>%
  mutate(across(-1, ~ .x / sum(.x, na.rm = TRUE)))

# Calculate column sums of shares (should all be 1)
colsums <- shares %>%
  summarise(across(-1, sum), .groups = "drop") %>%
  mutate(!!names(bridge_transform)[1] := "Colsums")

# Combine
bridge_transform_shares <- bind_rows(shares, colsums)


#Export table before RAS 
writexl::write_xlsx(bridge_transform_shares, paste0(results_dir,"COICOP-NACE-bridge_transform_shares.xlsx"))



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

# Assign row and col totals from input data
row_totals <- as.numeric(Row_total_for_RAS$HH_Fd_Dom)
col_totals <- as.numeric(Col_total_for_RAS$HH_d_CP)


# str(col_totals)
matrix <- bridge_transform %>%
  dplyr::select(-NACE_code) 

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
  NACE_code = c(bridge_transform$NACE_code, "Colsums"),
  balanced_matrix_export,
  check.names = FALSE
)

balanced_matrix_shares_export <- data.frame(
  NACE_code = c(bridge_transform$NACE_code, "Colsums"),
  balanced_matrix_shares_export,
  check.names = FALSE
)


#Export  result 
export_name <- paste0(results_dir,"COICOP-NACE RAS balanced bridge matrix.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Balanced Matrix Shares")
writeData(wb, sheet = "Balanced Matrix Shares", x = balanced_matrix_shares_export)
addWorksheet(wb, "Balanced Matrix Values")
writeData(wb, sheet = "Balanced Matrix Values", x = balanced_matrix_export)
saveWorkbook(wb, file = export_name, overwrite = TRUE)






col_totals <- as.numeric(Col_total_for_RAS$HH_d_CP)


#NACE to COICOP code 

#Choose sheet 
data_sheet <- "NACE to COICOP" #### <-- CHANGE THE TEXT IN QUOTES (also make sure the sheet order is correct)

## Read your data
df <- read.xlsx(data_file, sheet = data_sheet) 
#Note file type needs to be .xlsx

ind_col <- df[,1] #Extract industry info 
df <- df[,-1] #remove industry row 
df <- as.data.frame(t(df)) #Transpose data 

# str(df)

#Clean and separate data 
row_totals <- df[,ncol(df)] 
row_totals <- row_totals[-length(row_totals)] #Excluding the final sum value
col_totals <- as.numeric(df[nrow(df), 1:(ncol(df) - 1)])
# str(col_totals)
matrix <- df[1:(nrow(df)-1), 1:(ncol(df)-1)] #Separate data into a matrix 

# Check if dimensions match
if (length(row_totals) != nrow(matrix) || length(col_totals) != ncol(matrix)) {
  stop("Mismatch between the dimensions of the matrix and the target margins.")
} else {"No mismatch found"}

# Check for negative values
if (any(matrix < 0) || any(row_totals < 0) || any(col_totals < 0)) {
  stop("Negative values found. The RAS method requires non-negative data.")
} else {"No negative values found"}

str(matrix)

## Perform matrix balancing using the Iterative Proportional Fitting Procedure (IPFP), also known as the RAS method

#Matrix balancing with example data
# data_matrix <- matrix(c(1,2,3,4), nrow=2)
# row_totals <- c(10, 20)
# col_totals <- c(15, 15)
# result <- Ipfp(data_matrix, list(1,2), list(row_totals, col_totals))
# balanced_matrix <- result$x.hat


# 'matrix' is your initial matrix, 'row_totals' and 'col_totals' are target sums
result <- Ipfp(
  seed        = matrix,
  target.list = list(1, 2),
  target.data = list(row_totals, col_totals),
  iter        = 5000,      # raise iteration cap
  tol         = 1e-5       # optional: tighten convergence tolerance
)
# warnings()

#Extract the balanced matrix 
balanced_matrix <- result$x.hat

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
cat("The IPFP was configured to adjust over rows and columns (list(1,2)), using the default convergence tolerance of", 
    tol, ". The algorithm converged after", iterations, 
    "iterations, the default maximum is 1,000 iterations. The final balanced matrix met the target margins with a maximum absolute deviation of less than", 
    max_deviation_formatted, ", indicating a high level of precision in the balancing process.")
# Check for accuracy! 


#Calculate shares by column 
balanced_matrix_shares <- sweep(balanced_matrix, 2, colSums(balanced_matrix), "/")

# str(balanced_matrix)

#Calculate col totals 
totals <- as.data.frame(t(colSums(balanced_matrix, na.rm = TRUE))) 
balanced_matrix_export <- rbind(balanced_matrix, totals)

share_totals <- as.data.frame(t(colSums(balanced_matrix_shares, na.rm = TRUE)))
balanced_matrix_shares_export <- rbind(balanced_matrix_shares, share_totals)


#Transpose 
balanced_matrix_export <- as.data.frame(t(balanced_matrix_export))
balanced_matrix_shares_export <- as.data.frame(t(balanced_matrix_shares_export))

#Rename last column
colnames(balanced_matrix_export)[ncol(balanced_matrix_export)] <- "Row_Total"
colnames(balanced_matrix_shares_export)[ncol(balanced_matrix_shares_export)] <- "Row_Total"


#Add Industry information back in 
Industries <- ind_col[-length(ind_col)]

balanced_matrix_export <- cbind(Industry = Industries, balanced_matrix_export)
balanced_matrix_shares_export <- cbind(Industry = Industries, balanced_matrix_shares_export)


#Export  result 
export_name <- paste0(data_sheet," RAS balanced matrix.xlsx")
wb <- createWorkbook()
addWorksheet(wb, "Balanced Matrix Shares")
writeData(wb, sheet = "Balanced Matrix Shares", x = balanced_matrix_shares_export)
addWorksheet(wb, "Balanced Matrix Values")
writeData(wb, sheet = "Balanced Matrix Values", x = balanced_matrix_export)
saveWorkbook(wb, file = export_name, overwrite = TRUE)


