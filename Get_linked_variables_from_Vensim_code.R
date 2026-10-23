
## Remove all existing objects from the environment
rm(list=ls()) 

# libraries ---------------------------------------------------------------
# library(dplyr)
# library(readxl)
# library(writexl)
# library(mipfp)
# library(readxl)
# 
# source("fingreen-r-utils.R")
# source("R/bp-pp-convert.R")

# directory setup ---------------------------------------------------------
working_directory <- getwd()

results_dir <- paste0(working_directory, "/results/Vensim-code/")



###FINLAND

data_file <- "source-data/Vensim-code-txt/FINGREEN_w_sufficiency_v2.02.txt"


# Read input file
text <- readChar(data_file, file.info(data_file)$size, useBytes = TRUE)

# Define a paragraph break pattern that covers Windows, Unix, and old Mac line endings
paragraph_break <- "(\\r\\n|\\n|\\r)"

# Remove paragraph breaks after the specified preceding symbols
text <- gsub(paste0("\\\\", paragraph_break), "", text)
text <- gsub(paste0("\\( ", paragraph_break), "(", text)
text <- gsub(paste0(", ", paragraph_break), ",", text)

# Replace two or more consecutive tabs with a single tab
text <- gsub("\t{2,}", "\t", text)

# Replace tab followed by comma with just comma
text <- gsub("\t,", ",", text)

# Replace any occurrence of more than one space with a single space
text <- gsub(" {2,}", " ", text)

# Write the cleaned text back to a file
output_file <- paste0(results_dir,"FINGREEN_w_sufficiency_v2.02_cleaned.txt")
writeChar(text, output_file, useBytes = TRUE)




###ITALY 
data_file <- "source-data/Vensim-code-txt/MODEL_2023_households_v3_w_energy.txt"

# Read input file
text <- readChar(data_file, file.info(data_file)$size, useBytes = TRUE)

# Define a paragraph break pattern that covers Windows, Unix, and old Mac line endings
paragraph_break <- "(\\r\\n|\\n|\\r)"

# Remove paragraph breaks after the specified preceding symbols
text <- gsub(paste0("\\\\", paragraph_break), "", text)
text <- gsub(paste0("\\( ", paragraph_break), "(", text)
text <- gsub(paste0("\\(", paragraph_break,"\t"), "(", text)
text <- gsub(paste0(", ", paragraph_break), ",", text)

# Replace equals sign + paragraph break + tab with just equals sign
text <- gsub(paste0("=", paragraph_break, "\t"), "= ", text)

# Replace two or more consecutive tabs with a single tab
text <- gsub("\t{2,}", "\t", text)

# Replace tab followed by comma with just comma
text <- gsub("\t,", ",", text)

# Replace any occurrence of more than one space with a single space
text <- gsub(" {2,}", " ", text)

# Write the cleaned text back to a file
output_file <- paste0(results_dir,"MODEL_2023_households_v3_w_energy_cleaned.txt")
writeChar(text, output_file, useBytes = TRUE)

