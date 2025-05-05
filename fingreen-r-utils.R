coicop_to_fingreen_coicop <- function(codes){
  library(dplyr)
  # Note that the category level has to be at least 2 (which corresponds to 3 in the official level)
  res <- case_when(
    grepl("^01\\.", x = codes, perl = T) ~ "CP01",
    grepl("^02\\.", x = codes, perl = T) ~ "CP02",
    grepl("^03\\.", x = codes, perl = T) ~ "CP03",
    grepl("^04\\.[1,2,3]", x = codes, perl = T) ~ "CP041_043",
    grepl("^04\\.4", x = codes, perl = T) ~ "CP044",
    grepl("^01\\.5", x = codes, perl = T) ~ "CP045",
    grepl("^05\\.", x = codes, perl = T) ~ "CP05",
    grepl("^06\\.", x = codes, perl = T) ~ "CP06",
    grepl("^07\\.[1,2]", x = codes, perl = T) ~ "CP071_072",
    grepl("^07\\.3", x = codes, perl = T) ~ "CP073",
    grepl("^08\\.", x = codes, perl = T) ~ "CP08",
    grepl("^09\\.", x = codes, perl = T) ~ "CP09",
    grepl("^10\\.", x = codes, perl = T) ~ "CP10",
    grepl("^11\\.", x = codes, perl = T) ~ "CP11",
    grepl("^12\\.1", x = codes, perl = T) ~ "CP121",
    grepl("^12\\.[2,3,4,5,6,7]", x = codes, perl = T) ~ "CP122_127",
  )
  return(res)
}

eurostat_coicop_to_fingreen_coicop <- function(codes){
  library(dplyr)
  
   res <- case_when(
    grepl("^CP01", x = codes, perl = T) ~ "CP01",
    grepl("^CP02", x = codes, perl = T) ~ "CP02",
    grepl("^CP03", x = codes, perl = T) ~ "CP03",
    grepl("^CP04[1,2,3]", x = codes, perl = T) ~ "CP041_043",
    grepl("^CP044", x = codes, perl = T) ~ "CP044",
    grepl("^CP045", x = codes, perl = T) ~ "CP045",
    grepl("^CP05", x = codes, perl = T) ~ "CP05",
    grepl("^CP06", x = codes, perl = T) ~ "CP06",
    grepl("^CP07[1,2]", x = codes, perl = T) ~ "CP071_072",
    grepl("^CP073", x = codes, perl = T) ~ "CP073",
    grepl("^CP08", x = codes, perl = T) ~ "CP08",
    grepl("^CP09", x = codes, perl = T) ~ "CP09",
    grepl("^CP10", x = codes, perl = T) ~ "CP10",
    grepl("^CP11", x = codes, perl = T) ~ "CP11",
    grepl("^CP121", x = codes, perl = T) ~ "CP121",
    grepl("^CP12[2,3,4,5,6,7]", x = codes, perl = T) ~ "CP122_127",
  )
}

fingreen_coicop_to_description <- function(codes){
  library(dplyr)
  res <- case_match(
    codes,
    "CP01" ~ "Food and non-alcoholic beverages",
    "CP02" ~ "Alcoholic beverages, tobacco and narcotics",
    "CP03"	~ "Clothing and footwear",
    "CP041_043" ~	"Housing",
    "CP044" ~	"Water supply and miscellaneous services relating to the dwelling",
    "CP045" ~	"Electricity, gas and other fuels",
    "CP05" ~ "Furnishings, household equipment and routine household maintenance",
    "CP06" ~ "Health",
    "CP071_072" ~ "Private transport",
    "CP073" ~ "Transport services",
    "CP08" ~ "Communications",
    "CP09" ~ "Recreation and culture",
    "CP10" ~ "Education",
    "CP11" ~ "Restaurants and hotels",
    "CP121" ~ "Personal care",
    "CP122_127" ~ "Other"
  )
  return(res)
}

is_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
}

catn <- function(...){
  cat(... , "\n", sep = "")
}

make_names <- function(x){
  res <- tolower(gsub(".", "_", x = make.names(x), fixed = T)) %>% 
    gsub("_+", "_", x = ., perl = T) %>% 
    gsub("^_", "", x = ., perl = T)
  return(res)
}

fix_names <- function(df){
  library(dplyr)
  res <- rename_all(df, make_names)
  return(res)
}

replace_na <- function(x, value){
  library(dplyr)
  res <- if_else(is.na(x), value, x)
  return(res)
}

nth_biggest <- function(x, n, unique_only = TRUE){
  if(isTRUE(unique_only)) x <- unique(x)
  k <- length(x)
  res <- sort(x,partial=k-n+1)[k-n+1]
  return(res)
}

save_plotly_plot <- function(plot, file, ...){
  htmlwidgets::saveWidget(widget = plotly::as_widget(plot), file = "tmp.html", selfcontained = TRUE)
  file.rename("tmp.html", file)
}
