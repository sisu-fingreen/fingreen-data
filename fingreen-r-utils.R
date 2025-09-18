# general utils -----------------------------------------------------------

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

create_dir_if_not_exists <- function(dir_path, purpose = ""){
  if(!dir.exists(file.path(dir_path))){
    dir.create(file.path(dir_path), recursive = TRUE)
    catn(sprintf("Created %s directory %s", purpose, dir_path))
  } else{
    catn(sprintf("%s directory '%s' exists", purpose, dir_path))
  }
  invisible()
}

# plotting -----------------------------------------------------------------

save_plotly_plot <- function(plot, file, ...){
  htmlwidgets::saveWidget(widget = plotly::as_widget(plot), file = "tmp.html", selfcontained = TRUE)
  file.rename("tmp.html", file)
}


# categorizations ----------------------------------------------------------

statfin_coicop_to_fingreen_coicop <- function(codes){
  library(dplyr)
  # Note that the category level has to be at least 2 (which corresponds to 3 in the official level)
  res <- case_when(
    grepl("^01\\.", x = codes, perl = T) ~ "CP01",
    grepl("^02\\.", x = codes, perl = T) ~ "CP02",
    grepl("^03\\.", x = codes, perl = T) ~ "CP03",
    grepl("^04\\.[1,2,3]", x = codes, perl = T) ~ "CP041_043",
    grepl("^04\\.4", x = codes, perl = T) ~ "CP044",
    grepl("^04\\.5", x = codes, perl = T) ~ "CP045",
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

fingreen_industry_code_to_description <- function(codes){
  res <- factor(
    codes,
    levels = c("A1", "A2", "A3", "B", "C10_12", "C13_15", "C16_17", "C18", 
               "C19", "C20_22", "C23", "C24_25", "C26_27", "C28", "C29", "C30", 
               "C31_32", "C33", "D1", "D2", "D3", "E", "F", "G", "H49_51", "H52_53", 
               "I", "J", "K", "L", "MN", "MN_72", "MN_73", "O", "P", "Q", "R", 
               "S95", "ST"),
    labels = c("01 Crop and animal production, hunting and related service activities", 
               "02 Forestry and logging", "03 Fishing and aquaculture", "B Mining and quarrying (05-09)", 
               "10-12 Food, beverages and tobacco", "13-15 Textile, clothing and leather industry", 
               "16-17 Forest industry", "18 Printing and reproduction of recorded media", 
               "19 Manufacture of coke and refined petroleum products", "20-22 Chemical industry - Except for petroleum (altered)", 
               "23 Manufacture of other non-metallic mineral products", "24-25 Manufacture of basic metals and fabricated metal products (altered)", 
               "26-27 Manufacture of electrical and electronic products", "28 Manufacture of machinery and equipment n.e.c.", 
               "29 Manufacture of motor vehicles, trailers and semi-trailers", 
               "30 Manufacture of other transport equipment", "31-32 Manufacture of furniture and other products", 
               "33 Repair and installation of machinery and equipment", "35 Electricity (altered; D disaggreagted)", 
               "35 Gas (altered; D disaggreagted)", "35 Heat (altered; D disaggreagted)", 
               "E Water supply; sewerage, waste management and remediation activities (36-39)", 
               "F Construction (41-43)", "G Wholesale and retail trade; repair of motor vehicles and motorcycles (45-47)", 
               "49-51 Transportation (altered; Land, Water and Air only)", "52-53 Transport related logistics, warehousing and postal services (altered)", 
               "I Accommodation and food service activities (55-56)", "J Information and communication (58-63)", 
               "K Financial and insurance activities (64-66)", "L Real estate activities (68)", 
               "MN Professional, scientific and technical activities, administrative and support service activities - Except for scientific R&D and advertising (altered; 69-71, 74-82)", 
               "72 Scientific research and development", "73 Advertising and market research", 
               "O Public administration and defence; compulsory social security (84)", 
               "P Education (85)", "Q Human health and social work activities (86-88)", 
               "R Arts, entertainment and recreation (90-93)", "95 Repair of computers and personal and household goods", 
               "ST Other service activities - Including household emplyers, excluding repair (altered; 94, 96-98)"
    )
  ) %>% as.character()
  return(res)
}

fingreen_industry_code_to_abbreviation <- function(codes){
  res <- factor(
    codes,
    levels = c("A1", "A2", "A3", "B", "C10_12", "C13_15", "C16_17", "C18", 
               "C19", "C20_22", "C23", "C24_25", "C26_27", "C28", "C29", "C30", 
               "C31_32", "C33", "D1", "D2", "D3", "E", "F", "G", "H49_51", "H52_53", 
               "I", "J", "K", "L", "MN", "MN_72", "MN_73", "O", "P", "Q", "R", 
               "S95", "ST"),
    labels = c("A1 agriculture", "A2 forestry", "A3 fishing", "B mining", 
               "C10_12 manu_food", "C13_15 manu_clothing", "C16_17 manu_forest", 
               "C18 manu_record", "C19 manu_petroleum", "C20_22 manu_chemical", 
               "C23 manu_mineral", "C24_25 manu_metal", "C26_27  manu_electrical", 
               "C28 manu_machine", "C29 manu_vechicles", "C30 manu_transport", 
               "C31_32 manu_furniture", "C33 manu_install", "D1 electricity", 
               "D2 gas", "D3 heat", "E water", "F construction", "G commerce", 
               "H49_51 transportation", "H52_53 logistics", "I hospitality", 
               "J ict", "K finance", "L realestate", "MN serv_prof", "MN_72 serv_science", 
               "MN_73 serv_advertising", "O public", "P education", "Q health", 
               "R entertainment", "S95 serv_repair", "ST serv_other")
  ) %>% as.character()
  return(res)
}

# misc --------------------------------------------------------------------

convert_eur_value_between_years <- function(x, from, to){
  stopifnot(is_installed("pxweb"))
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(from))
  stopifnot(is.numeric(to))
  
  # If no need to convert, just return the original value and avoid api calls
  if(all(from == to)){
    return(x)
  }
  
  year_pairs_list <- tibble(from, to) %>% distinct() %>% filter(from != to) %>% asplit(1)
  
  get_conversion_factor_for_year_pair <- function(year_pair) {
    
    # Statfin has a handy api for money (eur) value conversion between years
    conversion_factor_query <- pxweb::pxweb_query(
      list(Vuosi = as.character(year_pair), Tiedot = "pisteluku")
    )
    
    conversion_factor <- pxweb::pxweb_get(
      url = "https://pxdata.stat.fi/PxWeb/api/v1/fi/StatFin/khi/statfin_khi_pxt_11xy.px",
      query = conversion_factor_query
    ) %>% as.data.frame() %>% 
      fix_names() %>% 
      arrange(vuosi) %>% 
      summarise(conversion_factor = first(pisteluku) / last(pisteluku)) %>% 
      pull(conversion_factor)
    
    return(conversion_factor)
  }
  
  conversion_factors <- lapply(year_pairs_list, get_conversion_factor_for_year_pair) %>% unlist()
  
  conversion_df <- bind_rows(year_pairs_list) %>% cbind(conversion_factors)
  
  res_df <- tibble(x, from, to) %>% 
    left_join(conversion_df, by = c("from", "to")) %>% 
    mutate(
      res = case_when(
        from == to ~ x,
        from != to ~ x * conversion_factors
      )
    )
  
  return(res_df$res)
}

convert_data_from_euklems_to_fingreen_industry <- function(df, mapping, join_var, id_vars, vars_to_transform){
  
  catn(
    "Warning. Converting data from EUKLEMS to fingreen industry categorization will",
    "mess with the totals over industry categories, since the mapping is many-to-many. ",
    "Also, disaggregated categories inherit total values from their parent categories. Use only with intention."
  )
  
  stopifnot(
    identical(colnames(mapping), c(join_var, "fingreen_industry_code", "relationship", "recomposition_method"))
  )
  stopifnot(join_var %in% colnames(df))
  stopifnot(!"relationship" %in% colnames(df))
  
  df <- df %>% 
    inner_join(mapping, by = join_var, relationship = "many-to-many")
  
  # Conversion logic in pieces, according to the relationship between the categories
  
  # One-to-one is simple, no need to do anything
  df_one_to_one <- filter(df, relationship == "one-to-one") %>% 
    select(all_of(c("fingreen_industry_code", id_vars, vars_to_transform)))
  
  # Disaggregation and inheritance is also simple, the new child categories already got the value of the parent category
  # in the join
  df_disaggregated <- filter(
    df,
    relationship == "disaggregation" |
      (relationship == "recomposition" & recomposition_method == "disaggregation")
  ) %>% 
    select(all_of(c("fingreen_industry_code", id_vars, vars_to_transform)))
  
  df_inherited <- filter(df, relationship == "recomposition" & recomposition_method == "inherit") %>% 
    select(all_of(c("fingreen_industry_code", id_vars, vars_to_transform)))
  
  # Aggregation is straightforward, just sum the child categories under the new parent
  df_aggregated <- filter(
    df,
    relationship == "aggregation" |
      (relationship == "recomposition" & recomposition_method == "aggregation")
  ) %>% 
    group_by(across(all_of(c("fingreen_industry_code", id_vars)))) %>% 
    summarise(across(all_of(vars_to_transform), .fns = sum), .groups = "drop")
  
  # One last thing is averaging, for categories which are partially overlapping
  
  df_averaged <- filter(df, relationship == "recomposition" & recomposition_method == "average") %>% 
    group_by(across(all_of(c("fingreen_industry_code", id_vars)))) %>% 
    summarise(across(all_of(vars_to_transform), .fns = mean), .groups = "drop")
  
  res <- bind_rows(
    df_one_to_one,
    df_disaggregated,
    df_aggregated,
    df_averaged,
    df_inherited
  )
  return(res)
}

