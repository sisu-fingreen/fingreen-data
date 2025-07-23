library(dplyr)
library(rvest)
source("fingreen-r-utils.R")

output_dir <- "source-data/tsm/"
create_dir_if_not_exists(paste0(getwd(), "/", output_dir))

tsm_base_url <- "https://kaivosvastuu.fi/yhtio/kaivostoimintaa/"
tsm_base_page <- read_html(tsm_base_url)
tsm_report_urls <- tsm_base_page |>
  html_elements(".cta-primary") |>
  html_attr("href")

# The url sometimes has the wrong year, but the text has the right one. Extract the correct years
tsm_report_years <- tsm_base_page |>
  html_elements(".cta-primary") |>
  html_text()

tsm_report_url_and_year <- cbind(tsm_report_urls, tsm_report_years) |> 
  asplit(MARGIN = 1)

scrape_tsm_data <- function(tsm_url_and_year, output_dir) {

  # Scrapes data from TSM Finland reports, and outputs (append) to one csv file
  # tsm_url_and_year = named vector of the report url and the year
  # output_dir = where to output
  
  tsm_url = tsm_url_and_year["tsm_report_urls"]
  
  year = tsm_url_and_year["tsm_report_years"] |> as.integer()
  
  mine <- gsub(x = tsm_url, ".*?/\\d{4}-(.*)/$", "\\1") |>
    # Trim possible dashes and numbers from the url after the mine name
    gsub(x = _, "[-\\d]$", "")
  
  page <- read_html(tsm_url)

  # responsibility_report_url <- page |>
  #   html_element(".responsibilityreport") |> html_children() |> 
  #   html_attr("href")

  # responsibility_report_file_extension <- responsibility_report_url |> 
  #   gsub(x = _, ".*?\\.(\\w*)$", "\\1")

  # responsibility_report_path <- sprintf(
  #   "%sresponsibility-report-%s-%s.%s", output_dir, year, mine, responsibility_report_file_extension
  # )

  # tryCatch(
  #   download.file(
  #     url = responsibility_report_url,
  #     destfile = responsibility_report_path
  #   ),
  #   error = function(e) print(e)
  # )
  
  elements_to_extract <- page |> html_elements(".py-3")
  
  filter_vec_regex <- function(vec, expression) {
    res <- vec[grepl(pattern = expression, x = vec, perl = T)]
    return(res)
  }
  
  key_figures <- elements_to_extract |> html_text() |> filter_vec_regex("Avainluvut")
  company_figures<- elements_to_extract |> html_text() |> filter_vec_regex("Yritys lukuina")
  
  keep_only_first_colon_for_each_row <- function(vec){
    # first replace all : with ;, then replace the first ; back to :
    res <- vec |>
      gsub(x = _, ":", ";", perl = TRUE) |>
      gsub(x = _, "([^;]*);(.*)", "\\1:\\2", perl = TRUE)
    # res <- gsub("(\\:.*?)(:)(.*)", replacement = "\\1;\\3", x = vec, perl = TRUE)
    return(res)
  }
  
  parse_to_df <- function(figures_txt, skip = 0) {
    res <- tryCatch(
      data.table::fread(
        figures_txt,
        sep = ":",
        header = FALSE,
        col.names = c("variable_name", "value"),
        skip = skip
      ),
      error = function(e) tibble(variable_name = "parse_error", value = as.character(e))
    )
    return(res)
  }
  
  key_figures_df <- key_figures |>
    keep_only_first_colon_for_each_row() |>
    lapply(FUN = parse_to_df, skip = "Avainluvut") |> 
    bind_rows() |> 
    mutate(
      info_category = "key-figures",
      mine = mine,
      year = year
    )
  
  company_figures_df <- company_figures |>
    keep_only_first_colon_for_each_row() |> 
    parse_to_df() |>
    mutate(
      info_category = "company-figures",
      mine = mine,
      year = year
    )
  
  output_file <- paste0(output_dir, "tsm-statistics-fi.csv")

  key_figures_df |>
    bind_rows(company_figures_df) |>
    relocate(year, mine, info_category) |>
    data.table::fwrite(
      file = output_file,
      append = TRUE
    )
  
  # writexl::write_xlsx(
    # list("key_figures" = key_figures_df, "company_figures" = company_figures_df),
    # path = paste0(output_dir, "tsm-statistics-", mine, "-", year, ".xlsx")
  # )
  
}

lapply(tsm_report_url_and_year, FUN = scrape_tsm_data, output_dir = output_dir) |> invisible()

# debugonce(scrape_tsm_data)

# scrape_tsm_data("https://kaivosvastuu.fi/kaivostoiminta/2023-nordkalk-oy-ab/", output_dir)
