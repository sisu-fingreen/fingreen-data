get_number_of_households <- function(geo, year) {
  res <- eurostat::get_eurostat(
    "lfst_hhnhtych",
    time_format = "num",
    filters = list(
      geo = geo,
      time = year,
      agechild = "TOTAL",
      n_child = "TOTAL",
      phhcomp = "TOTAL"
    )
  ) |> 
    dplyr::transmute(
      geo = geo,
      year = as.integer(time),
      n_households = as.integer(values * 1e3)
    )

  return(res)
}