transform_categorization <- function(
  df, # df to convert to new industry structure
  new_mapping, # a mapping from the old eg. industry structure to the new
  old_mapping_name, # name of old mapping variable in the data
  new_mapping_name, # name of the new mapping variable in the data
  value_vars, # variables that will be reaggregated
  grouping_vars = NULL, # other variables to preserve in the result, if NULL, preserve all
  use_disaggregation_coefficient = TRUE,
  # How should unmatched industry categories be handled in
  #  x (the data df) and y (the mapping df)? "drop" or "error"
  unmatched = c("x" = "drop", "y" = "error")
) {

  library(dplyr)
  
  if(is.null(grouping_vars)) {
    vars_to_group_by <- colnames(df)[!colnames(df) %in% c(old_mapping_name, value_vars)]
  } else {
    vars_to_group_by <- grouping_vars
  }

  df_has_duplicates <- df |> 
    select(all_of(c(old_mapping_name, vars_to_group_by))) |> 
    duplicated() |>
    any()

  if(df_has_duplicates) {
    stop("df has duplicate rows not accounted for by the grouping variables")
  }

  if(!use_disaggregation_coefficient){
    if("disaggregation_coefficient" %in% colnames(new_mapping)){
      stop("use_disaggregation_coefficient = FALSE but there is a disaggregation coefficient in the new mapping")
    }
    new_mapping <- mutate(new_mapping, disaggregation_coefficient = 1)
  }

  res <- df %>%
    inner_join(
      new_mapping,
      by = old_mapping_name,
      relationship = "many-to-many",
      unmatched = unmatched
    ) %>% 
    group_by(pick(all_of(c(grouping_vars, new_mapping_name)))) %>% 
    summarise(
      across(all_of(value_vars), ~ sum(.x * coalesce(disaggregation_coefficient, 1), na.rm = TRUE)),
      .groups = "drop"
    )
  
  return(res)
}
