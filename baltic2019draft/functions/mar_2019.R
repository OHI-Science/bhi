
MAR <- function(layers){

  ## From code in 'functions.R MAR' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year


  ## Status ----

  mar_status <- AlignDataYears(layer_nm="mar_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Trend ----

  mar_trend <- AlignDataYears(layer_nm="mar_trend", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Return mariculture status and trend scores ----

  mar_status_and_trend <- dplyr::bind_rows(
    mutate(mar_status, goal = "MAR"),
    mutate(mar_trend, goal = "MAR")
  )
  scores <- select(mar_status_and_trend, region_id, goal, dimension, score)

  return(scores)


} # End MAR function
