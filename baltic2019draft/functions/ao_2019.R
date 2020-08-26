
AO <- function(layers){

  ## From code in 'functions.R AO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  ao_status <- AlignDataYears(layer_nm="ao_stock_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Trend ----

  ao_trend <- AlignDataYears(layer_nm="ao_stock_slope", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Return artisial fishing opportunities status and trend scores ----

  ao_status_and_trend <- dplyr::bind_rows(
    mutate(ao_status, goal = "AO"),
    mutate(ao_trend, goal = "AO")
  )
  scores <- select(ao_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End AO function
