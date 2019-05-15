
LIV <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R LIV' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, prep 'LIV' subfolder)

  liv_status <- AlignDataYears(layer_nm="liv_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = as.character("status")) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  liv_trend <- AlignDataYears(layer_nm="liv_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = as.character("trend")) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- rbind(liv_status, liv_trend) %>%
    mutate(goal = "LIV") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End LIV function
