
ECO <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R ECO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, prep 'ECO' subfolder)

  eco_status <- AlignDataYears(layer_nm="eco_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  eco_trend <- AlignDataYears(layer_nm="eco_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- rbind(eco_status, eco_trend) %>%
    mutate(goal = "ECO") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End ECO function
