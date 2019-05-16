
TRA <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R TRA' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how status and reference point are calculated in prep file (bhi-prep repository, 'CW/trash' subfolders)

  tra_status <- AlignDataYears(layer_nm="cw_tra_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::rename(region_id = rgn_id)

  tra_trend <- AlignDataYears(layer_nm="cw_tra_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::rename(region_id = rgn_id)

  scores <- rbind(tra_status, tra_trend) %>%
    mutate(goal = "ECO") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End TRA function
