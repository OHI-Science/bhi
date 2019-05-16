
CS <- function(layers){

  ## From code in 'functions.R CS' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  cs_status <- AlignDataYears(layer_nm="cs_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  ## for now NA carbon storage trends...
  cs_trend <- data.frame(
    region_id = seq(1, 42, 1),
    score = rep(NA, 42),
    dimension = rep("trend", 42)) %>%
    dplyr::mutate(dimension = as.character(dimension))

  scores <- rbind(cs_status, cs_trend) %>%
    mutate(goal = "CS")

  return(scores)

} ## End CS function
