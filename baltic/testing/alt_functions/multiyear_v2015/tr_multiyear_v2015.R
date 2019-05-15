
TR <- function(layers){

  ## From code in 'functions.R TR' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep TR subfolders)
  ## possible alternative: base metric on EU blue growth reports?

  scen_year <- layers$data$scenario_year

  tr_status <- AlignDataYears(layer_nm="tr_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  tr_trend <- AlignDataYears(layer_nm="tr_trend", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- dplyr::bind_rows(tr_status, tr_trend) %>%
    dplyr::mutate(goal = "TR")

  return(scores)

} ## end TR function
