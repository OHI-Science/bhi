
EUT <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R EUT' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, 'CW/eutrophication' subfolders)
  ## status: Secchi + Anoxia + DIN (dissolved inorganic nitrogen) + DIP (dis. inorg. phophorus) + Chla (Chlor. A)

  eut_status <- AlignDataYears(layer_nm="cw_eut_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  eut_trend <- AlignDataYears(layer_nm="cw_eut_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  scores <- rbind(eut_status, eut_trend) %>%
    mutate(goal = "EUT") %>%
    select(region_id, goal, dimension, score) %>%
    arrange(dimension, region_id)

  return(scores)

} ## End EUT function
