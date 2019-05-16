
BD <- function(layers){

  ## From code in 'functions.R BD' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## BD goal currently has no subgoals; it is only species
  ## status calculated in spp_prep file because calculated by basin and then applied to BHI regions
  ## status is the geometric mean of each taxa group status by basin
  ## trend is temporarily substituted by OHI-global BD trend scores

  scen_year <- layers$data$scenario_year

  ## call status and trend layers ----
  status <- AlignDataYears(layer_nm="bd_spp_status", layers_obj=layers) %>%
    dplyr::mutate(score = round(score*100),
           dimension = as.character(dimension)) %>%
    dplyr::rename(region_id = rgn_id, year = bd_spp_status_year)

  trend <- AlignDataYears(layer_nm="bd_spp_trend", layers_obj=layers) %>%
    dplyr::rename(region_id = rgn_id, year = bd_spp_trend_year) %>%
    dplyr::mutate(dimension = "trend")

  scores <- dplyr::bind_rows(status, trend) %>%
    dplyr::mutate(goal = "BD") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)

} ## End BD Function
