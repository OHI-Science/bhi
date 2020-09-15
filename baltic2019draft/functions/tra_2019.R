
TRA <- function(layers){

  ## From code in 'functions.R TRA' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  tra_status <- ohicore::AlignDataYears(layer_nm="cw_tra_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score)


  ## Trend ----

  tra_trend <- ohicore::AlignDataYears(layer_nm="cw_tra_trend_scores", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score)


  ## Return trash status and trend scores ----

  tra_status_and_trend <- dplyr::bind_rows(
    mutate(tra_status, dimension = "status", goal = "TRA"),
    mutate(tra_trend, dimension = "trend", goal = "TRA")
  ) %>%
  ## region 36 is archipelago sea not åland subbasin;
  ## because of issues with aggregating CON and EUT, will not include region 36 here
  ## (do not want region 36 to have Clean Waters score based only on Trash subgoal)
  ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
  mutate(score = ifelse(region_id == 36, NA, score))

  scores <- select(tra_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End TRA function
