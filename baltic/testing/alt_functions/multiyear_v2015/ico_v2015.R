
ICO <- function(layers){

  ## From code in 'functions.R ICO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep ICO subfolders)
  ## status is calculated in ico_prep.rmd because calculated by basin and then applied to BHI regions

  scen_year <- layers$data$scenario_year

  ico_status <- AlignDataYears(layer_nm="ico_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = "status", score = score*100) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  ## for now have NA iconic species trends...
  ico_trend <- data.frame(
    region_id = seq(1, 42, 1),
    score = rep(NA, 42),
    dimension = rep("trend", 42)) %>%
    dplyr::mutate(dimension = as.character(dimension))

  scores <- bind_rows(ico_status, ico_trend) %>%
    mutate(goal = "ICO") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End ICO function
