
LE <- function(layers, scores){

  ## From code in 'functions.R LE' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  ## average ECO and LIV dimensions to get LE scores
  scores.LE <- scores %>%
    dplyr::filter(goal %in% c("LIV", "ECO"),
                  dimension %in% c("status", "trend", "future", "score")) %>%
    reshape2::dcast(region_id + dimension ~ goal, value.var = "score") %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    mutate(goal = "LE") %>%
    dplyr::select(region_id, dimension, score) %>%
    data.frame()

  return(rbind(scores, scores.LE))

} ## End LE function
