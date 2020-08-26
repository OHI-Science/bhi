
LE <- function(layers, scores){

  ## From code in 'functions.R LE' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year

  le_scores <- scores %>%
    dplyr::filter(
      goal %in% c("LIV", "ECO"),
      dimension %in% c("status", "trend", "future", "score"),
      year == scen_year
    ) %>%
    # reshape2::dcast(region_id + dimension ~ goal, value.var = "score") %>%
    # mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score)

  return(rbind(scores, le_scores))

} ## End LE function
