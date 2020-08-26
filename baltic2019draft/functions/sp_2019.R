
SP <- function(layers, scores){

  ## From code in 'functions.R SP' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year


  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP

  sp_scores <- scores %>%
    dplyr::filter(
      goal %in% c("ICO", "LSP"),
      dimension %in% c("status", "trend", "future", "score"),
      year == scen_year
    ) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    mutate(goal = "SP") %>%
    ungroup() %>%
    dplyr::select(region_id, goal, dimension, score)

  return(rbind(scores, sp_scores))

} ## End SP function
