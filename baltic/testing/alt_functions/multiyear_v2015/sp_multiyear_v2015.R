
SP <- function(layers, scores){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R SP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year

  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP

  s <- scores %>%
    filter(goal %in% c("ICO", "LSP"),
           dimension %in% c("status", "trend", "future", "score")) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  return(rbind(scores, s))

} ## End SP function
