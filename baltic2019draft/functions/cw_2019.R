
CW <- function(layers, scores){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R CW' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year

  ## see how scores and trends are calculated in prep files (bhi-prep repository 'CW' subfolders)
  ## status is calculated as geometric mean of EUT, CON, TRA status for most recent year, same for future and score
  ## trend is the arithmetic mean of EUT, CON, TRA trends because can not deal with 0 values in geometric mean
  ## trend calculated as simple (arithmetic) mean

  ## some functions
  ## function to calculate geometric mean
  geometric.mean2 <- function(x, na.rm = TRUE){
    if(is.null(nrow(x))){
      exp(mean(log(x), na.rm = TRUE))
    } else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }
  ## function to deal with cases where want to take the arithmetic mean of a vector of NA values,
  ## will return NA instead of NaN
  mean_NAall <- function(x){
    if(sum(is.na(x)) == length(x)){
      mean_val <- NA
    } else { mean_val = mean(x, na.rm = TRUE)}

    return(mean_val)
  }

  ## calculate dimensions for CW goal, from scores subsetted to CW subgoals
  subsetscores <- scores %>%
    filter(
      goal %in% c("EUT", "TRA", "CON"),
      dimension %in% c("status", "trend", "future", "score")
    )

  cw_scores <- rbind(
    subsetscores %>%
      dplyr::filter(dimension %in% c("status", "future", "score")) %>%
      dplyr::group_by(region_id, dimension) %>%
      dplyr::summarize(score = round(geometric.mean2(score, na.rm = TRUE))) %>% # round status to 0 decimals
      ungroup(),
    subsetscores %>%
      dplyr::filter(dimension == "trend") %>%
      dplyr::group_by(region_id, dimension) %>%
      dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
      ungroup()) %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  return(rbind(scores, cw_scores))

} ## End CW function
