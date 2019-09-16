## Libraries
source(file.path(here::here(), "R", "setup.R"))
library(ohicore)


## Functions

#' calculate BHI scores for each scenario year and save to a single csv file
#'
#' @param dir_assess filepath to the current assessment directory, an immediate subdirectory of the project root folder; set in `setup.R`
#' @param scenario_yrs which years scores are to be calculated for
#' @param scores_path the directory where to save the scores.csv, by default in the assessment (dir_assess) folder
#'
#' @return OHI scores

calculate_scores <- function(dir_assess, scenario_yrs, scores_path = ".", write_scores = TRUE){

  currentwd <- getwd()
  setwd(dir_assess)

  ## load scenario configuration
  conf <- ohicore::Conf("conf")

  ## check that scenario layers files in the \layers folder match layers.csv registration
  ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)

  ## load scenario layers for ohicore to access
  layers <- ohicore::Layers("layers.csv", "layers")

  scorelist <- lapply(scenario_yrs, function(yr){
    print(sprintf("For assessment year %s", yr))
    layers$data$scenario_year <- yr
    scores_scenario_year <- ohicore::CalculateAll(conf, layers) %>%
      dplyr::mutate(year = yr)
    }) %>%
    dplyr::bind_rows()

  ## write csv by default to 'dir_assess'
  if(is.TRUE(write_scores)){
    readr::write_csv(
      scorelist,
      sprintf("%s/scores.csv", scores_path),
      na = ""
    )
  }

  setwd(currentwd)
  return(scorelist)
}
