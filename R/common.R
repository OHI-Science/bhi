## Libraries
library(tidyverse)
library(here)
library(tools)
library(broom)
library(ohicore)

## Directories
dir_bhi <- here::here()
dir_R <- file.path(dir_bhi, "R")

## Functions

#' calculate BHI scores for each scenario year and save to a single csv file
#'
#' @param assessment_path the bhi repository 'baltic' subfolder, wherever that is on your local computer
#' @param scenario_yrs which years scores are to be calculated for
#' @param scores_path the directory where to save the scores.csv, by default in the 'baltic' (assessment) folder
#'
#' @return

calculate_scores <- function(assessment_path, scenario_yrs, scores_path = "."){

  currentwd <- getwd()
  setwd(assessment_path)

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
      dplyr::mutate(year = yr)}) %>% dplyr::bind_rows()

  ## write csv by default to 'assessment_path'
  readr::write_csv(scorelist,
                   sprintf("%s/scores.csv", scores_path),
                   na = "")

  setwd(currentwd)
}
