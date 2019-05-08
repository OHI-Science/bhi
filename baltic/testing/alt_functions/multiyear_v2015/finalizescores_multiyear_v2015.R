
FinalizeScores <- function(layers, conf, scores){

  ## From code in 'functions.R FinalizeScores' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## modified from functions.R template to aggregte to EEZs and Subbasins of Baltic

  ## Regions to aggregate as EEZs and Basins

  if(!file.exists(file.path(here::here(), "spatial", "regions_lookup_complete.csv"))){
    source(file.path(here::here(), "R", "spatial.R"))
    create_rgn_lookup(here::here(), layers, conf) # creates regions complete lookup
  }
  ## regions_lookup_complete.csv need not be updated unless BHI regions are changed or additional subregions are created
  rgns_complete <- read.csv(file.path(here::here(), "spatial", "regions_lookup_complete.csv"), # complete regions df in bhi/spatial
                            stringsAsFactors = FALSE)

  rgns_eez_subbasin <- rgns_complete %>%
    dplyr::filter(type %in% c("eez", "subbasin")) # subset to eez and subbasin ids
  rgns_aggregate <- rgns_complete %>%
    dplyr::filter(type %in% c("eez", "subbasin", "GLOBAL")) # exclude bhi rgns; eez, subbasin, and global

  ## Calculate Scores for EEZs and SUBBASINS by area weighting
  cat(sprintf("Calculating scores for EEZ and SUBBASIN AREAS by area weighting...\n"))

  ## For EEZs
  scores_eez <- scores %>%
    dplyr::filter(dimension %in% c("score", "status", "future"), region_id != 0) %>%
    dplyr::left_join(rgns, by = "region_id") %>% # merge to the area (km2) of each region
    dplyr::group_by(goal, dimension, eez_id) %>% # weighted mean by area
    dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(goal, dimension, score, region_id = eez_id) # select eez ids

  ## For SUBBASINS
  scores_subbasin <- scores %>%
    dplyr::filter(dimension %in% c("score", "status", "future"), region_id != 0) %>% # filter to score, status, future dims
    dplyr::left_join(rgns, by = "region_id") %>% # merge to the area (km2) of each region
    dplyr::group_by(goal, dimension, subbasin_id) %>%
    dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm = TRUE)) %>% # calculate weighted mean by area
    dplyr::ungroup() %>%
    dplyr::select(goal, dimension, score, region_id = subbasin_id) # select subbasin ids

  ## combine scores with EEZ and SUBBASIN scores
  scores <- bind_rows(scores, scores_eez, scores_subbasin)

  ## add NAs to missing combos of region_id, goal, dimension
  d <- expand.grid(list(
    score_NA = NA,
    region_id = c(rgns_complete$region_id),
    dimension = c("pressures", "resilience", "status", "trend", "future", "score"),
    goal = c(conf$goals$goal, "Index")
  ),
  stringsAsFactors = FALSE) %>%
    subset(
      !(dimension %in% c("pressures", "resilience", "trend") &
          region_id %in% rgns_aggregate$region_id) &
      !(dimension %in% c("pressures", "resilience", "status", "trend") &
          goal == "Index"))

  scores <- merge(scores, d, all = TRUE)[, c("goal", "dimension", "region_id", "score")] %>%
    dplyr::arrange(goal, dimension, region_id) # merge NAs dataframe with scores, and arrange
  scores$score <- round(scores$score, 2) # round scores

  return(scores)
} ## End FinalizeScores function
