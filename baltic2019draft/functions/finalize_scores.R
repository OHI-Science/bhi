
FinalizeScores <- function(layers, conf, scores){

  ## From code in 'functions.R FinalizeScores' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## modified from functions.R template to aggregte to EEZs and Subbasins of Baltic

  ## resilience and pressures for supragoals??
  supragoal_prs_res <- scores %>%
    left_join(conf$goals, by = "goal") %>%
    select("region_id", "dimension", "goal", "score", "parent", "weight") %>%
    filter(!(is.na(parent)|parent == "NA"), dimension %in% c("pressures", "resilience")) %>%
    group_by(region_id, parent, dimension) %>%
    summarize(score = weighted.mean(score, weight)) %>%
    select(region_id, dimension, goal = parent, score)

  scores <- bind_rows(scores, supragoal_prs_res)


  ## Calculate Scores for EEZs and SUBBASINS by area weighting ----

  ## regions_lookup_complete.csv does not need to be updated
  ## unless BHI regions are changed or additional subregions are created
  rgns_complete <- read.csv(file.path(dir_assess, "layers", "rgns_complete.csv")) %>%
    mutate(eez_id = case_when(
      eez == "Sweden" ~ 301,
      eez == "Denmark" ~ 302,
      eez == "Germany" ~ 303,
      eez == "Poland" ~ 304,
      eez == "Russia" ~ 305,
      eez == "Lithuania" ~ 306,
      eez == "Latvia" ~ 307,
      eez == "Estonia" ~ 308,
      eez == "Finland" ~ 309
    ))

  cat(sprintf("Calculating scores for EEZ and SUBBASIN AREAS by area weighting...\n"))


  ## For EEZs ----
  scores_eez <- scores %>%
    dplyr::filter(region_id %in% 1:42) %>%

    ## merge to the area (km2) of each region
    dplyr::left_join(rgns_complete, by = "region_id") %>%
    dplyr::group_by(goal, dimension, eez_id) %>%

    ## calculate weighted mean by area
    dplyr::summarise(score = weighted.mean(score, region_area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score)) %>%

    ## select eez ids
    dplyr::select(goal, dimension, score, region_id = eez_id)


  ## For SUBBASINS ----
  scores_subbasin <- scores %>%
    dplyr::filter(region_id %in% 1:42) %>%

    ## merge to the area (km2) of each region
    dplyr::left_join(rgns_complete, by = "region_id") %>%
    dplyr::group_by(goal, dimension, subbasin_id) %>%

    ## calculate weighted mean by area
    dplyr::summarise(score = weighted.mean(score, region_area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score)) %>%

    ## select subbasin ids
    dplyr::select(goal, dimension, score, region_id = subbasin_id)


  ## combine scores with EEZ and SUBBASIN scores ----
  scores <- bind_rows(scores, scores_eez, scores_subbasin)


  ## Add NAs to missing combos of region_id, goal, dimension ----
  explst <- list(
    region_id = c(0, 1:42, 301:309, 501:517),
    dimension = c("pressures", "resilience", "status", "trend", "future", "score"),
    goal = c(conf$goals$goal, "Index")
  )
  d <- expand.grid(explst, stringsAsFactors = FALSE)


  ## RETURN SCORES ----
  ## merge NAs dataframe with scores, and arrange
  scores <- dplyr::left_join(d, scores, by = c("goal",  "dimension", "region_id")) %>%
    dplyr::arrange(goal, dimension, region_id) %>%
    dplyr::mutate(score = ifelse(dimension == "trend", round(score, 3), round(score, 1))) %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score))


  return(scores)

} ## End FinalizeScores function
