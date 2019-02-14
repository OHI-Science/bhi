FinalizeScores = function(layers, conf, scores){

  ## modified from original to aggregte to EEZs and SUBBASINs
  ## @jules32 September 2016


  ## Regions to aggregate as eezs and basins

  source('prep/create_rgns_lookup.R')

  ## complete dataframe
  rgns_complete <- read.csv('spatial/regions_lookup_complete.csv', stringsAsFactors = FALSE); head(rgns_complete)

  ## subset only eez, subbasin ids
  rgns_eez_subbasin <- rgns_complete %>%
    filter(type %in% c('eez', 'subbasin'))

  ## subset excluding bhi regions
  rgns_aggregate = rgns_complete %>%
    filter(!type %in% 'bhi')


  ## ---- Calculate Scores for EEZs and SUBBASINS by area weighting ----
  cat(sprintf("Calculating scores for EEZ and SUBBASIN AREAS by area weighting...\n"))

  ## EEZs
  scores_eez <- scores %>%

    # filter only score, status, future dimensions, merge to the area (km2) of each region
    dplyr::filter(dimension %in% c('score','status','future'),
                  region_id != 0) %>%
    left_join(rgns, by = 'region_id') %>%

    # calculate weighted mean by area
    dplyr::group_by(goal, dimension, eez_id) %>%
    dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::select(goal, dimension, score, region_id = eez_id)

  ## SUBBASINS
  scores_subbasin <- scores %>%

    # filter only score, status, future dimensions, merge to the area (km2) of each region
    dplyr::filter(dimension %in% c('score','status','future'),
                  region_id != 0) %>%
    left_join(rgns, by = 'region_id') %>%

    # calculate weighted mean by area
    dplyr::group_by(goal, dimension, subbasin_id) %>%
    dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::select(goal, dimension, score, region_id = subbasin_id)

  ## combine scores with EEZ and SUBBASIN scores
  scores = bind_rows(scores, scores_eez, scores_subbasin)


  ## add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns_complete$region_id),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id %in% rgns_aggregate$region_id) &
               !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))

  ## Merge with scores, and arrange
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')] %>%
    arrange(goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
} ## End FinalizeScores function
