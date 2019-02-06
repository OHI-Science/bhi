ECO = function(layers){

  eco_status = layers$data[['eco_status']] %>%
    mutate(dimension = 'status') %>%
    dplyr::select(-layer)

  eco_trend = layers$data[['eco_trend']] %>%
    mutate(dimension = 'trend') %>%
    dplyr::select(-layer)

  ## create scores and rbind to other goal scores
  scores = rbind(eco_status, eco_trend) %>%
    mutate(goal='ECO') %>%
    dplyr::select(region_id = rgn_id,
                  score,
                  dimension,
                  goal)

  return(scores)

} ## End ECO function
