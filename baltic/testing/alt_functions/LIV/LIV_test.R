

LIV = function(layers){


  liv_status = layers$data[['liv_status']] %>%
    dplyr::select(region_id = rgn_id, score) %>%
    mutate(dimension = "status")

  liv_trend = layers$data[['liv_trend']] %>%
    dplyr::select(region_id = rgn_id, score) %>%
    mutate(dimension = "trend")

  scores = liv_status %>%
    bind_rows(.,liv_trend) %>%
    mutate(goal='LIV')

  return(scores)

} ## End LIV function
