CS = function(layers){

  cs_status <- layers$data[['cs_status']] %>%
    dplyr::select(region_id = rgn_id, dimension, score) %>%
    mutate(dimension = as.character(dimension))

  cs_trend <- data.frame(region_id = seq(1,42,1),
                         dimension = rep("trend",42),
                         score = rep((NA), 42)) %>%
    mutate(dimension = as.character(dimension))

  scores = rbind(cs_status, cs_trend) %>%
    mutate(goal = 'CS')

  return(scores)
}## End CS function
