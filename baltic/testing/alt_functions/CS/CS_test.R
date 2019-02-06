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



TR = function(layers){

  ## CALCULATE STATUS ##
  ## Scores calculated in prep file (prep/TR/tr_prep.rmd) Alternative 2 based on EU blue growth report 2015
  ## updated by Ning Jiang Jan2017

  tr_status = layers$data[['tr_status']] %>%
    dplyr::select(region_id = rgn_id, score, dimension)
  tr_trend = layers$data[['tr_trend']] %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  ## create scores and rbind status and trend scores
  scores = tr_status %>%
    bind_rows(.,tr_trend) %>%
    mutate(goal='TR')

  return(scores)
  ##-------------------------------------------------------------------##
} ## end TR function

