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
} ## End TR function
