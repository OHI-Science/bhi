ICO = function(layers){
  ## UPDATED 22 June 2016 Jennifer Griffiths

  ## Select layers

  ##
  ico_status   = SelectLayersData(layers, layers='ico_status') %>%
    dplyr::select(rgn_id = id_num, score = val_num)


  ## ICO STATUS
  ## Status is calculated in ico_prep.rmd because calculated by basin and then applied to BHI regions

  ## add dimension to object
  ico_status = ico_status %>%
    dplyr::rename(region_id =rgn_id) %>%
    mutate(dimension = 'status',
           score = score* 100)

  ## ICO TREND
  ## No data to calculate as trend, set as NA

  ico_trend = data.frame(region_id = seq(1,42,1),
                         score = rep(NA,42),
                         dimension = rep('trend',42))

  ## Return scores
  scores = bind_rows(ico_status,
                     ico_trend)%>%
    mutate(goal   = 'ICO')

  return(scores)


} ## End ICO function
