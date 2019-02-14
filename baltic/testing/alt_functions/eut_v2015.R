EUT = function(layers){
  #####----------------------######
  ## EUT Status - Secchi + Anoxia + DIN (dissolved inorganic nitrogen) + DIP (dis. inorg. phophorus) + Chla (Chlor. A)
  #####----------------------######

  # updated 21March, 2017 by Ning Jiang

  eut_status = SelectLayersData(layers, layers='cw_eut_status') %>%
    dplyr::select(rgn_id = id_num, score = val_num) %>%
    mutate(dimension = "status")

  eut_trend  = SelectLayersData(layers, layers='cw_eut_trend') %>%
    dplyr::select(rgn_id = id_num, score = val_num) %>%
    mutate(dimension = "trend")

  # rbind eut status and trend to one dataframe
  scores =  rbind(eut_status, eut_trend) %>%
    mutate(goal = 'EUT') %>%
    dplyr::select(goal,
                  dimension,
                  region_id = rgn_id,
                  score) %>%
    arrange(dimension, region_id)

  return(scores)
  #####----------------------######

} ## End EUT function
