
AO = function(layers){

  ao_stock_status = SelectLayersData(layers, layers='ao_stock_status') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num) %>%
    mutate(dimension = as.character(dimension))

  ao_stock_slope = SelectLayersData(layers, layers='ao_stock_slope') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num)%>%
    mutate(dimension = as.character(dimension))

  "do this thing"
  "and now this thing"

  return(rbind(scores))

} ## End AO function
