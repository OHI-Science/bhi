TRA = function(layers){
  #####----------------------######
  ## Trash
  #####----------------------######

  ## UPDATE 14NOV2016 by Ning Jiang
  ## Status calcuated in prep file
  ## reference points set and calculated in /prep/CW/trash/trash_prep.rmd

  tra_status = layers$data[['cw_tra_status']] %>%
    dplyr::select(-layer,
                  region_id = rgn_id) %>%
    mutate(dimension = 'status')

  tra_trend = layers$data[['cw_tra_trend']] %>%
    dplyr::select(-layer,
                  region_id = rgn_id) %>%
    mutate(dimension = 'trend')

  ## create scores variable
  scores = rbind(tra_status, tra_trend) %>%
    mutate(goal = 'TRA')

  return(scores)
} ## END TRA function
