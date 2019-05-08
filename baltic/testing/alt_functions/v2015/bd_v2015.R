BD = function(layers){
  ## BD goal now has no subgoals; it is only species.
  ## Updated by Jennifer Griffiths 7 July 2016
  ## BD status calculated in spp_prep file because calculated by basin and then applied to BHI regions
  ## Status is the geometric mean of each taxa group status by basin
  ## Trend is temporarily substituted by OHI-Global 2016 BD trend scores


  ## Call Layers
  ## status
  status = SelectLayersData(layers, layers='bd_spp_status', narrow=T) %>%
    dplyr::select(region_id = id_num,
                  dimension = category,
                  score= val_num) %>%
    mutate(score = round(score*100))

  ##trend
  trend = layers$data[['bd_spp_trend']] %>%
    dplyr::select(region_id = rgn_id, score) %>%
    mutate(dimension = "trend")

  # scores
  scores = bind_rows(status, trend)%>%
    mutate(goal = 'BD')

  return(scores)
} ## End BD Function
