## Note that some of the commands used below are from older R packages that we don't recommend using anymore (dcast, etc). Instead, use
## dplyr and tydr packages that have more streamlined functions to manipulate data. To learn those funtions:
## http://ohi-science.org/manual/#appendix-5-r-tutorials-for-ohi

## See functions.R of CHN (OHI-China) for how those functions are used in OHI+ assessments



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
