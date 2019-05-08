AO = function(layers){
  ## AO Goal has thre components stock status, access, need
  ## Currently BHI only addresses the stock  status

  ## updated 9 May 2016 Jennifer Griffiths

  ##----------------------------##
  ## STOCK STATUS SUB-COMPONENT

  ## Status is calculated in the file ao_prep.rmd because calculated on the HOLAS basin scale
  ##  and applied to BHI regions

  ## The slope of the trend is also calculated in ao_prep.rmd on the HOLAS basin scale and
  ##  applied to BHI regions
  ## the slope is not yet modified by the future year of interest (5 years from now)


  ## read in layers

  ao_stock_status = SelectLayersData(layers, layers='ao_stock_status') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num) %>%
    mutate(dimension = as.character(dimension))

  ao_stock_slope = SelectLayersData(layers, layers='ao_stock_slope') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num)%>%
    mutate(dimension = as.character(dimension))


  ## status value if NA for status
  ## NA is because there has been no data, not because not applicable
  ## decision is to leave as NA (eg not replace with 0)


  ## trend calc
  future_year = 5  ## number of years in the future for trend

  ao_stock_trend = ao_stock_slope %>%
    mutate(score = score* future_year)


  ## join status and trend
  ao_stock = bind_rows(ao_stock_status, ao_stock_trend)%>%
    dplyr::rename(region_id = rgn_id)


  ##----------------------------##
  ## OTHER sub-components

  ## if have access or need data layers, bring in here


  ##----------------------------##
  ## AO SCORE object


  scores = ao_stock %>%
    mutate(goal   = 'AO')

  return(scores)

} ## END AO function
