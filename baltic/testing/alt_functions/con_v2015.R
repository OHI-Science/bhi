CON = function(layers){

  #####----------------------######
  ## Contaminants
  #####----------------------######
  ## UDPATE 11May2016 - Jennifer Griffiths - CON ICES6 added from contaminants_prep,
  ##TODO
  ## add other CON components
  ## UPDATE 14 JULY 2016 - Jennifer Griffiths
  ## CON dioxin indicator added
  ## function mean_NAall added so that NA not NaN produced from arithmetic mean of a vector of NA
  ## UPDATE 21 JULY 2016 -Jennifer Griffiths, PFOS indicator added

  ## Function to deal with cases where want to take the arithmetic mean of a vector of NA values, will return NA instead of NaN

  mean_NAall = function(x){

    if(sum(is.na(x))==length(x)){mean_val = NA
    }else{mean_val =mean(x,na.rm=TRUE) }
    return(mean_val)
  }


  ## 3 Indicators for contaminants: ICES6, Dioxin, PFOS

  ## 3 indicators will be averaged (arithmetic mean) for status and trend (if trend for more than ICES6)

  ## status of each indicator will be multiplied by a penalty factor based on ratio: # measured / # known contaminants

  ## Penalty factor
  penalty <- layers$data[['cw_con_penalty']] %>%
    dplyr::select(-layer,
                  region_id = rgn_id,
                  penalty_factor)

  ## ICES6

  cw_con_ices6_status   = SelectLayersData(layers, layers='cw_con_ices6_status') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num) %>%
    mutate(dimension = as.character(dimension))

  cw_con_ices6_trend  = SelectLayersData(layers, layers='cw_con_ices6_trend') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num) %>%
    mutate(dimension = as.character(dimension))

  ##Join ICES6
  cw_con_ices6 = cw_con_ices6_status %>%
    rbind(cw_con_ices6_trend) %>%
    dplyr::rename(region_id = rgn_id)%>%
    mutate(indicator = "ices6")

  ##Dioxin
  cw_con_dioxin_status   = SelectLayersData(layers, layers='cw_con_dioxin_status') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num)%>%
    mutate(dimension = as.character(dimension))

  cw_con_dioxin_trend  = SelectLayersData(layers, layers='cw_con_dioxin_trend') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num)%>%
    mutate(dimension = as.character(dimension))

  ##Join dioxin
  cw_con_dioxin = full_join(cw_con_dioxin_status,cw_con_dioxin_trend, by = c('rgn_id','dimension','score')) %>%
    dplyr::rename(region_id = rgn_id)%>%
    mutate(indicator = "dioxin")

  ##PFOS
  cw_con_pfos_status   = SelectLayersData(layers, layers='cw_con_pfos_status') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num)%>%
    mutate(dimension = as.character(dimension))

  cw_con_pfos_trend  = SelectLayersData(layers, layers='cw_con_pfos_trend') %>%
    dplyr::select(rgn_id = id_num, dimension=category, score = val_num)%>%
    mutate(dimension = as.character(dimension))

  ##Join pfos
  cw_con_pfos = full_join(cw_con_pfos_status,cw_con_pfos_trend, by = c('rgn_id','dimension','score')) %>%
    dplyr::rename(region_id = rgn_id)%>%
    mutate(indicator = "pfos")

  ##Join all indicators
  cw_con = bind_rows(cw_con_ices6, cw_con_dioxin, cw_con_pfos)

  ## Average CON indicators for Status and Trend
  cw_con = cw_con %>%
    dplyr::select(-indicator) %>%
    group_by(region_id,dimension)%>%
    summarise(score = mean_NAall(score))%>% ## If there is an NA, skip over now, if all are NA, NA not NaN returned
    ungroup() %>%
    mutate(subcom = 'CON')%>%
    arrange(dimension,region_id)

  ## Add penalty factor to status scores (21March, 2017, by Ning Jiang)
  cw_con_status_with_penalty <- full_join(cw_con, penalty,
                                          by = "region_id") %>%
    filter(dimension == "status") %>%
    mutate(score2 = score * penalty_factor) %>%
    dplyr::select(region_id, dimension, subcom, score = score2)

  cw_con_full_scores <- rbind(cw_con_status_with_penalty,
                              filter(cw_con, dimension == "trend",
                                     !is.na(region_id)))

  ## create scores variable
  scores = cw_con_full_scores %>%
    mutate(goal = 'CON') %>%
    dplyr::select(goal,
                  dimension,
                  region_id,
                  score) %>%
    arrange(dimension,region_id)

  return(scores)

}  ## End CON function
