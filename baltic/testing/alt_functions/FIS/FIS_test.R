FIS = function(layers){
  ## FIS code revised by Melanie Fraser
  ## added to functions.r by Jennifer Griffiths 7 June 2016
  ## only Cod and Herrings data are used here. Sprats data moved to NP, by Ning Jiang in Oct 2016.


  ## Call Layers
  bbmsy = SelectLayersData(layers, layers='fis_bbmsy', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
                  stock = category,
                  year,
                  score= val_num) %>%
    mutate(metric ="bbmsy") %>%
    dplyr::rename(region_id = rgn_id)

  ffmsy = SelectLayersData(layers, layers='fis_ffmsy', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
                  stock = category,
                  year,
                  score= val_num) %>%
    mutate(metric= "ffmsy")%>%
    dplyr::rename(region_id = rgn_id)

  landings = SelectLayersData(layers, layers='fis_landings', narrow=T) %>%
    dplyr::select(rgn_id =id_num,
                  stock = category,
                  year,
                  landings= val_num)%>%
    dplyr::rename(region_id = rgn_id)


  ## combine bbmsy and ffmsy to single object

  metric.scores = rbind(bbmsy, ffmsy) %>%
    dplyr::select(region_id, stock, year, metric, score) %>%
    mutate(metric = as.factor(metric))%>%
    spread(metric, score)


  ###########################################################################
  ## STEP 1: converting B/Bmsy and F/Fmsy to F-scores
  ## see plot describing the relationship between these variables and scores
  ## this may need to be adjusted:
  ###########################################################################
  F_scores <- metric.scores %>%
    mutate(score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy+1.5), 0, NA),
           score = ifelse(bbmsy < 0.8 & ffmsy < (bbmsy - 0.2), ffmsy/(bbmsy-0.2), score),
           score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy + 0.2) & ffmsy < (bbmsy + 1.5), (bbmsy + 1.5 - ffmsy)/1.5, score),
           score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy - 0.2) & ffmsy < (bbmsy + 0.2), 1, score)) %>%
    mutate(score = ifelse(bbmsy >= 0.8 & ffmsy < 0.8, ffmsy/0.8, score),
           score = ifelse(bbmsy >= 0.8 & ffmsy >= 0.8 & ffmsy < 1.2, 1, score),
           score = ifelse(bbmsy >= 0.8 & ffmsy >= 1.2, (2.5 - ffmsy)/1.3, score)) %>%
    mutate(score = ifelse(score <= 0, 0.1, score)) %>%
    mutate(score_type = "F_score")
  ### NOTE: The reason the last score is 0.1 rather than zero is because
  ### scores can't be zero if using a geometric mean because otherwise, 1 zero
  ### results in a zero score.

  ###########################################################################
  ## STEP 2: converting B/Bmsy to B-scores
  ###########################################################################
  B_scores <- metric.scores %>%
    mutate(score = ifelse(bbmsy < 0.8 , bbmsy/0.8, NA),
           score = ifelse(bbmsy >= 0.8 & bbmsy < 1.5, 1, score),
           score = ifelse(bbmsy >= 1.5, (3.35 - bbmsy)/1.8, score)) %>%
    mutate(score = ifelse(score <= 0.1, 0.1, score)) %>%
    mutate(score = ifelse(score > 1, 1, score))%>%
    mutate(score_type = "B_score")

  ###########################################################################
  ## STEP 3: Averaging the F and B-scores to get the stock status score
  ###########################################################################
  status.scores <- rbind(B_scores, F_scores) %>%
    group_by(region_id, stock, year) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    data.frame()


  #############################################
  ## STEP 4: calculating the weights.
  #############################################

  ##### Subset the data to include only the most recent 10 years
  landings <- landings %>%
    dplyr::filter(year %in% (max(year)-9):max(year))

  ## we use the average catch for each stock/region across all years
  ## to obtain weights
  weights <- landings %>%
    group_by(region_id, stock) %>%
    mutate(avgCatch = mean(landings)) %>%
    ungroup() %>%
    data.frame()

  ## each region/stock will have the same average catch across years:
  #filter(weights, region_id==3)

  ## determine the total proportion of catch each stock accounts for:
  weights <- weights %>%
    group_by(region_id, year) %>%
    mutate(totCatch = sum(avgCatch)) %>%
    ungroup() %>%
    mutate(propCatch = avgCatch/totCatch)


  status <- weights %>%
    left_join(status.scores, by=c('region_id', 'year', 'stock'))%>%
    filter(!is.na(score)) %>%                    # remove missing data
    dplyr::select(region_id, year, stock, propCatch, score)        # cleaning data

  status_with_penalty <- status %>%
    mutate(scores.with.penal = ifelse(stock == "cod_2532", score*0.872,
                                      score)) %>%
    dplyr::select(-score) %>%
    dplyr::rename(score = scores.with.penal)



  status <- status_with_penalty %>%
    group_by(region_id, year) %>%
    summarize(status = prod(score^propCatch)) %>%
    ungroup() %>%
    data.frame()

  ### To get trend, get slope of regression model based on most recent 5 years
  ### of data

  trend_years <- (max(status$year)-4):max(status$year)

  trend <- status %>%
    group_by(region_id) %>%
    filter(year %in% trend_years) %>%
    do(mdl = lm(status ~ year, data=.)) %>%
    summarize(region_id = region_id,
              trend = coef(mdl)['year'] * 5) %>%  ## trend multiplied by 5 to get prediction 5 years out
    ungroup() %>%
    mutate(trend = round(trend, 2))

  ### final formatting of status data: choosing 2013 as status year coz 2014 data not reliable
  status <- status %>%
    filter(year == 2013) %>%
    # filter(year == max(year, na.rm = T)) %>%
    mutate(status = round(status * 100, 1)) %>%
    dplyr::select(region_id, status)


  ############################################################
  #####  STEP 6: Assemble dimensions
  ############################################################

  scores = status %>%
    dplyr::select(region_id,
                  score = status)%>%
    complete(region_id = full_seq(c(1,42), 1)) %>%
    mutate(dimension='status') %>%
    rbind(
      trend %>%
        dplyr::select(region_id,
                      score = trend) %>%
        complete(region_id = full_seq(c(1,42), 1)) %>%
        mutate(dimension = 'trend')) %>%
    mutate(goal='FIS')

  return(scores)

} ## End FIS function
