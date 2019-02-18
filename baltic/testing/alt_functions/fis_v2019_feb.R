
FIS = function(layers){

  ## Based on code from v2015 BHI assessment
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  bbmsy <- AlignDataYears(layer_nm="fis_bbmsy", layers_obj=layers) %>%
    mutate(metric="bbmsy") %>%
    dplyr::rename(region_id = rgn_id)

  ffmsy <- AlignDataYears(layer_nm="fis_ffmsy", layers_obj=layers) %>%
    mutate(metric="ffmsy") %>%
    dplyr::rename(region_id = rgn_id)

  landings <- AlignDataYears(layer_nm="fis_landings", layers_obj=layers) %>%
    dplyr::rename(region_id = rgn_id)


  ## combine bbmsy and ffmsy into single object

  metric_scores <- rbind(bbmsy %>% dplyr::select(-fis_bbmsy_year),
                         ffmsy %>% dplyr::select(-fis_ffmsy_year)) %>%
    dplyr::select(-layer_name, year = scenario_year) %>%
    dplyr::mutate(metric = as.factor(metric)) %>%
    spread(metric, score)


  ## STEP 1: converting B/Bmsy and F/Fmsy to F-scores

  ## see plot describing the relationship between these variables and scores
  ## this may need to be adjusted

  F_scores <- metric_scores %>%
    mutate(score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy + 1.5), 0, NA),
           score = ifelse(bbmsy < 0.8 & ffmsy < (bbmsy - 0.2), ffmsy/(bbmsy - 0.2), score),
           score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy + 0.2) & ffmsy < (bbmsy + 1.5), (bbmsy + 1.5 - ffmsy)/1.5, score),
           score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy - 0.2) & ffmsy < (bbmsy + 0.2), 1, score)) %>%
    mutate(score = ifelse(bbmsy >= 0.8 & ffmsy < 0.8, ffmsy/0.8, score),
           score = ifelse(bbmsy >= 0.8 & ffmsy >= 0.8 & ffmsy < 1.2, 1, score),
           score = ifelse(bbmsy >= 0.8 & ffmsy >= 1.2, (2.5 - ffmsy)/1.3, score)) %>%
    mutate(score = ifelse(score <= 0, 0.1, score)) %>%
    mutate(score_type = "F_score")

  ## note: last score is 0.1 rather than zero because
  ## if have one zero w/ geometric mean, results in a zero score


  ## STEP 2: converting B/Bmsy to B-scores

  B_scores <- metric_scores %>%
    mutate(score = ifelse(bbmsy < 0.8 , bbmsy/0.8, NA),
           score = ifelse(bbmsy >= 0.8 & bbmsy < 1.5, 1, score),
           score = ifelse(bbmsy >= 1.5, (3.35 - bbmsy)/1.8, score)) %>%
    mutate(score = ifelse(score <= 0.1, 0.1, score)) %>%
    mutate(score = ifelse(score > 1, 1, score))%>%
    mutate(score_type = "B_score")


  ## STEP 3: averaging the F and B-scores to get the stock status score

  status_scores <- rbind(B_scores, F_scores) %>%
    group_by(region_id, stock, year) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    data.frame()


  ## STEP 4: calculating the weights

  ## subset the data to include only the most recent 10 years
  landings <- landings %>%
    dplyr::select(year = scenario_year, region_id, stock, landings) %>%
    dplyr::filter(year %in% (max(year)-9):max(year))

  ## we use average catch for each stock/region across all yrs to obtain weights
  weights <- landings %>%
    group_by(region_id, stock) %>%
    mutate(avgCatch = mean(landings)) %>%
    ungroup() %>%
    data.frame()

  ## each region/stock will have the same average catch across years
  ## determine the total proportion of catch each stock accounts for
  weights <- weights %>%
    group_by(region_id, year) %>%
    mutate(totCatch = sum(avgCatch)) %>%
    ungroup() %>%
    mutate(propCatch = avgCatch/totCatch)


  ## STEP 5: join scores and weights to calculate status

  status <- weights %>%
    left_join(status_scores, by=c("region_id", "year", "stock")) %>%
    filter(!is.na(score)) %>% # remove missing data
    dplyr::select(region_id, year, stock, propCatch, score) # clean data

  ##### becaue of bad cod condition in Eastern Baltic(ICES_subdivision = 2532),
  ##### we added a penalty factor of 0.872 based on historical cod body weight
  ##### see FIS prep for full calculation of the penalty factor
  ##### by Ning Jiang, 16 Feb, 2017
  status_with_penalty <- status %>%
    mutate(scores_with_penalty = ifelse(stock == "cod_2532", score*0.872, score)) %>%
    dplyr::select(-score) %>%
    dplyr::rename(score = scores_with_penalty)

  status <- status_with_penalty %>%
    group_by(region_id, year) %>%
    summarize(status = prod(score^propCatch)) %>%
    ungroup() %>%
    data.frame()

  ## for trend, get slope of regression model based on most recent 5 yrs of data
  trend_years <- (scen_year - 4):(scen_year)

  trend <- status %>%
    group_by(region_id) %>%
    filter(year %in% trend_years) %>%
    do(mdl = lm(status ~ year, data = .)) %>%
    summarize(region_id = region_id,
              trend = coef(mdl)["year"] * 5) %>%  # trend multiplied by 5 to predict 5 yrs out
    ungroup() %>%
    mutate(trend = round(trend, 2))

  ## could replace trend calc w/ ohicore::CalculateTrend, but then coeff is normalized by min trend year
  ## need more years of data in scenario_data_years though for this to work
  # trend <- ohicore::CalculateTrend(status, trend_years)

  status <- status %>%
    filter(year == scen_year) %>%
    mutate(status = round(status * 100, 1)) %>%
    dplyr::select(region_id, status)


  ## STEP 6: assemble dimensions

  scores <- status %>%
    dplyr::select(region_id, score = status) %>%
    tidyr::complete(region_id = full_seq(c(1, 42), 1)) %>%
    dplyr::mutate(dimension = "status") %>%
    rbind(
      trend %>%
        dplyr::select(region_id,
                      score = trend) %>%
        tidyr::complete(region_id = full_seq(c(1,42), 1)) %>%
        dplyr::mutate(dimension = "trend")) %>%
    dplyr::mutate(goal = "FIS")

  return(scores)

} ## End FIS function
