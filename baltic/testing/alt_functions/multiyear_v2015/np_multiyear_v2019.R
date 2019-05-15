
NP <- function(layers){ # NP <- function(layers, scores){

  ## From code in 'functions.R NP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## this is the same function as used for FIS, but only sprat data is used here
  ## consider updating for v2019 to include: sediment extraction, seaweeds, ornamentals, corals, shells, sponges?

  ## OHI global includes exposure, risk, sustainability estimates by product:
  ## exposure (based on habitat types), risk (from fishing activities harmfulness ratings)
  ## sustainability coefficient for each natural product: 1 - mean(c(exposure, risk))
  ## status for all production years for each region, a weighted mean of all products produced


  scen_year <- layers$data$scenario_year

  ## STEP 0: call and combine layers ----
  bbmsy <- AlignDataYears(layer_nm="np_bbmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="bbmsy") %>%
    dplyr::rename(region_id = rgn_id,
                  year = np_bbmsy_year)

  ffmsy <- AlignDataYears(layer_nm="np_ffmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="ffmsy") %>%
    dplyr::rename(region_id = rgn_id,
                  year = np_ffmsy_year)

  landings <- AlignDataYears(layer_nm="np_landings", layers_obj=layers) %>%
    dplyr::rename(region_id = rgn_id,
                  year = np_landings_year)

  ## combine bbmsy and ffmsy to single object
  metric.scores <- rbind(bbmsy, ffmsy) %>%
    dplyr::select(region_id, stock, year, scenario_year, metric, score) %>%
    mutate(metric = as.factor(metric))%>%
    spread(metric, score)

  ## STEP 1: convert F/Fmsy with B/Bmsy to F-scores ----
  ## see plot describing the relationship between these variables and scores
  ## this may need to be adjusted

  ## NOTE: The reason the last score is 0.1 rather than zero is because
  ## scores can't be zero if using a geometric mean because otherwise, 1 zero
  ## results in a zero score.

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

  ## STEP 2: convert B/Bmsy to B-scores ----

  B_scores <- metric.scores %>%
    mutate(score = ifelse(bbmsy < 0.8 , bbmsy/0.8, NA),
           score = ifelse(bbmsy >= 0.8 & bbmsy < 1.5, 1, score),
           score = ifelse(bbmsy >= 1.5, (3.35 - bbmsy)/1.8, score)) %>%
    mutate(score = ifelse(score <= 0.1, 0.1, score)) %>%
    mutate(score = ifelse(score > 1, 1, score))%>%
    mutate(score_type = "B_score")

  ## STEP 3: average F and B-scores to get the stock status score ----

  status.scores <- rbind(B_scores, F_scores) %>%
    group_by(region_id, stock, year, scenario_year) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    data.frame()

  ## STEP 4: calculate weights using landings data ----
  ## subset the data to include only the most recent 10 years
  landings <- filter(landings, year %in% (max(year)-9):max(year))

  ## we use the average catch for each stock/region across all years to obtain weights
  ## (each region/stock will have the same average catch across years)
  ## lastly, determine the total proportion of catch each stock accounts for

  weights <- landings %>%
    group_by(region_id, stock) %>%
    mutate(avgCatch = mean(landings)) %>%
    ungroup() %>%
    group_by(region_id, year, scenario_year) %>% # region/stock will have same avg catch across years
    mutate(totCatch = sum(avgCatch)) %>%
    ungroup() %>%
    mutate(propCatch = avgCatch/totCatch) %>% # total proportion of catch each stock accounts for
    data.frame()

  ## STEP 5: calculate and return status and trend ----

  status <- weights %>%
    dplyr::left_join(status.scores, by = c("region_id", "year", "scenario_year", "stock")) %>%
    dplyr::filter(!is.na(score)) %>% # remove missing data
    dplyr::select(region_id, year, scenario_year, stock, propCatch, score)

  ## take geometric mean weighted by proportion of catch in each region
  status <- status %>%
    group_by(region_id, year, scenario_year) %>%
    summarize(status = prod(score^propCatch)) %>%
    ungroup() %>%
    data.frame()

  ## could replace trend calc w/ ohicore::CalculateTrend, but then coeff is normalized by min trend year...
  ## need more years of data in scenario_data_years though for this to work
  # trend <- ohicore::CalculateTrend(status, trend_years)

  ## trend calculated as slope of regression model based on previous 5 years of data
  trend <- status %>%
    group_by(region_id) %>%
    filter(scenario_year %in% (scen_year-4):scen_year) %>% # year %in% unique(filter(status, scenario_year %in% (scen_year-4):scen_year)$year)
    do(mdl = lm(status ~ year, data=.)) %>%
    summarize(region_id = region_id,
              trend = coef(mdl)["year"] * 5) %>%  # trend multiplied by 5 to get prediction 5 years out
    ungroup() %>%
    mutate(trend = round(trend, 3)) %>%
    mutate(scenario_year = scen_year)

  ## STEP 6: assemble dimensions and final formatting ----

  ## final formatting of status data
  status <- status %>%
    filter(scenario_year == scen_year) %>%
    mutate(status = round(status * 100, 2)) %>%
    dplyr::select(region_id, status)

  scores <- status %>%
    dplyr::select(region_id, score = status)%>%
    mutate(dimension = "status") %>%
    rbind(trend %>%
            dplyr::select(region_id, score = trend) %>%
            mutate(dimension = "trend")) %>%
    mutate(goal = "NP")

  return(scores)

} ## End NP function
