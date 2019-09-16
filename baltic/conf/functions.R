FinalizeScores = function(layers, conf, scores){

  ## modified from original to aggregte to EEZs and SUBBASINs
  ## @jules32 September 2016


  ## Regions to aggregate as eezs and basins

  source('prep/create_rgns_lookup.R')

  ## complete dataframe
  rgns_complete <- read.csv('spatial/regions_lookup_complete.csv', stringsAsFactors = FALSE); head(rgns_complete)

  ## subset only eez, subbasin ids
  rgns_eez_subbasin <- rgns_complete %>%
    filter(type %in% c('eez', 'subbasin'))

  ## subset excluding bhi regions
  rgns_aggregate = rgns_complete %>%
    filter(!type %in% 'bhi')


  ## ---- Calculate Scores for EEZs and SUBBASINS by area weighting ----
  cat(sprintf("Calculating scores for EEZ and SUBBASIN AREAS by area weighting...\n"))

  ## EEZs
  scores_eez <- scores %>%

    # filter only score, status, future dimensions, merge to the area (km2) of each region
    dplyr::filter(dimension %in% c('score','status','future'),
                  region_id != 0) %>%
    left_join(rgns, by = 'region_id') %>%

    # calculate weighted mean by area
    dplyr::group_by(goal, dimension, eez_id) %>%
    dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::select(goal, dimension, score, region_id = eez_id)

  ## SUBBASINS
  scores_subbasin <- scores %>%

    # filter only score, status, future dimensions, merge to the area (km2) of each region
    dplyr::filter(dimension %in% c('score','status','future'),
                  region_id != 0) %>%
    left_join(rgns, by = 'region_id') %>%

    # calculate weighted mean by area
    dplyr::group_by(goal, dimension, subbasin_id) %>%
    dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::select(goal, dimension, score, region_id = subbasin_id)

  ## combine scores with EEZ and SUBBASIN scores
  scores = bind_rows(scores, scores_eez, scores_subbasin)


  ## add NAs to missing combos (region_id, goal, dimension)
  d = expand.grid(list(score_NA  = NA,
                       region_id = c(rgns_complete$region_id),
                       dimension = c('pressures','resilience','status','trend','future','score'),
                       goal      = c(conf$goals$goal, 'Index')), stringsAsFactors = FALSE); head(d)
  d = subset(d,
             !(dimension %in% c('pressures','resilience','trend') & region_id %in% rgns_aggregate$region_id) &
               !(dimension %in% c('pressures','resilience','status','trend') & goal=='Index'))

  ## Merge with scores, and arrange
  scores = merge(scores, d, all=T)[,c('goal','dimension','region_id','score')] %>%
    arrange(goal, dimension, region_id)

  # round scores
  scores$score = round(scores$score, 2)

  return(scores)
}

AO <- function(layers){

  ## From code in 'functions.R AO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  ## the AO goal defined by the OHI framework has 3 components: stock status, access, need
  ## currently the BHI only addresses the stock status

  ## STOCK STATUS ----
  ## status and trend slope are calculated in the ao_prep of the bhi-prep repo
  ## calculated on the HOLAS basin scale and applied to the BHI regions

  ao_stock_status <- AlignDataYears(layer_nm="ao_stock_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(scenario_year, rgn_id, dimension, score)
  ## status as NA indicates missing data, not that it is not applicable...

  ao_stock_slope <- AlignDataYears(layer_nm="ao_stock_slope", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(scenario_year, rgn_id, dimension, score)


  ## TREND ----
  ## the trend slope is not yet modified by the future year of interest (5 years from now)

  trend_n_years <- 5  # number of years for trend

  ao_stock_trend <- ao_stock_slope %>%
    dplyr::mutate(score = score * trend_n_years)


  ## join status and trend
  ao_stock <- dplyr::bind_rows(ao_stock_status, ao_stock_trend) %>%
    dplyr::rename(region_id = rgn_id)


  ## SUBCOMPONENTS ----
  ## in future check whether data is available for other sub-componenets!
  ## if have access or need data layers, incorporate following example of ohi-global:
  # sustainability <- 1.0
  # r <- AlignDataYears(layer_nm="ao_access", layers_obj=layers) %>%
  #   dplyr::rename(region_id = rgn_id, access = value) %>%
  #   na.omit()
  #
  # ry <- AlignDataYears(layer_nm="ao_need", layers_obj=layers) %>%
  #   dplyr::left_join(r, by = c("region_id", "scenario_year")) %>%
  #   dplyr::mutate(Du = (1 - need) * (1 - access)) %>%
  #   dplyr::mutate(status = (1 - Du) * sustainability * 100)
  #
  # status <- ry %>%
  #   dplyr::filter(scenario_year == scen_year) %>%
  #   dplyr::select(region_id, score = status) %>%
  #   dplyr::mutate(dimension = "status")
  #
  # trend_years <- (scen_year - 4):(scen_year)
  # trend <- ohicore::CalculateTrend(status_data = ry, trend_years = trend_years)
  #
  # scores <- rbind(status, trend) %>%
  #   dplyr::mutate(goal = "AO")


  ## RETURN SCORES ----
  scores <- ao_stock %>%
    dplyr::select(region_id, dimension, score) %>%
    dplyr::mutate(goal = "AO") %>%
    dplyr::arrange(goal, region_id)

  return(scores)

} ## End AO function

BD <- function(layers){

  ## From code in 'functions.R BD' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## BD goal currently has no subgoals; it is only species
  ## status calculated in spp_prep file because calculated by basin and then applied to BHI regions
  ## status is the geometric mean of each taxa group status by basin
  ## trend is temporarily substituted by OHI-global BD trend scores

  scen_year <- layers$data$scenario_year

  ## call status and trend layers ----
  status <- AlignDataYears(layer_nm="bd_spp_status", layers_obj=layers) %>%
    dplyr::mutate(score = round(score*100),
           dimension = as.character(dimension)) %>%
    dplyr::rename(region_id = rgn_id, year = bd_spp_status_year)

  trend <- AlignDataYears(layer_nm="bd_spp_trend", layers_obj=layers) %>%
    dplyr::rename(region_id = rgn_id, year = bd_spp_trend_year) %>%
    dplyr::mutate(dimension = "trend")

  scores <- dplyr::bind_rows(status, trend) %>%
    dplyr::mutate(goal = "BD") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)

} ## End BD Function

CON <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R CON' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, 'CW/contaminants' subfolders)

  ## there are 3 indicators for contaminants: ICES6, Dioxin, PFOS
  ## the 3 indicators will be averaged (arithmetic mean) for status and trend (if trend for more than ICES6)
  ## status of each indicator will be multiplied by a penalty factor based on ratio: # measured / # known contaminants

  ## a function to deal with cases where want to take the arithmetic mean of a vector of NA values, will return NA instead of NaN ----
  mean_NAall <- function(x){ # defined also in clean water goal function...
    if(sum(is.na(x)) == length(x)){
      mean_val <- NA
    } else { mean_val = mean(x, na.rm = TRUE)}

    return(mean_val)
  }

  ## penalty factor ----
  penalty <- AlignDataYears(layer_nm="cw_con_penalty", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, penalty_factor)


  ## collect and join 3 CON indicator datasets ----
  ## call and join ICES6 datasets
  cw_con_ices6_status <- AlignDataYears(layer_nm="cw_con_ices6_status", layers_obj=layers) %>% # maybe cw_con_ices_status in future...
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension) %>%
    dplyr::mutate(dimension = as.character(dimension))

  cw_con_ices6_trend <- AlignDataYears(layer_nm="cw_con_ices6_trend", layers_obj=layers) %>% # maybe cw_con_ices_trend in future...
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension) %>%
    dplyr::mutate(dimension = as.character(dimension))

  cw_con_ices6 <- rbind(cw_con_ices6_status, cw_con_ices6_trend) %>%
    mutate(indicator = "ices6")

  ## call and join Dioxin datasets
  cw_con_dioxin_status <- AlignDataYears(layer_nm="cw_con_dioxin_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension) %>%
    dplyr::mutate(dimension = as.character(dimension))

  cw_con_dioxin_trend <- AlignDataYears(layer_nm="cw_con_dioxin_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension) %>%
    dplyr::mutate(dimension = as.character(dimension))

  cw_con_dioxin <- full_join(
    cw_con_dioxin_status,
    cw_con_dioxin_trend,
    by = c("region_id", "dimension", "score")) %>%
    mutate(indicator = "dioxin")

  ## call and join PFOS datasets
  cw_con_pfos_status <- AlignDataYears(layer_nm="cw_con_pfos_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension) %>%
    dplyr::mutate(dimension = as.character(dimension))

  cw_con_pfos_trend <- AlignDataYears(layer_nm="cw_con_pfos_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension) %>%
    dplyr::mutate(dimension = as.character(dimension))

  cw_con_pfos <- full_join(
    cw_con_pfos_status,
    cw_con_pfos_trend,
    by = c("region_id", "dimension", "score")) %>%
    mutate(indicator = "pfos")

  ## join all indicators
  cw_con <- dplyr::bind_rows(cw_con_ices6, cw_con_dioxin, cw_con_pfos)


  ## average CON indicators for status and trend ----
  cw_con <- cw_con %>%
    dplyr::select(-indicator) %>%
    group_by(region_id, dimension) %>%
    summarise(score = mean_NAall(score)) %>% # if there is an NA, skip over now, if all are NA, NA not NaN returned
    ungroup() %>%
    mutate(subcom = "CON") %>%
    arrange(dimension, region_id)

  ## incorporate penalty factor to status scores ----
  cw_con_status_with_penalty <- full_join(cw_con, penalty, by = "region_id") %>%
    filter(dimension == "status") %>%
    mutate(score2 = score*penalty_factor) %>%
    dplyr::select(region_id, dimension, subcom, score = score2)

  ## create and return scores object ----
  scores <- rbind(cw_con_status_with_penalty,
                  filter(cw_con, dimension == "trend", !is.na(region_id))) %>%
    dplyr::mutate(goal = "CON") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    dplyr::arrange(dimension, region_id)

  return(scores)

} ## End CON Function

CS <- function(layers){

  ## From code in 'functions.R CS' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  cs_status <- AlignDataYears(layer_nm="cs_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  ## for now NA carbon storage trends...
  cs_trend <- data.frame(
    region_id = seq(1, 42, 1),
    score = rep(NA, 42),
    dimension = rep("trend", 42)) %>%
    dplyr::mutate(dimension = as.character(dimension))

  scores <- rbind(cs_status, cs_trend) %>%
    mutate(goal = "CS")

  return(scores)

} ## End CS function

CW <- function(layers, scores){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R CW' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year

  ## see how scores and trends are calculated in prep files (bhi-prep repository 'CW' subfolders)
  ## status is calculated as geometric mean of EUT, CON, TRA status for most recent year, same for future and score
  ## trend in the arithmetic mean of EUT, CON, TRA trend because can not deal with 0 values in geometric mean
  ## trend calculated as simple (arithmetic) mean

  ## some functions
  ## function to calculate geometric mean
  geometric.mean2 <- function(x, na.rm = TRUE){
    if(is.null(nrow(x))){
      exp(mean(log(x), na.rm = TRUE))
    } else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }
  ## function to deal with cases where want to take the arithmetic mean of a vector of NA values, will return NA instead of NaN
  mean_NAall <- function(x){
    if(sum(is.na(x)) == length(x)){
      mean_val <- NA
    } else { mean_val = mean(x, na.rm = TRUE)}

    return(mean_val)
  }

  ## calculate dimensions for CW goal, from scores subsetted to CW subgoals
  scores_cw <- scores %>%
    filter(goal %in% c("EUT", "TRA", "CON"),
           year == scen_year) %>%
    arrange(dimension, region_id)

  s <- rbind(
    scores_cw %>%
      dplyr::filter(dimension %in% c("status", "future", "score")) %>%
      dplyr::group_by(region_id, dimension, year) %>%
      dplyr::summarize(score = round(geometric.mean2(score, na.rm = TRUE))) %>% # round status to 0 decimals
      ungroup(),
    scores_cw %>%
      dplyr::filter(dimension %in% c("trend")) %>%
      dplyr::group_by(region_id, dimension, year) %>%
      dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
      ungroup()) %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  return(rbind(scores, s))

} ## End CW function

ECO <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R ECO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, prep 'ECO' subfolder)

  eco_status <- AlignDataYears(layer_nm="eco_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  eco_trend <- AlignDataYears(layer_nm="eco_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- rbind(eco_status, eco_trend) %>%
    mutate(goal = "ECO") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End ECO function

EUT <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R EUT' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, 'CW/eutrophication' subfolders)
  ## status: Secchi + Anoxia + DIN (dissolved inorganic nitrogen) + DIP (dis. inorg. phophorus) + Chla (Chlor. A)

  eut_status <- AlignDataYears(layer_nm="cw_eut_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  eut_trend <- AlignDataYears(layer_nm="cw_eut_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  scores <- rbind(eut_status, eut_trend) %>%
    mutate(goal = "EUT") %>%
    select(region_id, goal, dimension, score) %>%
    arrange(dimension, region_id)

  return(scores)

} ## End EUT function

FIS <- function(layers){

  ## From code in 'functions.R FIS' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  bbmsy <- AlignDataYears(layer_nm="fis_bbmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="bbmsy") %>%
    dplyr::rename(region_id = rgn_id)

  ffmsy <- AlignDataYears(layer_nm="fis_ffmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="ffmsy") %>%
    dplyr::rename(region_id = rgn_id)

  landings <- AlignDataYears(layer_nm="fis_landings", layers_obj=layers) %>%
    dplyr::rename(region_id = rgn_id)


  ## combine bbmsy and ffmsy into single object

  metric_scores <- rbind(bbmsy %>% dplyr::select(-fis_bbmsy_year),
                         ffmsy %>% dplyr::select(-fis_ffmsy_year)) %>%
    dplyr::select(-layer_name, year = scenario_year) %>%
    dplyr::mutate(metric = as.factor(metric)) %>%
    tidyr::spread(metric, score)


  ## STEP 1: converting B/Bmsy and F/Fmsy to F-scores ----

  ## see plot describing the relationship between these variables and scores
  ## this may need to be adjusted...

  F_scores <- metric_scores %>%
    dplyr::mutate(score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy + 1.5), 0, NA),
                  score = ifelse(bbmsy < 0.8 & ffmsy < (bbmsy - 0.2),
                                 ffmsy/(bbmsy - 0.2),
                                 score),
                  score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy + 0.2) & ffmsy < (bbmsy + 1.5),
                                 (bbmsy + 1.5 - ffmsy)/1.5,
                                 score),
                  score = ifelse(bbmsy < 0.8 & ffmsy >= (bbmsy - 0.2) & ffmsy < (bbmsy + 0.2), 1, score)) %>%

    dplyr::mutate(score = ifelse(bbmsy >= 0.8 & ffmsy < 0.8, ffmsy/0.8, score),
                  score = ifelse(bbmsy >= 0.8 & ffmsy >= 0.8 & ffmsy < 1.2, 1, score),
                  score = ifelse(bbmsy >= 0.8 & ffmsy >= 1.2, (2.5 - ffmsy)/1.3, score)) %>%

    dplyr::mutate(score = ifelse(score <= 0, 0.1, score)) %>%
    dplyr::mutate(score_type = "F_score")

  ## note: last score is 0.1 rather than zero because
  ## if have one zero w/ geometric mean, results in a zero score


  ## STEP 2: converting B/Bmsy to B-scores ----

  B_scores <- metric_scores %>%
    dplyr::mutate(score = ifelse(bbmsy < 0.8 , bbmsy/0.8, NA),
                  score = ifelse(bbmsy >= 0.8 & bbmsy < 1.5, 1, score),
                  score = ifelse(bbmsy >= 1.5, (3.35 - bbmsy)/1.8, score)) %>%

    dplyr::mutate(score = ifelse(score <= 0.1, 0.1, score)) %>%
    dplyr::mutate(score = ifelse(score > 1, 1, score))%>%

    dplyr::mutate(score_type = "B_score")


  ## STEP 3: averaging the F and B-scores to get the stock status score ----

  status_scores <- rbind(B_scores, F_scores) %>%
    dplyr::group_by(region_id, stock, year) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    data.frame()


  ## STEP 4: calculating the weights ----

  ## subset the data to include only the most recent 10 years
  landings <- landings %>%
    dplyr::select(year = scenario_year, region_id, stock, landings) %>%
    dplyr::filter(year %in% (max(year)-9):max(year))

  ## we use average catch for each stock/region across all yrs to obtain weights
  weights <- landings %>%
    dplyr::group_by(region_id, stock) %>%
    dplyr::mutate(avgCatch = mean(landings)) %>%
    dplyr::ungroup() %>%
    data.frame()

  ## each region/stock will have the same average catch across years
  ## determine the total proportion of catch each stock accounts for
  weights <- weights %>%
    dplyr::group_by(region_id, year) %>%
    dplyr::mutate(totCatch = sum(avgCatch)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(propCatch = avgCatch/totCatch)


  ## STEP 5: alculate status and trend ----

  ## join scores and weights to calculate status
  status <- weights %>%
    dplyr::left_join(status_scores, by = c("region_id", "year", "stock")) %>%
    dplyr::filter(!is.na(score)) %>% # remove missing data
    dplyr::select(region_id, year, stock, propCatch, score) # clean data

  ## becaue of bad cod condition in Eastern Baltic(ICES_subdivision = 2532),
  ## we added a penalty factor of 0.872 based on historical cod body weight
  ## see FIS prep for full calculation of the penalty factor
  ## by Ning Jiang, 16 Feb, 2017
  status_with_penalty <- status %>%
    dplyr::mutate(scores_with_penalty = ifelse(
      stock == "cod_2532",
      score * 0.872,
      score)) %>%
    dplyr::select(-score) %>%
    dplyr::rename(score = scores_with_penalty)

  status <- status_with_penalty %>%
    dplyr::group_by(region_id, year) %>%
    dplyr::summarize(status = prod(score^propCatch)) %>%
    dplyr::ungroup() %>%
    data.frame()


  ## for trend, get slope of regression model based on most recent 5 yrs of data
  trend_years <- (scen_year - 4):(scen_year)

  trend <- status %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(year %in% trend_years) %>%
    dplyr::do(mdl = lm(status ~ year, data = .)) %>%
    dplyr::summarize(region_id = region_id,
                     trend = coef(mdl)["year"] * 5) %>%  # trend multiplied by 5 to predict 5 yrs out
    dplyr::ungroup() %>%
    dplyr::mutate(trend = round(trend, 2))

  ## could replace trend calc w/ ohicore::CalculateTrend, but then coeff is normalized by min trend year...
  ## need more years of data in scenario_data_years though for this to work
  # trend <- ohicore::CalculateTrend(status, trend_years)

  status <- status %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(status = round(status * 100, 1)) %>%
    dplyr::select(region_id, status)


  ## STEP 6: assemble dimensions ----

  scores <- status %>%
    dplyr::select(region_id, score = status) %>%
    tidyr::complete(region_id = full_seq(c(1, 42), 1)) %>%
    dplyr::mutate(dimension = "status") %>%
    rbind(
      trend %>%
        dplyr::select(region_id,
                      score = trend) %>%
        tidyr::complete(region_id = full_seq(c(1, 42), 1)) %>%
        dplyr::mutate(dimension = "trend")) %>%
    dplyr::mutate(goal = "FIS")

  return(scores)

} ## End FIS function

FP <- function(layers, scores){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R FP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()


  ## weights of FIS and MAR by rgn_id
  w <- ohicore::AlignDataYears(layer_nm="fp_wildcaught_weight",
                               layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::rename(region_id = rgn_id) %>%
    dplyr::select(-layer_name)

  ## scores of FIS and MAR with appropriate weight
  s <- scores %>%
    dplyr::filter(goal %in% c("FIS", "MAR")) %>%
    dplyr::filter(!(dimension %in% c("pressures", "resilience"))) %>%
    dplyr::left_join(w, by = "region_id")  %>%
    dplyr::mutate(w_mar = 1 - w_fis) %>%
    dplyr::mutate(weight = ifelse(goal == "FIS", w_fis, w_mar))


  ## warning messages for potential data mismatches ----
  ## NA score but there is a weight
  tmp <- s %>% dplyr::filter(
    goal == "FIS" &
      is.na(score) &
      dimension == "score" &
      (!is.na(w_fis) & w_fis != 0)
  )
  if (dim(tmp)[1] > 0) {
    warning(sprintf(
      "Check: these regions have a FIS weight but no score: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <- s %>% dplyr::filter(
    goal == "MAR" &
      is.na(score) &
      dimension == "score" &
      (!is.na(w_mar) & w_mar != 0)
  )
  if(dim(tmp)[1] > 0){
    warning(sprintf(
      "Check: these regions have a MAR weight but no score: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  ## there is a score, but weight is NA or 0
  tmp <- s %>% dplyr::filter(
    goal == "FIS" &
      dimension == "score" &
      region_id != 0 &
      (!is.na(score) & score > 0) &
      (is.na(w_fis) | w_fis == 0)
  )
  if(dim(tmp)[1] > 0) {
    warning(sprintf(
      "Check: these regions have a FIS score but no weight: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <- s %>% dplyr::filter(
    goal == "MAR" &
      (!is.na(score) & score > 0) &
      (is.na(w_mar) | w_mar == 0) &
      dimension == "score" &
      region_id != 0
  )
  if(dim(tmp)[1] > 0) {
    warning(sprintf(
      "Check: these regions have a MAR score but no weight: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }


  ## summarize scores for FP ----
  ## FP scores are based on MAR, and FIS weights
  scores_fp <- s %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    dplyr::arrange(goal, region_id) %>%
    data.frame()

  return(rbind(scores, scores_fp))

} ## End FP function

ICO <- function(layers){

  ## From code in 'functions.R ICO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep ICO subfolders)
  ## status is calculated in ico_prep.rmd because calculated by basin and then applied to BHI regions

  scen_year <- layers$data$scenario_year

  ico_status <- AlignDataYears(layer_nm="ico_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = "status", score = score*100) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  ## for now have NA iconic species trends...
  ico_trend <- data.frame(
    region_id = seq(1, 42, 1),
    score = rep(NA, 42),
    dimension = rep("trend", 42)) %>%
    dplyr::mutate(dimension = as.character(dimension))

  scores <- bind_rows(ico_status, ico_trend) %>%
    mutate(goal = "ICO") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End ICO function

LE <- function(layers, scores){

  ## From code in 'functions.R LE' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year

  scen_year <- layers$data$scenario_year

  ## average ECO and LIV dimensions to get LE scores
  scores.LE <- scores %>%
    dplyr::filter(goal %in% c("LIV", "ECO"),
                  dimension %in% c("status", "trend", "future", "score"),
                  year == scen_year) %>%
    reshape2::dcast(region_id + dimension ~ goal, value.var = "score") %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    mutate(goal = "LE") %>%
    dplyr::select(region_id, dimension, score) %>%
    data.frame()

  return(rbind(scores, scores.LE))

} ## End LE function

LIV <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R LIV' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, prep 'LIV' subfolder)

  liv_status <- AlignDataYears(layer_nm="liv_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = as.character("status")) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  liv_trend <- AlignDataYears(layer_nm="liv_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = as.character("trend")) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- rbind(liv_status, liv_trend) %>%
    mutate(goal = "LIV") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End LIV function

LSP <- function(layers){

  ## From code in 'functions.R LSP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep LSP subfolders)

  scen_year <- layers$data$scenario_year

  lsp_status <- AlignDataYears(layer_nm="lsp_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  lsp_trend <- AlignDataYears(layer_nm="lsp_trend", layers_obj=layers) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- rbind(lsp_status, lsp_trend) %>%
    mutate(goal = "LSP") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End LSP function

MAR <- function(layers){

  ## From code in 'functions.R MAR' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year
  regr_length <- 5 # number of years of data to use in trend calc
  regr_years <- seq(scen_year - regr_length + 1, scen_year)

  ## COLLECT LAYERS ----
  ## layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score
  harvest_tonnes <- AlignDataYears(layer_nm="mar_harvest_tonnes",
                                   layers_obj=layers)
  sustainability_score <- AlignDataYears(layer_nm="mar_sustainability_score",
                                         layers_obj=layers)
  harvest_species <- layers$data$mar_harvest_species # lookup, no data-scenario yr


  # ## move this to data prep for multiyear framework, so can use AlignDataYears function...
  # ## spread and gather data again which will result in all years present for all regions
  # scen_years <- 2010:2015
  # harvest_tonnes <- harvest_tonnes %>% spread(key = year, value = tonnes) %>%
  #   gather(year, tonnes, -rgn_id,-species_code) %>%
  #   mutate(year = as.numeric(year) ) %>%  # make sure year is not a character
  #   arrange(rgn_id,year) %>%
  #   filter(year %in% status_years)


  ## merge harvest (production) data with sustainability score
  ## ref_point is half of max production achieve sust coeff of 1
  tmp <- harvest_tonnes %>%
    dplyr::select(-layer_name) %>%
    dplyr::left_join(harvest_species %>% select(-layer),
                     by = "species_code") %>%
    dplyr::left_join(sustainability_score %>% select(-"layer_name"),
                     by = c("scenario_year", "rgn_id", "species")) %>%
    dplyr::group_by(rgn_id, species_code) %>% # by region and species but not scen yr
    dplyr::mutate(ref_value = max(tonnes) * 1) %>% # but why multiplying by 1?
    dplyr::ungroup() %>%
    dplyr::group_by(rgn_id, species_code, scenario_year) %>%
    dplyr::mutate(tonnes_sust = tonnes * sust_coeff) %>% # per rgn per yr
    dplyr::ungroup()


  ## Xmar = Mar_current / Mar_ref
  ## if use this, need to decide if the production should be scaled per capita

  ## CALCULATE STATUS ----
  mar_status_score <- tmp %>%
    dplyr::group_by(rgn_id, scenario_year) %>%
    dplyr::mutate(status = pmin(1, tonnes_sust/ref_value) * 100) %>%
    dplyr::select(rgn_id, scenario_year, status) %>%
    dplyr::ungroup() %>%
    dplyr::filter(scenario_year %in% regr_years)

  mar_status <- mar_status_score %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(rgn_id, score = status) %>%
    tidyr::complete(rgn_id = full_seq(c(1, 42), 1))


  ## CALCULATE TREND ----
  mar_trend <- mar_status_score %>%
    dplyr::group_by(rgn_id) %>%
    dplyr::do({
      ## calculate trend iff have all yrs of data in last 5 (regr_length) years of time series
      ## regr_length set in contants, this is the value 5 in the old code
      if(sum(!is.na(.$status)) >= regr_length){
        data.frame(trend_score = max(-1, min(1, coef(lm(status ~ scenario_year, .))["scenario_year"]*0.05)))
      } else {data.frame(trend_score = NA)}
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(trend_score = round(trend_score, 2)) %>%
    tidyr::complete(rgn_id = full_seq(c(1, 42), 1))


  ## ASSEMBLE DIMENSIONS & RETURN SCORES ----
  scores <- mar_status %>%
    dplyr::select(region_id = rgn_id, score = score) %>%
    dplyr::mutate(dimension = "status") %>%
    rbind(
      mar_trend %>%
        dplyr::select(region_id = rgn_id,
                      score = trend_score) %>%
        dplyr::mutate(dimension = "trend")) %>%
    dplyr::mutate(goal = "MAR")

  return(scores)

} # End MAR function

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

SP <- function(layers, scores){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R SP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year

  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP

  s <- scores %>%
    filter(goal %in% c("ICO", "LSP"),
           dimension %in% c("status", "trend", "future", "score")) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  return(rbind(scores, s))

} ## End SP function

TR <- function(layers){

  ## From code in 'functions.R TR' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep TR subfolders)
  ## possible alternative: base metric on EU blue growth reports?

  scen_year <- layers$data$scenario_year

  tr_status <- AlignDataYears(layer_nm="tr_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  tr_trend <- AlignDataYears(layer_nm="tr_trend", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- dplyr::bind_rows(tr_status, tr_trend) %>%
    dplyr::mutate(goal = "TR")

  return(scores)

} ## end TR function

TRA <- function(layers){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R TRA' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how status and reference point are calculated in prep file (bhi-prep repository, 'CW/trash' subfolders)

  tra_status <- AlignDataYears(layer_nm="cw_tra_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::rename(region_id = rgn_id, year = cw_tra_status_year)

  tra_trend <- AlignDataYears(layer_nm="cw_tra_trend", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::rename(region_id = rgn_id, year = cw_tra_trend_year)

  scores <- rbind(tra_status, tra_trend) %>%
    mutate(goal = "TRA") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End TRA function
