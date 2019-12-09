## Note that some of the commands used below are from older R packages that we don't recommend using anymore (dcast, etc). Instead, use
## dplyr and tydr packages that have more streamlined functions to manipulate data. To learn those funtions:
## http://ohi-science.org/manual/#appendix-5-r-tutorials-for-ohi

## See functions.R of CHN (OHI-China) for how those functions are used in OHI+ assessments


FIS = function(layers){
  ## FIS code revised by Melanie Fraser
  ## added to functions.r by Jennifer Griffiths 7 June 2016
  ## only Cod and Herrings data are used here. Sprats data moved to NP, by Ning Jiang in Oct 2016.


  scenario_years <- layers$data$scenario_year

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
    dplyr::select(rgn_id = id_num,
           stock = category,
           year,
           landings= val_num)%>%
    dplyr::rename(region_id = rgn_id)


  cod_penalty = SelectLayersData(layers, layers='fis_cod_penalty') %>%
    dplyr::select(rgn_id =id_num,
                  year,
                  prop_above_pt8 = val_num)%>%
    dplyr::rename(region_id = rgn_id) %>%
    dplyr::mutate(
      stock = as.character(unique(bbmsy$stock)) %>%
        grep(pattern = "cod", value = TRUE) %>%
        grep(pattern = "32", value = TRUE)
    )


  ## combine bbmsy and ffmsy to single object

  metric_scores = rbind(bbmsy, ffmsy) %>%
    dplyr::select(region_id, stock, year, metric, score) %>%
    mutate(metric = as.factor(metric))%>%
    spread(metric, score)


  ## BSCORES
  ## note: these parameters can be changed (see data prep for exploration)!
  lowerB <- 0.95
  upperB1 <- 1.3
  upperB2 <- 5

  B_scores <- metric_scores %>%
    mutate(
      score_type = "B_score",

      score = ifelse(
        bbmsy < lowerB, (1/lowerB)*bbmsy,
        ifelse(
          lowerB <= bbmsy & bbmsy < upperB1, 1,
          ifelse(
            bbmsy >= upperB1,
            (bbmsy - upperB2)/(upperB1-upperB2), NA
          )
        )
      )
    ) %>%
    mutate(
      score = ifelse(
        score <= 0.1, 0.1,
        ifelse(score > 1, 1, score)
      )
    )

  ## FSCORES
  ## STEP 2: converting B/Bmsy and F/Fmsy to F-scores

  lowerF <- 0.5
  upperF1 <- 1
  upperF2 <- 4.5
  m <- (upperF2-1)/lowerB

  # install.packages("pracma")
  norm1 = pracma::cross(
    c(lowerB, upperF1, 1) - c(0, 1, 0),
    c(lowerB, upperF2, 0) - c(0, 1, 0)
  )
  norm2 = pracma::cross(
    c(lowerB, 0, (upperF2-lowerF-1)/(upperF2-1)) - c(lowerB, lowerF, 1),
    c(0, 1-(upperF2-lowerF), 1) - c(lowerB, lowerF, 1)
  )


  F_scores <- metric_scores %>%
    mutate(
      score_type = "F_score",

      score = ifelse(
        ## when bbmsy < lowerB :
        bbmsy < lowerB,

        ## will be space near zero where scores start going back down from 1:
        ## on y-axis towards zero if upperF2-lowerF < 1, on x-axis towards zero if upperF2-upperF1 > 1
        ifelse(
          ffmsy > m*bbmsy + 1, 0,
          ifelse(
            m*bbmsy + 1 >= ffmsy & ffmsy > m*bbmsy + (1-(upperF2-upperF1)),
            ## http://mathworld.wolfram.com/Plane.html n1x + n2y + n3z - (n1x0 + n2y0 + n3z0) = 0
            (norm1[2] - norm1[1]*bbmsy - norm1[2]*ffmsy)/norm1[3],
            ifelse(
              m*bbmsy + (1-(upperF2-upperF1)) >= ffmsy & ffmsy > m*bbmsy + (1-(upperF2-lowerF)), 1,
              ((norm2[1]*lowerB + norm2[2]*lowerF + norm2[3]) - (norm2[1]*bbmsy + norm2[2]*ffmsy))/norm2[3]
            )
          )
        ),
        ## when bbmsy >= lowerB :
        ifelse(
          ffmsy > upperF1,
          (upperF2-ffmsy)/(upperF2-upperF1),
          ifelse(
            upperF1 >= ffmsy & ffmsy > lowerF,
            1,
            ffmsy/(upperF2-1) + (upperF2-lowerF-1)/(upperF2-1)
          )
        )
      )
    ) %>%
    ## set scores less than 0.1 to 0.1, greater than 1 to 1
    ## 0.1 rather than zero because one zero w/ geometric mean results in a zero score
    mutate(
      score = ifelse(
        score <= 0.1, 0.1,
        ifelse(score > 1, 1, score)
      )
    )

  status_scores <- rbind(B_scores, F_scores) %>%
    group_by(region_id, stock, year) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>% # hist(scores$score)
    left_join(
      spread(rbind(B_scores, F_scores), key = "score_type", value = "score"),
      by = c("year", "stock", "region_id")
    )


  ## STEP 4: calculating the weights ----

  ## subset the data to include only the most recent 10 years
  landings <- landings %>%
    dplyr::select(year, region_id, stock, landings) %>%
    dplyr::filter(year %in% (max(year)-9):max(year))

  ## we use the average catch for each stock/region across the last 25 years to obtain weights
  weights <- landings %>%
    filter(year > max(year) - 26) %>%
    ## each region/stock will have same avg catch across years, timeseries mean
    group_by(region_id, stock) %>%
    mutate(avgCatch = mean(landings)) %>%
    ungroup() %>%
    group_by(region_id, year) %>%
    mutate(totCatch = sum(avgCatch)) %>% # total (i.e. all stock) catch on average (across years)
    ungroup() %>%
    ## average prop. of catch each stock accounts for, over last 25yr
    dplyr::mutate(propCatch = avgCatch/totCatch) %>%
    data.frame()

  ## if saved rgn_landings as BHI data layer...
  # weights <- landings %>%
  #   group_by(region_id, year) %>%
  #   mutate(rgn_yr_totCatch = sum(landings)) %>%
  #   ungroup() %>%
  #   ## proportion a given stock makes up of total catch in region, by year
  #   mutate(rgn_yr_propCatch = landings/rgn_yr_totCatch)


  ## STEP 5: calculate status and trend ----

  ## join scores and weights to calculate status
  status_with_penalty <- weights %>%

    ## start with overall score as an average of B-scores and F-scores
    ## these averages will be weighted by stock landings and have cod condition penalty applied
    left_join(status_scores, by = c("region_id", "year", "stock")) %>%
    filter(!is.na(score)) %>% # remove missing data

    ## apply penalty because bad cod condition in eastern baltic
    left_join(cod_penalty, by = c("year", "region_id", "stock")) %>%
    # mutate(penalty = ifelse(
    #   is.na(prop_above_pt8), 1, ifelse(
    #     num_samp %in% 1:3, NA, prop_above_pt8 # based on 3 or fewer data points, make NA...
    #   )
    # )) %>%
    # dplyr::select(-prop_above_pt8, -num_samp) %>%
    mutate(penalty = ifelse(
      is.na(prop_above_pt8) & !str_detect(stock, "cod.*32"),
      1, prop_above_pt8
    )) %>%
      # is.na(prop_above_pt8), 1, prop_above_pt8)) %>%
    dplyr::select(-prop_above_pt8) %>%
    mutate(score = penalty*score)

  ## calculate BHI scores, using stocks by region
  ## scores are geometric mean weighted by stock proportions of total catch in each region
  status_scores <- status_with_penalty %>%
    dplyr::filter(!is.na(score)) %>% # remove missing data
    dplyr::group_by(region_id, year) %>%
    dplyr::summarize(status_prop = prod(score^propCatch)) %>%
    dplyr::ungroup() %>%
    data.frame()

  trend_years <- (max(status_scores$year)-4):max(status_scores$year)

  trend <- status_scores %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(year %in% trend_years) %>%
    dplyr::do(mdl = lm(status_prop ~ year, data = .)) %>%
    dplyr::summarize(region_id = region_id,
                     trend = coef(mdl)["year"] * 5) %>% # trend multiplied by 5 to predict 5 yrs out
    dplyr::ungroup() %>%
    dplyr::mutate(trend = round(trend, 2))

  scores <- rbind(
    ## status scores
    status_scores %>%
      dplyr::filter(year == scenario_years) %>%
      dplyr::mutate(status = round(status_prop * 100, 1)) %>%
      dplyr::select(region_id, score = status) %>%
      tidyr::complete(region_id = full_seq(c(1, 42), 1)) %>%
      dplyr::mutate(dimension = "status"),
    ## trend scores
    trend %>%
      dplyr::select(region_id, score = trend) %>%
      tidyr::complete(region_id = full_seq(c(1, 42), 1)) %>%
      dplyr::mutate(dimension = "trend")) %>%
    dplyr::mutate(goal = "FIS")

  return(scores)

} ## End FIS function


MAR = function(layers){
  ##updated by Jennifer Griffiths 25Feb2016
  ##updated by Julie 26Feb2016
  ##updated by Jennifer Griffiths 29Feb2016 - make sure mar_status_score limited to status_years, change layer names
  ##updated by Jennifer Griffiths 29March2016 - added code for temporal reference point but this is commented out until final decision made
  ##updated by Jennifer Griffiths 05April2016 - made reference point temporal (removed spatial), made data unit tons of production, not per capita
  ##updated by Jennifer Griffiths 16June2016 - change code so areas with no data are NA for status (not zero)
  ##updated by Ning Jiang 3Nov2016 - changed status function, and ref point to: max production of most recent five years has sust coef of 1

  ##layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score


  ## select layers for MAR
  harvest_tonnes = SelectLayersData(layers, layers='mar_harvest_tonnes', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
           species_code = category,
           year,
           tonnes = val_num)

  harvest_species = SelectLayersData(layers, layers='mar_harvest_species', narrow=T) %>%
    dplyr::select(species_code = category,
           species = val_chr)

  sustainability_score = SelectLayersData(layers, layers='mar_sustainability_score', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
           species = category,
           sust_coeff = val_num)


  ## SETTING CONSTANTS
  regr_length =5          # number of years to use for regression for trend.  Use this to replace reading in the csv file "mar_trend_years_gl2014"
  status_years = 2010:2014 #this was originally set in goals.csv

  #####----------------------######
  ##harvest_tonnes has years without data but those years not present with NAs
  ##spread and gather data again which will result in all years present for all regions
  harvest_tonnes=harvest_tonnes %>% spread(key=year,value=tonnes) %>%
    gather(year, tonnes, -rgn_id,-species_code) %>%
    mutate(year=as.numeric(year) )%>%  #make sure year is not a character
    arrange(rgn_id,year) %>%
    filter(year %in% status_years)

  # Merge harvest (production) data with sustainability score
  # ref_point = half of max production achieve sust coeff of 1
  temp = left_join(harvest_tonnes, harvest_species, by = 'species_code') %>%
    left_join(., sustainability_score, by = c('rgn_id', 'species')) %>%
    group_by(rgn_id, species_code) %>%
    mutate(tonnes_sust = tonnes * sust_coeff,
           ref_value = max(tonnes) * 1) %>%
    ungroup()

  ###----------------------------###
  ## Xmar = Mar_current / Mar_ref
  ## if use this, need to decide if the production should be scaled per capita

  ## Calculate status
  mar_status_score = temp %>% group_by(rgn_id)%>%
    mutate(status = pmin(1, tonnes_sust/ref_value) * 100)%>% #calculate status per year
    dplyr::select(rgn_id, year, status)%>%
    ungroup()

  mar_status = mar_status_score %>%
    filter(year == max(year)) %>%
    dplyr::select(rgn_id, score = status) %>%
    complete(rgn_id = full_seq(c(1, 42), 1))

  ###----------------------------###
  ## Calulate trend
  mar_trend= mar_status_score %>%
    group_by(rgn_id) %>%
    do(tail(. , n = regr_length)) %>% #select the years for trend calculate (regr_length # years from the end of the time series)
    #regr_length replaces the need to read in the trend_yrs from the csv file
    do({if(sum(!is.na(.$status)) >= 5)      #calculate trend only if X years of data (min_regr_length) in the last Y years of time serie (regr_length)
      data.frame(trend_score = max(-1, min(1, coef(lm(status ~ year, .))['year'] * 0.05))) #future_year set in contants, this is the value 5 in the old code
      else data.frame(trend_score = NA)}) %>%
    ungroup() %>%
    mutate(trend_score = round(trend_score,2)) %>%
    complete(rgn_id = full_seq(c(1, 42), 1))

  #####----------------------######
  # return MAR scores
  scores = mar_status %>%
    dplyr::select(region_id = rgn_id,
           score     = score) %>%
    mutate(dimension='status') %>%
    rbind(
      mar_trend %>%
        dplyr::select(region_id = rgn_id,
               score     = trend_score) %>%
        mutate(dimension = 'trend')) %>%
    mutate(goal='MAR')
  return(scores)


} #end MAR function

FP = function(layers, scores){

  # weights of FIS, MAR by rgn_id
  w <- SelectLayersData(layers, layers='fp_wildcaught_weight', narrow = TRUE) %>%
    dplyr::select(region_id = id_num,
           w_FIS = val_num); head(w)

  # scores of FIS, MAR with appropriate weight
  s <- scores %>%
    filter(goal %in% c('FIS', 'MAR')) %>%
    filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    left_join(w, by="region_id")  %>%
    mutate(w_MAR = 1 - w_FIS) %>%
    mutate(weight = ifelse(goal == "FIS", w_FIS, w_MAR))


  ## Some warning messages for potential mismatches in data:
  # NA score but there is a weight
  tmp <- filter(s, goal=='FIS' & is.na(score) & (!is.na(w_FIS) & w_FIS!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & is.na(score) & (!is.na(w_MAR) & w_MAR!=0) & dimension == "score")
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR weight but no score: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  # score, but the weight is NA or 0
  tmp <- filter(s, goal=='FIS' & (!is.na(score) & score > 0) & (is.na(w_FIS) | w_FIS==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a FIS score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  tmp <- filter(s, goal=='MAR' & (!is.na(score) & score > 0) & (is.na(w_MAR) | w_MAR==0) & dimension == "score" & region_id !=0)
  if(dim(tmp)[1]>0){
    warning(paste0("Check: these regions have a MAR score but no weight: ",
                   paste(as.character(tmp$region_id), collapse = ", ")))}

  ## summarize scores as FP based on MAR, FIS weight
  s <- s  %>%
    group_by(region_id, dimension) %>%
    summarize(score = weighted.mean(score, weight, na.rm=TRUE)) %>%
    mutate(goal = "FP") %>%
    ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  ## return all scores
  return(rbind(scores, s))
} ## End FP Function


NP = function(layers){

  ## This is the same function as FIS except for data. Only sprat data is used here.

  ## Call Layers
  bbmsy = SelectLayersData(layers, layers='np_bbmsy', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
           stock = category,
           year,
           score= val_num) %>%
    mutate(metric ="bbmsy") %>%
    dplyr::rename(region_id = rgn_id)

  ffmsy = SelectLayersData(layers, layers='np_ffmsy', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
           stock = category,
           year,
           score= val_num) %>%
    mutate(metric= "ffmsy")%>%
    dplyr::rename(region_id = rgn_id)

  landings = SelectLayersData(layers, layers='np_landings', narrow=T) %>%
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
    filter(year %in% (max(year)-9):max(year))

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

  #### The total proportion of landings for each region/year will sum to one:
  #filter(weights, region_id ==3, year==2014)

  ############################################################
  #####  STEP 5: Join scores and weights to calculate status
  ############################################################


  status <- weights %>%
    left_join(status.scores, by=c('region_id', 'year', 'stock'))%>%
    filter(!is.na(score)) %>%                    # remove missing data
    dplyr::select(region_id, year, stock, propCatch, score)        # cleaning data


  ### Geometric mean weighted by proportion of catch in each region
  status <- status %>%
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

  ### final formatting of status data:
  status <- status %>%
    filter(year == max(year)) %>%
    mutate(status = round(status * 100, 1)) %>%
    dplyr::select(region_id, status)


  ############################################################
  #####  STEP 6: Assemble dimensions
  ############################################################

  scores = status %>%
    dplyr::select(region_id,
           score = status)%>%
    mutate(dimension='status') %>%
    rbind(
      trend %>%
        dplyr::select(region_id,
               score = trend) %>%
        mutate(dimension = 'trend')) %>%
    mutate(goal='NP')
  return(scores)

}

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


CS = function(layers){

  cs_status <- layers$data[['cs_status']] %>%
    dplyr::select(region_id = rgn_id, dimension, score) %>%
    mutate(dimension = as.character(dimension))

  cs_trend <- data.frame(region_id = seq(1,42,1),
                         dimension = rep("trend",42),
                         score = rep((NA), 42)) %>%
    mutate(dimension = as.character(dimension))

  scores = rbind(cs_status, cs_trend) %>%
    mutate(goal = 'CS')

  return(scores)
}## End CS function



TR = function(layers){

  ## CALCULATE STATUS ##
  ## Scores calculated in prep file (prep/TR/tr_prep.rmd) Alternative 2 based on EU blue growth report 2015
  ## updated by Ning Jiang Jan2017

tr_status = layers$data[['tr_status']] %>%
  dplyr::select(region_id = rgn_id, score, dimension)
tr_trend = layers$data[['tr_trend']] %>%
  dplyr::select(region_id = rgn_id, score, dimension)

  ## create scores and rbind status and trend scores
  scores = tr_status %>%
    bind_rows(.,tr_trend) %>%
    mutate(goal='TR')

  return(scores)
  ##-------------------------------------------------------------------##
} ## end TR function


LIV = function(layers){

  # ## Updated 11 July 2016 by Jennifer Griffiths
  #
  # ##-------------------------------------------------##
  # ## Select Layers
  # liv_regional_employ = SelectLayersData(layers, layers='liv_regional_employ') %>%
  #   dplyr::select(rgn_id = id_num, year,  employ_pop_bhi= val_num)
  #
  #
  # liv_national_employ = SelectLayersData(layers, layers='liv_national_employ') %>%
  #   dplyr::select(rgn_id = id_num, year,  employ_pop= val_num)
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ###Set parameters
  # ## set lag window for reference point calculations
  # lag_win = 5  # 5 year lag
  # trend_yr = 4 # to select the years for the trend calculation, select most recent year - 4 (to get 5 data points)
  # bhi_rgn = data.frame(rgn_id = as.integer(seq(1,42,1))) #unique BHI region numbers to make sure all included with final score and trend
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ### STATUS CALCULATION
  #
  # ### prepare region and country layers
  #
  # ## LIV region: prepare for calculations with a lag
  # liv_region = liv_regional_employ %>%
  #   dplyr::rename(employ = employ_pop_bhi) %>%
  #   filter(!is.na(employ)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(employ, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(rgn_value = employ/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,rgn_value) %>%
  #   arrange(rgn_id,year)
  #
  #
  # ## LIV country
  # liv_country =   liv_national_employ %>%
  #   dplyr::rename(employ = employ_pop) %>%
  #   filter(!is.na(employ)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(employ, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(cntry_value = employ/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,cntry_value) %>%
  #   arrange(rgn_id,year)
  #
  # ## Calculate status time series
  # ## calculate status
  # liv_status_calc = inner_join(liv_region,liv_country, by=c("rgn_id","year"))%>% #join region and country current/ref ratios ## inner_join because need to have both region and country values to calculate
  #   mutate(Xliv = rgn_value/cntry_value)%>% #calculate status
  #   mutate(status = pmin(1, Xliv)) # status calculated cannot exceed 1
  #
  # ## Extract most recent year status
  # liv_status = liv_status_calc%>%
  #   group_by(rgn_id)%>%
  #   filter(year== max(year))%>%       #select status as most recent year
  #   ungroup()%>%
  #   full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for status, this should be 0 to indicate the measure is applicable, just no data
  #   mutate(score=round(status*100),   #scale to 0 to 100
  #          dimension = 'status')%>%
  #   select(region_id = rgn_id,score, dimension) #%>%
  # ##mutate(score= replace(score,is.na(score), 0)) #assign 0 to regions with no status calculated because insufficient or no data
  # ##will this cause problems if there are regions that should be NA (because indicator is not applicable?)
  #
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ##  CALCULATE TREND
  #
  # ## calculate trend for 5 years (5 data points)
  # ## years are filtered in liv_region and liv_country, so not filtered for here
  # liv_trend = liv_status_calc %>%
  #   filter(year >= max(year - trend_yr))%>%                #select five years of data for trend
  #   filter(!is.na(status)) %>%                              # filter for only no NA data because causes problems for lm if all data for a region are NA
  #   group_by(rgn_id) %>%
  #   mutate(regr_length = n())%>%                            #get the number of status years available for greggion
  #   filter(regr_length == (trend_yr + 1))%>%                   #only do the regression for regions that have 5 data points
  #   do(mdl = lm(status ~ year, data = .)) %>%             # regression model to get the trend
  #   summarize(rgn_id = rgn_id,
  #             score = coef(mdl)['year'] * lag_win)%>%
  #   ungroup() %>%
  #   full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for trend #should this stay NA?  because a 0 trend is meaningful for places with data
  #   mutate(score = round(score, 2),
  #          dimension = "trend") %>%
  #   select(region_id = rgn_id, dimension, score) %>%
  #   data.frame()
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ## FINAL SCORES OBEJCT
  # ## create scores and rbind to other goal scores

  ########### updated 10.28.2016 by Ning Jiang from ohi-science ##############

  liv_status = layers$data[['liv_status']] %>%
    dplyr::select(region_id = rgn_id, score) %>%
    mutate(dimension = "status")

  liv_trend = layers$data[['liv_trend']] %>%
    dplyr::select(region_id = rgn_id, score) %>%
    mutate(dimension = "trend")

  scores = liv_status %>%
    bind_rows(.,liv_trend) %>%
    mutate(goal='LIV')

  return(scores)

} ## End LIV function


ECO = function(layers){

  ## Status model: Xeco = (GDP_Region_c/GDP_Region_r)/(GDP_Country_c/GDP_Country_r)

  ## Updated 7 July 2016, Jennifer Griffiths. If there is no data for status or trend, is NA


  # ## read in data
  # ## in data prep, year range included is selected
  #   ##if different time periods exisit for region and country, NAs for status will be produced
  # le_gdp_region   = SelectLayersData(layers, layers='le_gdp_region') %>%
  #   dplyr::select(rgn_id = id_num, year, rgn_gdp_per_cap = val_num)
  #
  # le_gdp_country  = SelectLayersData(layers, layers='le_gdp_country') %>%
  #   dplyr::select(rgn_id = id_num, year, nat_gdp_per_cap = val_num)
  #
  #
  # ## temp readin TODO: SelectLayers()
  # # library(dplyr)
  # # le_gdp_region = read.csv('~/github/bhi/baltic2015/layers/le_gdp_region_bhi2015.csv'); head(le_gdp_region)
  # # le_gdp_country = read.csv('~/github/bhi/baltic2015/layers/le_gdp_country_bhi2015.csv'); head(le_gdp_country)
  #
  # ## set lag window for reference point calculations
  # lag_win = 5  # 5 year lag
  # trend_yr = 4 # to select the years for the trend calculation, select most recent year - 4 (to get 5 data points)
  # bhi_rgn = data.frame(rgn_id = as.integer(seq(1,42,1))) #unique BHI region numbers to make sure all included with final score and trend
  #
  # ## ECO region: prepare for calculations with a lag
  # eco_region = le_gdp_region %>%
  #   dplyr::rename(gdp = rgn_gdp_per_cap) %>%
  #   filter(!is.na(gdp)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(gdp, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(rgn_value = gdp/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,rgn_value)
  #
  # ## ECO country
  # eco_country = le_gdp_country %>%
  #   dplyr::rename(gdp = nat_gdp_per_cap) %>%
  #   filter(!is.na(gdp)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(gdp, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(cntry_value = gdp/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,cntry_value)
  #
  # ## calculate status
  # eco_status_calc = full_join(eco_region,eco_country, by=c("rgn_id","year"))%>% #join region and country current/ref ratios
  #              mutate(Xeco = rgn_value/cntry_value)%>% #calculate status
  #              mutate(status = pmin(1, Xeco)) # status calculated cannot exceed 1
  #
  # eco_status = eco_status_calc%>%
  #   group_by(rgn_id)%>%
  #   filter(year== max(year))%>%       #select status as most recent year
  #   ungroup()%>%
  #   full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for status
  #   mutate(score=round(status*100),   #scale to 0 to 100
  #          dimension = 'status')%>%
  #   select(region_id = rgn_id,score, dimension)
  #
  # ## this is where could change NA value of status to zero
  # #%>%
  # ##mutate(score= replace(score,is.na(score), 0)) #assign 0 to regions with no status calculated because insufficient or no data
  # ##will this cause problems if there are regions that should be NA (because indicator is not applicable?)
  #
  #
  # ## calculate trend for 5 years (5 data points)
  #     ## years are filtered in eco_region and eco_country, so not filtered for here
  #     eco_trend = eco_status_calc %>%
  #       filter(year >= max(year - trend_yr))%>%                #select five years of data for trend
  #       filter(!is.na(status)) %>%                              # filter for only no NA data because causes problems for lm if all data for a region are NA
  #       group_by(rgn_id) %>%
  #       mutate(regr_length = n())%>%                            #get the number of status years available for greggion
  #       filter(regr_length == (trend_yr + 1))%>%                   #only do the regression for regions that have 5 data points
  #         do(mdl = lm(status ~ year, data = .)) %>%             # regression model to get the trend
  #           summarize(rgn_id = rgn_id,
  #                     score = coef(mdl)['year'] * lag_win)%>%
  #       ungroup() %>%
  #       full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for trend #should this stay NA?  because a 0 trend is meaningful for places with data
  #       mutate(score = round(score, 2),
  #              dimension = "trend") %>%
  #       select(region_id = rgn_id, dimension, score) %>%
  #       data.frame()

  ### updated 14Nov, 2016 by Ning Jiang from ohi-science team

  eco_status = layers$data[['eco_status']] %>%
    mutate(dimension = 'status') %>%
    dplyr::select(-layer)

  eco_trend = layers$data[['eco_trend']] %>%
    mutate(dimension = 'trend') %>%
    dplyr::select(-layer)

  ## create scores and rbind to other goal scores
  scores = rbind(eco_status, eco_trend) %>%
    mutate(goal='ECO') %>%
    dplyr::select(region_id = rgn_id,
           score,
           dimension,
           goal)

   return(scores)

} ## End ECO function


LE = function(scores, layers){

  # calculate LE scores
  scores.LE = scores %>%
    filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
    reshape2::dcast(region_id + dimension ~ goal, value.var='score') %>%
    mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %>%
    dplyr::select(region_id, dimension, score) %>%
    mutate(goal  = 'LE')

  # rbind to all scores
  scores = scores %>%
    rbind(scores.LE)

  # return scores
  return(scores)
} ## End LE function


ICO = function(layers){
    ## UPDATED 22 June 2016 Jennifer Griffiths

  ## Select layers

  ##
  ico_status   = SelectLayersData(layers, layers='ico_status') %>%
    dplyr::select(rgn_id = id_num, score = val_num)


  ## ICO STATUS
    ## Status is calculated in ico_prep.rmd because calculated by basin and then applied to BHI regions

  ## add dimension to object
  ico_status = ico_status %>%
               dplyr::rename(region_id =rgn_id) %>%
               mutate(dimension = 'status',
                      score = score* 100)

  ## ICO TREND
   ## No data to calculate as trend, set as NA

  ico_trend = data.frame(region_id = seq(1,42,1),
                     score = rep(NA,42),
                     dimension = rep('trend',42))

  ## Return scores
  scores = bind_rows(ico_status,
                     ico_trend)%>%
            mutate(goal   = 'ICO')

  return(scores)


} ## end ICO function


LSP = function(layers){

# status

lsp_status <- SelectLayersData(layers, layers = 'lsp_status') %>%
  dplyr::select(region_id = id_num,
         score = val_num) %>%
  mutate(dimension = 'status'); head(lsp_status)

# trend

lsp_trend <-  SelectLayersData(layers, layers = 'lsp_trend') %>%
  dplyr::select(region_id = id_num,
                score = val_num) %>%
  mutate(dimension = 'trend'); head(lsp_trend)

# combine scores

scores <- rbind(lsp_status, lsp_trend) %>%
  mutate(goal = 'LSP')

    return(scores)
}


SP = function(scores){

  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    filter(goal %in% c('ICO','LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm=TRUE)) %>%
    ungroup() %>%
    arrange(region_id) %>%
    mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
} ## End SP function


EUT = function(layers){
  #####----------------------######
  ## EUT Status - Secchi + Anoxia + DIN (dissolved inorganic nitrogen) + DIP (dis. inorg. phophorus) + Chla (Chlor. A)
  #####----------------------######

  # updated 21March, 2017 by Ning Jiang

  eut_status = SelectLayersData(layers, layers='cw_eut_status') %>%
    dplyr::select(rgn_id = id_num, score = val_num) %>%
    mutate(dimension = "status")

  eut_trend  = SelectLayersData(layers, layers='cw_eut_trend') %>%
    dplyr::select(rgn_id = id_num, score = val_num) %>%
    mutate(dimension = "trend")

  # rbind eut status and trend to one dataframe
  scores =  rbind(eut_status, eut_trend) %>%
    mutate(goal = 'EUT') %>%
    dplyr::select(goal,
                  dimension,
                  region_id = rgn_id,
                  score) %>%
    arrange(dimension, region_id)

  return(scores)
  #####----------------------######

} ## End NUT Function

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

CON = function(layers){

  #####----------------------######
  ## Contaminants
  #####----------------------######
  ## UDPATE 11May2016 - Jennifer Griffiths - CON ICES6 added from contaminants_prep,
  ## TODO
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
  cw_con_dioxin_status = SelectLayersData(layers, layers='cw_con_dioxin_status') %>%
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
  cw_con_pfos_status  = SelectLayersData(layers, layers='cw_con_pfos_status') %>%
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
    mutate(score2 = score) %>% # NO PENALTY!
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

}  ## END CON Function

CW = function(scores){
  #####----------------------######
  ## CW status & CW Trend

  ## UPDATE 15June 2016 - Jennifer Griffiths - update CW status and trend calculation for CW
  ## only have 1 status point for CON, TRA (therefore can not take a CW status as geometric mean with many points over time)
  ## Calculate geometric mean of 1 status for current status
  ## calculate arithmetic mean of NUT and CON trend for the CW trend (because have 0, NA, and neg. values in trend, cannot use geometric mean)


  ## Status is the geometric mean of NUT, CON, TRA status for most recent year
  ## trend in the arithmetic mean of NUT, CON, TRA trend because can not deal with 0 values in geometric mean

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }

  ## Function to deal with cases where want to take the arithmetic mean of a vector of NA values, will return NA instead of NaN
  mean_NAall = function(x){

    if(sum(is.na(x))==length(x)){mean_val = NA
    }else{mean_val =mean(x,na.rm=TRUE) }
    return(mean_val)
  }

  ## subset CW subgoals
  scores_cw <- scores %>%
    filter(goal %in% c('NUT', 'TRA', 'CON')) %>%
    arrange(dimension,region_id)

  ## Calculate geometric mean for status, arithmetic mean for trend (ignore NAs)
  ## NOTE to @jennifergriffiths: there are still several 'NaN's, perhaps because of TRA?
  ## 7 July 2016 - Think have fixed the NaN problem with the function mean_NAall()
  ## also, rounding score doesn't seem to work here; ends up with .00 precision. maybe round later?

  ## July 21, 2016 @jules32 after discussing with @Melsteroni.
  ## Calculate CW as a geometric mean from NUT, CON, and TRA
  ## for only 3 dimensions: status, likely future state and score.
  ## Calculate trend as a simple mean.
  s <- rbind(
    scores_cw %>%
      filter(dimension %in% c('status', 'future', 'score')) %>%
      group_by(region_id, dimension) %>%
      summarize(score = round(geometric.mean2(score, na.rm=TRUE))) %>% # round status to 0 decimals
      ungroup(),
    scores_cw %>%
      filter(dimension %in% c('trend')) %>%
      group_by(region_id, dimension) %>%
      summarize(score = mean(score, na.rm=TRUE)) %>%
      ungroup()) %>%
    arrange(region_id) %>%
    mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  ## return all scores
  return(rbind(scores, s))
} ## End CW function


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


FinalizeScores = function(layers, conf, scores){

  ## modified from original to aggregte to EEZs and SUBBASINs
  ## @jules32 September 2016


  ## Regions to aggregate as eezs and basins

  # source('create_rgns_lookup.R')

  ## complete dataframe
  rgns_complete <- read.csv('regions_lookup_complete.csv', stringsAsFactors = FALSE); head(rgns_complete)
  rgns <- read.csv('regions_lookup_complete_wide.csv', stringsAsFactors = FALSE)

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
