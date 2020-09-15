
FIS <- function(layers){

  ## From code in 'functions.R FIS' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  bbmsy <- AlignDataYears(layer_nm="fis_bbmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="bbmsy")
  ffmsy <- AlignDataYears(layer_nm="fis_ffmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="ffmsy")

  ## combine bbmsy and ffmsy into single object
  metric_scores <- rbind(
    dplyr::select(bbmsy, -fis_bbmsy_year, -layer_name, year = scenario_year),
    dplyr::select(ffmsy, -fis_ffmsy_year, -layer_name, year = scenario_year)
  )
  metric_scores <- metric_scores %>%
    dplyr::mutate(metric = as.factor(metric)) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)


  landings <- AlignDataYears(layer_nm="fis_landings", layers_obj=layers) %>%
    select(year = scenario_year, region_id, stock, landings)

  cod_penalty <- AlignDataYears(layer_nm="fis_cod_penalty", layers_obj=layers) %>%
    dplyr::mutate(
      stock = as.character(unique(bbmsy$stock)) %>%
        grep(pattern = "cod", value = TRUE) %>%
        grep(pattern = "32", value = TRUE)
    ) %>%
    select(year = scenario_year, region_id, penalty_factor, stock)


  ## BSCORES
  ## STEP 1: converting B/Bmsy and F/Fmsy to B-scores ----
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
  ## STEP 2: converting B/Bmsy and F/Fmsy to F-scores ----

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

  ## STEP 3: averaging the F and B-scores to get the stock status score ----

  status_scores <- rbind(B_scores, F_scores) %>%
    group_by(region_id, stock, year) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>% # hist(scores$score)
    left_join(
      spread(rbind(B_scores, F_scores), key = "score_type", value = "score"),
      by = c("year", "stock", "region_id")
    )


  ## STEP 4: calculating the weights ----

  ## subset the data to include only the most recent 10 years
  # landings <- dplyr::filter(landings, year %in% (scen_year-9):scen_year)

  ## we use the average catch for each stock/region across the last 25 years to obtain weights
  weights <- landings %>%
    filter(year > scen_year - 25) %>%
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


  ## STEP 5: calculate status and trend ----

  ## STATUS

  ## join scores and weights to calculate status
  status_with_penalty <- weights %>%

    ## start with overall score as an average of B-scores and F-scores
    ## these averages will be weighted by stock landings and have cod condition penalty applied
    left_join(status_scores, by = c("region_id", "year", "stock")) %>%
    filter(!is.na(score)) %>% # remove missing data

    ## apply penalty because bad cod condition in eastern baltic
    left_join(cod_penalty, by = c("year", "region_id", "stock")) %>%
    mutate(penalty = ifelse(
      is.na(penalty_factor) & !str_detect(stock, "cod.*32"),
      1, penalty_factor
    )) %>%
    # is.na(penalty_factor), 1, penalty_factor)) %>%
    dplyr::select(-penalty_factor) %>%
    mutate(score = penalty*score)

  ## calculate BHI scores, using stocks by region
  ## scores are geometric mean weighted by stock proportions of total catch in each region
  status_scores <- status_with_penalty %>%
    dplyr::filter(!is.na(score)) %>% # remove missing data
    dplyr::group_by(region_id, year) %>%
    dplyr::summarize(status_prop = prod(score^propCatch)) %>%
    dplyr::ungroup() %>%
    data.frame()


  ## TREND

  trend_years <- (scen_year-4):scen_year

  trend <- status_scores %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(year %in% trend_years) %>%
    dplyr::do(mdl = lm(status_prop ~ year, data = .)) %>%
    dplyr::summarize(
      region_id = region_id,
      trend = coef(mdl)["year"] * 5
    ) %>% # trend multiplied by 5 to predict 5 yrs out
    dplyr::ungroup() %>%
    dplyr::mutate(trend = round(trend, 2))

  scores <- rbind(
    ## status scores
    status_scores %>%
      dplyr::filter(year == scen_year) %>%
      dplyr::mutate(status = round(status_prop * 100, 1)) %>%
      dplyr::select(region_id, score = status) %>%
      dplyr::mutate(dimension = "status", goal = "FIS"),
    ## trend scores
    trend %>%
      dplyr::select(region_id, score = trend) %>%
      dplyr::mutate(dimension = "trend", goal = "FIS")
  )

  return(scores)

} ## End FIS function

MAR <- function(layers){

  ## From code in 'functions.R MAR' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year

  mar_status <- data.frame(
    region_id = seq(1, 42, 1),
    goal = "MAR",
    dimension = "status",
    score = rep(NA, 42)
  )

  mar_trend <- data.frame(
    region_id = seq(1, 42, 1),
    goal = "MAR",
    dimension = "trend",
    score = rep(NA, 42)
  )

  scores <- dplyr::bind_rows(mar_status, mar_trend)

  return(scores)


} # End MAR function

FP <- function(layers, scores){

  ## From code in 'functions.R FP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  ## layers needed: fisheries landings and mariculture harvest ----
  mar_harvest_tonnes <- AlignDataYears(layer_nm="mar_harvest", layers_obj=layers) %>%
    select(year = scenario_year, region_id, produced_tonnes)

  fis_landings <- AlignDataYears(layer_nm="fis_landings", layers_obj=layers) %>%
    select(year = scenario_year, region_id, stock, landings)


  ## wrangle and join mar and fis data ----

  ## before joining the fis and mar datasets, need to estimate landings by region:
  ## in fis data prep the ICES assessment area values were assigned to each bhi region,
  ## because fis goal uses ratios not values
  fis_landings <- read.csv(file.path(dir_assess, "layers", "rgns_complete.csv")) %>%
    select(region_id, region_name, region_area_km2) %>%
    right_join(fis_landings, by = "region_id") %>%
    group_by(year, stock) %>%
    mutate(stock_assess_area = sum(region_area_km2)) %>%
    ungroup() %>%
    mutate(rgn_landings = landings*(region_area_km2/stock_assess_area))


  fp_data <- fis_landings %>%
    select(region_id, region_name, year, stock, rgn_landings) %>%
    group_by(region_id, region_name, year) %>%
    summarize(rgn_landings = sum(rgn_landings)) %>%
    ungroup() %>%
    left_join(mar_harvest_tonnes, by = c("year", "region_id"))


  ## calculate ratio of wildcaught fisheries to mariculture harvest ----

  ## only since 1991 have all stocks been reported/recorded in our dataset...
  ## thus, will set ratio between MAR and FIS to the default 1/2 for these years
  # fis_landings %>%
  #   group_by(year) %>%
  #   summarize(numstock = n_distinct(stock)) %>%
  #   filter(numstock == 6)


  fp_weights <- fp_data %>%
    mutate(prop_wildcaught = rgn_landings/(rgn_landings + pmax(0, produced_tonnes))) %>%
    mutate(prop_wildcaught = ifelse(is.na(produced_tonnes), 1, prop_wildcaught)) %>%
    ## if have some mariculture data for before 1991, will use 1991 ratio for the region
    mutate(ratio1991 = prop_wildcaught*(year == 1991)) %>%
    group_by(region_id, region_name) %>%
    mutate(ratio1991 = max(ratio1991)) %>%
    ungroup() %>%
    mutate(prop_wildcaught = ifelse(
      year < 1991 & !is.na(produced_tonnes),
      ratio1991, prop_wildcaught)
    ) %>%
    select(region_id, year, prop_wildcaught)

  ## save ratio of fis vs mar production as intermediate result
  write_csv(
    fp_weights,
    file.path(dir_assess, "intermediate", "wildcaught_weight.csv")
  )

  fp_weights <- fp_weights %>%
    filter(year == scen_year) %>%
    select(-year)


  ## scores of FIS and MAR with appropriate weight ----
  fp_scores <- scores %>%
    dplyr::filter(
      goal %in% c("FIS", "MAR"),
      dimension %in% c("status", "trend", "future", "score")
    ) %>%
    dplyr::left_join(fp_weights, by = "region_id")  %>%
    dplyr::mutate(prop_mar_harvest = 1 - prop_wildcaught) %>%
    dplyr::mutate(weight = ifelse(goal == "FIS", prop_wildcaught, prop_mar_harvest))


  ## warning messages for potential data mismatches ----
  ## NA score but there is a weight
  tmp <- fp_scores %>%
    dplyr::filter(
      goal == "FIS" &
        is.na(score) &
        dimension == "score" &
        (!is.na( weight) &  weight != 0)
    )
  if(dim(tmp)[1] > 0){
    warning(sprintf(
      "Check: these regions have a FIS weight but no score: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <- fp_scores %>%
    dplyr::filter(
      goal == "MAR" &
        is.na(score) &
        dimension == "score" &
        (!is.na(1- weight) & (1- weight) != 0)
    )
  if(dim(tmp)[1] > 0){
    warning(sprintf(
      "Check: these regions have a MAR weight but no score: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  ## there is a score, but weight is NA or 0
  tmp <- fp_scores %>%
    dplyr::filter(
      goal == "FIS" &
        dimension == "score" &
        region_id != 0 &
        (!is.na(score) & score > 0) &
        (is.na( weight) | weight == 0)
    )
  if(dim(tmp)[1] > 0){
    warning(sprintf(
      "Check: these regions have a FIS score but no weight: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <- fp_scores %>%
    dplyr::filter(
      goal == "MAR" &
        (!is.na(score) & score > 0) &
        (is.na(1- weight) | (1- weight) == 0) &
        dimension == "score" &
        region_id != 0
    )
  if(dim(tmp)[1] > 0){
    warning(sprintf(
      "Check: these regions have a MAR score but no weight: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }


  ## summarize scores for FP ----
  ## FP scores are based on MAR, and FIS weights
  scores_fp <- fp_scores %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score)

  return(rbind(scores, scores_fp))

} ## End FP function

NP <- function(layers){

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


  bbmsy <- AlignDataYears(layer_nm="np_bbmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="bbmsy")

  ffmsy <- AlignDataYears(layer_nm="np_ffmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="ffmsy")

  landings <- AlignDataYears(layer_nm="np_landings", layers_obj=layers)


  ## combine bbmsy and ffmsy into single object
  metric_scores <- rbind(
    dplyr::select(bbmsy, -np_bbmsy_year, -layer_name, year = scenario_year),
    dplyr::select(ffmsy, -np_ffmsy_year, -layer_name, year = scenario_year)
  )
  metric_scores <- metric_scores %>%
    dplyr::mutate(metric = as.factor(metric)) %>%
    tidyr::pivot_wider(names_from = metric, values_from = value)


  ## BSCORES
  ## STEP 1: converting B/Bmsy and F/Fmsy to B-scores ----
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
  ## STEP 2: converting B/Bmsy and F/Fmsy to F-scores ----

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


  ## STEP 3: averaging the F and B-scores to get the stock status score ----

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
    dplyr::select(year = scenario_year, region_id, stock, landings) %>%
    dplyr::filter(year %in% (max(year)-9):max(year))

  ## we use the average catch for each stock/region across the last 25 years to obtain weights
  ## (each region/stock will have the same average catch across years)
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

  ## join scores and weights to calculate status
  status_scores_weights <- weights %>%
    ## start with overall score as an average of B-scores and F-scores
    ## these averages will be weighted by stock landings and have cod condition penalty applied
    left_join(status_scores, by = c("region_id", "year", "stock")) %>%
    filter(!is.na(score)) # remove missing data


  ## STEP 5: calculate and return status and trend ----

  ## calculate BHI scores, using stocks by region (though currently NP data consists of only one stock)
  ## scores are geometric mean weighted by stock proportions of total catch in each region
  status <- status_scores_weights %>%
    dplyr::filter(!is.na(score)) %>% # remove missing data
    dplyr::group_by(region_id, year) %>%
    dplyr::summarize(status_prop = prod(score^propCatch)) %>%
    dplyr::ungroup() %>%
    data.frame()

  ## trend calculations
  ## could replace trend calc w/ ohicore::CalculateTrend, but then coeff is normalized by min trend year...
  ## need more years of data in scenario_data_years though for this to work
  # trend <- ohicore::CalculateTrend(status, trend_years)

  trend_years <- (max(status$year)-4):max(status$year)

  trend <- status %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(year %in% trend_years) %>%
    dplyr::do(mdl = lm(status_prop ~ year, data = .)) %>%
    dplyr::summarize(
      region_id = region_id,
      trend = coef(mdl)["year"] * 5
    ) %>% # trend multiplied by 5 to predict 5 yrs out
    dplyr::ungroup() %>%
    dplyr::mutate(trend = round(trend, 2))

  scores <- rbind(
    ## status scores
    status %>%
      dplyr::filter(year == scen_year) %>%
      dplyr::mutate(status = round(status_prop * 100, 1)) %>%
      dplyr::select(region_id, score = status) %>%
      tidyr::complete(region_id = full_seq(c(1, 42), 1)) %>%
      dplyr::mutate(dimension = "status"),

    ## trend scores
    trend %>%
      dplyr::select(region_id, score = trend) %>%
      tidyr::complete(region_id = full_seq(c(1, 42), 1)) %>%
      dplyr::mutate(dimension = "trend")

    ) %>% dplyr::mutate(goal = "NP")

  return(scores)

} ## End NP function

AO <- function(layers){

  ## From code in 'functions.R AO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  ao_status <- AlignDataYears(layer_nm="ao_stock_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Trend ----

  ao_trend <- AlignDataYears(layer_nm="ao_stock_slope", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Return artisial fishing opportunities status and trend scores ----

  ao_status_and_trend <- dplyr::bind_rows(
    mutate(ao_status, goal = "AO"),
    mutate(ao_trend, goal = "AO")
  )
  scores <- select(ao_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End AO function

BD <- function(layers){

  ## From code in 'functions.R BD' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## BD goal currently has no subgoals, though it contains info on species (SPP) and habitat (HAB)
  ## status is the geometric mean of each biodiversity component:
  ## benthic and pelagic habitats, fish, seals, and birds (wintering and breeding combined)

  ## biodiversity trend is temporarily substituted by OHI-global (2019) BD trend scores


  scen_year <- layers$data$scenario_year

  ## wrangle birds first, need to summarize across feeding and wintering vs breeding groups
  bd_spp_birds <- ohicore::AlignDataYears(layer_nm="bd_spp_birds", layers_obj=layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(helcom_id = as.character(helcom_id)) %>%
    mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
    select(-scenario_year)

  birds_spatial_units <- distinct(bd_spp_birds, region_id, spatial_group)
  birds <- bd_spp_birds %>%
    group_by(helcom_id, feeder_group) %>%
    mutate(helcom_id_area_km2 = sum(area_km2)) %>%
    ## each helcom_id region has 10 groups - 5 feeding groups, for each wintering and breeding
    ## what kind of average to use?? will use arithmetic mean of BQRs across groups for now...
    ## note: only benthic feeders are assessed for the open sea areas
    distinct(helcom_id, coastal, spatial_group, feeder_group, BQR, helcom_id_area_km2) %>%
    group_by(coastal, spatial_group, helcom_id_area_km2) %>%
    summarize(averageBQR = mean(BQR, na.rm = TRUE)) %>%
    ungroup() %>%
    ## will take area weighted means of BQRs within coastal areas of the spatial assessment area
    group_by(coastal, spatial_group) %>%
    summarize(averageBQR = weighted.mean(averageBQR, helcom_id_area_km2)) %>%
    ungroup() %>%
    ## combine coastal and open sea with equal weights rather than area-weighted average
    group_by(spatial_group) %>%
    summarize(averageBQR = mean(averageBQR, na.rm = TRUE)) %>%
    ungroup() %>%
    ## now rejoin the BHI regions based on which spatial unit they are within
    right_join(birds_spatial_units, by = "spatial_group") %>%
    mutate(indicator = "spp_birds") %>%
    select(-spatial_group)



  ## other layers are already ready to merge and summarize

  bd_all_data <- bind_rows(
    ## benthic habitat
    AlignDataYears(layer_nm="bd_hab_benthic", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "hab_benthic"),
    ## pelagic habitat
    AlignDataYears(layer_nm="bd_hab_pelagic", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "hab_pelagic"),
    ## fishes
    AlignDataYears(layer_nm="bd_spp_fish", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "spp_fishes"),
    ## seals
    AlignDataYears(layer_nm="bd_spp_seals", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "spp_seals")
  )


  ## will use geometric mean to represent how components are not interchangeable
  ## https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
  gm_mean <- function(x, na.rm = TRUE, zero.propagate = FALSE){
    if(any(x < 0, na.rm = TRUE)){
      return(NaN)
    }
    if(zero.propagate){
      if(any(x == 0, na.rm = TRUE)){
        return(0)
      }
      exp(mean(log(x), na.rm = na.rm))
    } else {
      exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
    }
  }


  ## Calculate Status ----

  ## at BQR of 0.6 is the reference point except for birds, which have a ref point of 0.75
  ## will rescale so BQR = 0.6 or above is a status of 1, while BQR = 0 is a status of zero
  bd_status <- bd_all_data %>%
    group_by(region_id, coastal, indicator) %>%
    summarize(averageBQR = weighted.mean(BQR, area_km2, na.rm = TRUE)) %>%
    ungroup() %>%
    ## combine coastal and open sea with equal weights rather than area-weighted average
    group_by(region_id, indicator) %>%
    summarize(averageBQR = mean(averageBQR, na.rm = TRUE)) %>%
    ungroup() %>%
    ## bind birds data with other biodiversity layers
    bind_rows(birds) %>%
    mutate(averageBQR = ifelse(is.nan(averageBQR), NA, averageBQR)) %>%
    ## calculate the status for individual indicators
    mutate(indicator_status = case_when(
      indicator == "spp_birds" ~ pmin(1, (4/3)*averageBQR),
      indicator != "spp_birds" ~ pmin(1, (5/3)*averageBQR)
    )) %>%
    mutate(indicator_status= 100*indicator_status) %>%
    select(-averageBQR)

  ## save individual indicators as intermediate results
  for(ind in unique(bd_status$indicator)){
    savefp <- file.path(
      dir_assess, "intermediate",
      sprintf("%s.csv", ind)
    )
    if(!file.exists(savefp)){
      write_csv(
        bd_status %>%
          filter(indicator == ind) %>%
          select(region_id, indicator_status),
        savefp
      )
    }
  }

  ## calculate overall biodiversity status scores
  bd_status <- bd_status %>%
    group_by(region_id) %>%
    summarize(status = gm_mean(indicator_status)) %>%
    ungroup() %>%
    rename(score = status)


  ## Trend ----

  bd_trend <- AlignDataYears(layer_nm="bd_spp_trend", layers_obj=layers) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id, score) %>%
    mutate(score = round(score, 3))


  ## Return biodiversity status and trend scores ----

  bd_status_and_trend <- dplyr::bind_rows(
    mutate(bd_status, dimension = "status"),
    mutate(bd_trend, dimension = "trend")
  )

  scores <- bd_status_and_trend %>%
    dplyr::mutate(goal = "BD") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)

} ## End BD Function

ICO <- function(layers){

  ## From code in 'functions.R ICO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  ico_subbasin_assessments <- AlignDataYears(layer_nm="sp_ico_assessments", layers_obj=layers) %>%
    ## scenario year here is the assessment_year_red_list:
    ## because of infrequent iucn assessments, there will inevitably be a lag in iconic species status...
    rename(year = scenario_year)


  ## Create vulnerability lookup table ----
  vuln_lookup <- mutate(
    data.frame(
      red_list_category = c(
        "EX - Extinct",
        "RE - Regionally Extinct",
        "CR - Critically Endangered",
        "EN - Endangered",
        "VU - Vulnerable",
        "NT - Near Threatened",
        "LC - Least Concern"
      ),
      stringsAsFactors = FALSE
    ),
    red_list_category_numeric = case_when(
      red_list_category == "EX - Extinct" ~ 1,
      red_list_category == "RE - Regionally Extinct" ~ 1,
      red_list_category == "CR - Critically Endangered" ~ 0.8,
      red_list_category == "EN - Endangered" ~ 0.6,
      red_list_category == "VU - Vulnerable" ~ 0.4,
      red_list_category == "NT - Near Threatened" ~ 0.2,
      red_list_category == "LC - Least Concern" ~ 0
    )
  )
  ## join vulnerability score to subbasin assessments dataframe
  ## filter where red_list_category_numeric is NA, as these correspond to unused categories:
  ## NA - Not Applicable, NE - Not Evaluated, DD - Data Deficient
  ico_assess_w_num <- left_join(ico_subbasin_assessments, vuln_lookup, by = "red_list_category") %>%
    filter(!is.na(red_list_category_numeric))



  ## Calculate Status ----

  ico_status <- ico_assess_w_num %>%
    ## 1:
    ## for each species, take only values from most recent redlist assessment
    ## s.t. the year is less than or equal to the scenario year being evaluated...
    filter(year <= scen_year) %>%
    group_by(scientific_name) %>%
    filter(year == max(year)) %>%
    ungroup() %>%

    ## 2:
    ## sum threat weights for each region and taxa (species_group),
    ## normalizing by number distinct spp
    group_by(region_id, species_group, red_list_category_numeric) %>%
    summarize(nspp = n()) %>%
    ungroup() %>%
    group_by(region_id, species_group) %>%
    rename(weights = red_list_category_numeric) %>%
    summarise(
      wi_spp = sum(weights*nspp)/sum(nspp),
      nspp = sum(nspp)
    ) %>%
    ungroup() %>%
    mutate(status_taxa_initial = (1 - wi_spp)) %>%

    ## 3:
    ## summarize status values across taxa by region, using geometric mean
    ## normalized by numbers of species in each species group
    group_by(region_id) %>%
    summarize(status = 100*round(exp(weighted.mean(log(status_taxa_initial), nspp, na.rm = TRUE)), 2))



  ## Trend ----

  ## for now have NA iconic species trends...
  ico_trend <- data.frame(
    region_id = seq(1, 42, 1),
    score = rep(NA, 42)
  )

  ## Return iconic species status and trend scores ----

  ico_status_and_trend <- dplyr::bind_rows(
    ico_status %>%
      rename(score = status) %>%
      mutate(dimension = "status", goal = "ICO"),
    ico_trend %>%
      mutate(dimension = "trend", goal = "ICO")
  )
  scores <- select(ico_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End ICO function

LSP <- function(layers){

  ## From code in 'functions.R LSP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  lsp_status <- AlignDataYears(layer_nm="lsp_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Trend ----

  lsp_trend <- AlignDataYears(layer_nm="lsp_trend", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Return lasting special places status and trend scores ----

  lsp_status_and_trend <- dplyr::bind_rows(
    mutate(lsp_status, goal = "LSP"),
    mutate(lsp_trend, goal = "LSP")
  )
  scores <- select(lsp_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End LSP function

SP <- function(layers, scores){

  ## From code in 'functions.R SP' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year


  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP

  sp_scores <- scores %>%
    dplyr::filter(
      goal %in% c("ICO", "LSP"),
      dimension %in% c("status", "trend", "future", "score")
    ) %>%
    group_by(region_id, dimension) %>%
    summarize(score = mean(score, na.rm = TRUE)) %>%
    mutate(goal = "SP") %>%
    ungroup() %>%
    dplyr::select(region_id, goal, dimension, score)

  return(rbind(scores, sp_scores))

} ## End SP function

CON <- function(layers){

  scen_year <- layers$data$scenario_year

  rgns_complete <- read.csv(file.path(dir_assess, "layers", "rgns_complete.csv")) %>%
    select(region_id, subbasin, eez, subbasin_id)

  ## From code in 'functions.R CON' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()
  ## Also see BHI2.0 contaminants data prep for more information and visuzlization

  ## see how scores and trends are calculated in prep file (bhi-prep repository, 'CW/contaminants' subfolders)

  ## there are 3 indicators for contaminants:
  ## PCBs and Dioxins (including dioxin-like PCBs) measured in each biota and sediment, and PFOS measured only in biota
  ## the indicators will be averaged (arithmetic mean) for status and trend (if can calc. trend for more than PCB indicator)
  ## status of the combined indicator will be multiplied by concerning substances indicator

  con_indicators <- function(layer, yrs, bio_thresh, sed_thresh){

    result <- list()


    ## STATUS CALCULATIONS ----
    ## calculate status by subbasins
    basin_matrix_status <- layer %>%
      filter(year %in% yrs) %>%
      ## NOTE: CHANGE METHOD FROM BHI1.0
      ## compare to reference point and cap at one before averaging concentrations,
      ## effectively giving more weight to the few higher-concentration values
      mutate(
        health_threshold = ifelse(stringr::str_detect(matrix, "^bio"), bio_thresh, sed_thresh),
        ratio = value/health_threshold,
        ## using pmin to cap the status at one
        dateloc_status = pmin(1, 1/ratio)
      ) %>%
      ## join regions info for grouping by subbasins
      left_join(rgns_complete, by = "region_id") %>%
      group_by(subbasin_id, matrix) %>%
      summarize(
        ## number of date-location specific (aggregated) data points contributing to status
        ## plus some other summary statisics of interest
        num_dateloc_pts = n(),
        status = mean(dateloc_status, na.rm = TRUE),
        max_dateloc_pts = max(value, na.rm = TRUE),
        min_dateloc_pts = min(value, na.rm = TRUE)
      ) %>%
      ungroup()

    result[["basin_status"]] <- basin_matrix_status %>%
      group_by(subbasin_id) %>%
      summarize(score = mean(status, na.rm = TRUE)) %>% # status from biota & sediment data combined w equal wgt
      right_join(rgns_complete, by = "subbasin_id") %>%
      mutate(dimension = "status") %>%
      ## region 36 is archipelago sea not åland subbasin;
      ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
      mutate(score = ifelse(region_id == 36, NA, score))

    result[["basin_matrix_status"]] <- basin_matrix_status %>%
      right_join(
        rgns_complete %>%
          select(subbasin_id, region_id) %>%
          mutate(matrix = list(c("bio", "sed"))) %>%
          tidyr::unnest(c(matrix)),
        by = c("matrix", "subbasin_id")
      ) %>%
      ## region 36 is archipelago sea not åland subbasin;
      ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
      mutate(status = ifelse(region_id == 36, NA, status))


    ## TREND CALCULATIONS ----
    trendlag = 5 # number of years into future to project trend
    trendyrs <- (max(yrs)-9):max(yrs) # ten years for trend calculation vs five for status


    ## trend approach: by observations scaled w.r.t. to reference point
    lm_estim <- layer %>%
      filter(year %in% trendyrs) %>%
      ## join rgns_complete info for grouping by subbasins
      left_join(rgns_complete, by = "region_id") %>%
      group_by(subbasin_id, matrix) %>%
      mutate(
        trendcalc_nyrs = n_distinct(year),
        num_obs = n()
      ) %>%
      ## how much data is enough data to calculate short-term trends?
      filter(trendcalc_nyrs >= 6|(trendcalc_nyrs >= 4 & num_obs > 8)) %>%
      do(trend_mdl = lm(value ~ year, data = .)) %>%
      mutate(
        health_threshold = ifelse(matrix == "bio", bio_thresh, sed_thresh),
        ## change w.r.t. health thresholds...
        expectedchange5yrs = coef(trend_mdl)["year"]*trendlag,
        trend_score = -expectedchange5yrs/health_threshold
      ) %>%
      ungroup()

    result[["basin_matrix_trends"]] <- lm_estim %>%
      select(-trend_mdl) %>%
      mutate(subbasin_id = as.numeric(subbasin_id)) %>%
      right_join(
        rgns_complete %>%
          select(subbasin_id, region_id) %>%
          mutate(matrix = list(c("bio", "sed"))) %>%
          tidyr::unnest(c(matrix)),
        by = c("matrix", "subbasin_id")
      ) %>%
      ## region 36 is archipelago sea not åland subbasin;
      ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
      mutate(trend_score = ifelse(region_id == 36, NA, trend_score))

    result[["basin_trend"]] <- lm_estim %>%
      select(-trend_mdl) %>%
      group_by(subbasin_id) %>%
      summarize(score = mean(trend_score, na.rm = TRUE) %>% round(3)) %>%
      ## constrain trend values between one and negative one
      mutate(score = ifelse(-1 <= score & score <= 1, score, score*abs(1/score))) %>%
      right_join(rgns_complete, by = "subbasin_id") %>%
      mutate(dimension = "trend") %>%
      ## region 36 is archipelago sea not åland subbasin;
      ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
      mutate(score = ifelse(region_id == 36, NA, score))


    ## return scores for bio and sed matrices and combined indicator scores
    matrix_scores <- dplyr::bind_rows(
      result$basin_matrix_status %>%
        mutate(dimension = "status") %>%
        select(region_id, subbasin_id, score = status, dimension, matrix),
      result$basin_matrix_trend %>%
        mutate(dimension = "trend") %>%
        select(region_id, subbasin_id, score = trend_score, dimension, matrix)
    )
    indicator <- dplyr::bind_rows(
      result$basin_status %>%
        select(region_id, subbasin_id, score, dimension) %>%
        mutate(matrix = "combined"),
      result$basin_trend %>%
        select(region_id, subbasin_id, score, dimension) %>%
        mutate(matrix = "combined")
    )

    return(list(indicator = indicator, matrix_scores = matrix_scores))
  }

  yrs <- (scen_year-6):(scen_year-1)

  ## three primary contaminants indicators ----
  pcb_indicator <- ohicore::AlignDataYears(layer_nm="cw_con_pcb", layers_obj=layers) %>%
    rename(year = scenario_year) %>%
    ## don't use archipelago sea data points...
    filter(region_id != 36) %>%
    con_indicators(yrs, bio_thresh = 75, sed_thresh = 4.1)

  pfos_indicator <- ohicore::AlignDataYears(layer_nm="cw_con_pfos", layers_obj=layers) %>%
    rename(year = scenario_year) %>%
    ## don't use archipelago sea data points...
    filter(region_id != 36) %>%
    con_indicators(yrs, bio_thresh = 9.1, sed_thresh = NULL)

  dioxin_indicator <- ohicore::AlignDataYears(layer_nm="cw_con_dioxin", layers_obj=layers) %>%
    rename(year = scenario_year) %>%
    ## don't use archipelago sea data points...
    filter(region_id != 36) %>%
    con_indicators(yrs, bio_thresh = 6.5, sed_thresh = 0.86)

  ## save individual indicators as intermediate results
  for(ind in c("pcb_indicator", "pfos_indicator", "dioxin_indicator")){
    write.table(
      bind_rows(get(ind)$indicator, get(ind)$matrix_scores) %>%
        select(region_id, score, dimension, matrix) %>%
        mutate(scen_year = scen_year),
      file.path(dir_assess, "intermediate", sprintf("%s.csv", ind)),
      append = file.exists(file.path(dir_assess, "intermediate", sprintf("%s.csv", ind))),
      sep = ",",
      row.names = FALSE
    )
  }

  pcb_indicator <- dplyr::mutate(pcb_indicator$indicator, indicator = "pcb")
  pfos_indicator <- dplyr::mutate(pfos_indicator$indicator, indicator = "pfos")
  dioxin_indicator <- dplyr::mutate(dioxin_indicator$indicator, indicator = "dioxin")

  ## join PCB, Dioxin and PFOS indicators, and take average
  cw_con <- dplyr::bind_rows(pcb_indicator, pfos_indicator, dioxin_indicator) %>%
    dplyr::group_by(subbasin_id, dimension) %>%
    dplyr::mutate(num_indicators = n()) %>%
    ## in first assessment, excluded where only had pcb indicator...
    # dplyr::filter(num_indicators > 1) %>%
    dplyr::summarise(score = ifelse(
      sum(is.na(score)) == n(), NA,
      mean(score, na.rm = TRUE)
    )) %>%
    dplyr::mutate(score = round(score, 3)) %>%
    ungroup() %>%
    ## rejoin with region ids so can integrate concerning substances indicator
    left_join(rgns_complete, by = "subbasin_id") %>%
    ## region 36 is archipelago sea not åland subbasin;
    ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
    ## once again, will set to NA here....
    mutate(score = ifelse(region_id == 36, NA, score))


  ## concerning substances indicator (previously a uniform baltic-wide penalty factor) ----
  concern_subst_layer <- ohicore::AlignDataYears(layer_nm="cw_con_penalty", layers_obj=layers) %>%
    rename(year = scenario_year) %>%
    ## look at monitoring only within time period of interest
    dplyr::filter(year %in% yrs) %>%
    mutate(substance = paste0(substance, "_monitored")) %>%
    tidyr::pivot_wider(names_from = "substance", values_from = "monitored")

  ## calculating emerging concerning substances indicator
  ## NOTE: not aggregating this indicator to basin level, because
  ## 1) as a binary indicator (monitored/not monitored) doesn't need as many points as the other indicators
  ## 2) monitoring and reporting decisions are more govt-specific, spatial interpolation makes less sense...
  num_substances <- length(grep("_monitored", colnames(concern_subst_layer)))

  concern_subst_indicator <- concern_subst_layer %>%
    group_by(region_id) %>%
    summarise_at(vars(ends_with("_monitored")), sum) %>%
    ungroup() %>%
    mutate_at(vars(ends_with("_monitored")), list(~ pmin(1, .))) %>%
    ## add columns on number substances and number monitored
    mutate(
      ## adding 3 because of the three monitored subtances quantified in the BHI
      ## (pcbs, pfos, and dioxins)
      num_substances = num_substances + 3,
      num_monitored = rowSums(.[grep("_monitored", names(.))]) + 3,
      proportion_monitored = round(num_monitored/num_substances, 2),
      dimension = "status"
    ) %>%
    select(region_id, dimension, proportion_monitored)

  ## incorporate concerning substances indicator
  cw_con_with_penalty <- cw_con %>%
    full_join(concern_subst_indicator, by = c("region_id", "dimension")) %>%
    mutate(score_w_penalty = ifelse(dimension == "trend", score, score*proportion_monitored*100)) %>%
    mutate(goal = "CON") %>%
    select(region_id, goal, dimension, score = score_w_penalty)

  scores <- cw_con_with_penalty

  return(scores)

} ## End CON Function

EUT <- function(layers){

  scen_year <- layers$data$scenario_year

  ## adjust subbasin_id values so joinable with helcom_id values
  rgns_complete <- read.csv(file.path(dir_assess, "layers", "rgns_complete.csv")) %>%
    select(region_id, subbasin_id)


  ## From code in 'functions.R EUT' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## see how scores and trends are calculated in prep file (bhi-prep repository, 'CW/eutrophication' subfolders)

  ## there are 5 indicators for eutrophication:
  ## Secchi depth, Chlorophyll A concentration, DIN and DIP  (dis. inorg. nitrogen and phosphorus), and Oxygen debt
  ## the indicators will be averaged (geometric mean) for status and trend

  eut_indicators <- function(layer, scen_year, ind, meanmetric, targets_df, no_coastal){

    spatialunits <- ifelse(no_coastal, "subbasin_id", "helcom_id")
    result <- list()


    ## AVERAGING FOR SEASONAL MEANS ----

    ## join regions info for grouping by subbasins
    ## also if there is no helcom_id column, add one with subbasin_id values
    layer <- left_join(layer, rgns_complete, by = "region_id")

    if(!"helcom_id" %in% names(layer)){
      layer <- layer %>%
        mutate(helcom_id = sprintf("SEA-0%s", substr(subbasin_id, 2, 3)))
    }

    ## for secchi, chl a, and dissolved inorganic nutrients,
    ## need to aggregate to seasonal averages:
    if(ind %in% c("secchi", "chla", "din", "dip")){

      ## Calculate mean monthly value for each month
      monthly_means <- layer %>%
        filter(str_detect(helcom_id, ifelse(no_coastal, "^SEA-", ".*"))) %>%
        group_by(!!!syms(union(spatialunits, c("subbasin_id", "year", "month")))) %>%
        summarise(
          month_mean = mean(indicator_value, na.rm = TRUE),
          month_sd = sd(indicator_value, na.rm = TRUE),
          n_pts = n()
        ) %>%
        mutate(month_sd = ifelse(is.na(month_sd), 0, month_sd)) %>%
        ungroup()

      ## Calculate seasonal mean for each year, with min max and 5-year moving average
      seasonal_means <- monthly_means %>%
        group_by(!!!syms(union(spatialunits, c("subbasin_id", "year")))) %>%
        summarise(
          season_mean = mean(month_mean, na.rm = TRUE),
          season_sd = sqrt(mean((month_sd^2), na.rm = TRUE)),
          n_pts = sum(n_pts)
        ) %>%
        ungroup() %>%
        arrange(!!!syms(union(spatialunits, c("subbasin_id", "year")))) %>%
        group_by(!!!syms(union(spatialunits, "subbasin_id"))) %>%
        mutate(moving_avg_5yr = zoo::rollapply(season_mean, 5, mean, na.rm = TRUE, align = "right", fill = NA)) %>%
        mutate(indicator_min = season_mean-season_sd, indicator_max = season_mean+season_sd) %>%
        ungroup()

    } else {
      seasonal_means <- filter(layer, str_detect(helcom_id, ifelse(no_coastal, "^SEA-", ".*")))
    }

    ## STATUS CALCULATIONS ----

    ## join with basin-specific eutrophication targets and calculate target-to-mean ratios
    status_all_yrs <- seasonal_means %>%
      mutate(subbasin_id = as.character(as.numeric(subbasin_id))) %>%
      rowwise() %>%
      mutate(join_column = ifelse(
        "helcom_id" %in% names(seasonal_means),
        as.character(helcom_id),
        sprintf("SEA-0%s", substr(subbasin_id, 2, 3))
      )) %>%
      left_join(targets_df, by = c("join_column" = "helcom_id")) %>%
      select(-join_column, meanmetric = meanmetric) %>%
      mutate(ratio = ifelse(ind == "secchi", meanmetric/target, target/meanmetric)) %>%
      mutate(status = pmin(1, ratio)) %>%
      ungroup()

    ## note: latest year of data is selected in each area, meaning status year may differ spatially...
    status_recent_yr <- status_all_yrs %>%
      group_by(!!!syms(union(spatialunits, "subbasin_id"))) %>%
      filter(year <= scen_year) %>%
      filter(year == max(year)) %>%
      ungroup()

    result[["basin_status"]] <- status_recent_yr %>%
      ## joining regions lookup expands from subbasins to one row per BHI region
      full_join(
        mutate(rgns_complete, subbasin_id = as.character(subbasin_id)),
        by = intersect(c("subbasin_id", "region_id"), names(status_recent_yr))
      ) %>%
      mutate(status = round(status*100, 2), dimension = "status") %>%
      dplyr::select(region_id, score = status, subbasin_id, dimension)

    ## TREND CALCULATIONS ----
    trendlag <- 5 # number of years into future to project trend
    trendyrs <- (scen_year-9):scen_year # ten years for trend calculation
    regr_length <- 10 # number of years to use for regression
    min_regr_length <- 5 # min actual number of years with data to use for regression


    colnames(status_all_yrs)[[grep("meanmetric", colnames(status_all_yrs))]] <- meanmetric

    lm_estim <- status_all_yrs %>%
      filter(year %in% trendyrs) %>%
      group_by(!!!syms(union(spatialunits, "subbasin_id"))) %>%
      ## calculate trend only if:
      ## have at least X yrs data (min_regr_length) in the last Y yrs (regr_length) of time series
      do(tail(., n = regr_length)) %>%
      do(data.frame(enough_data = ifelse(sum(!is.na(.$season_mean)) >= min_regr_length, TRUE, FALSE))) %>%
      right_join(
        status_all_yrs %>% filter(year %in% trendyrs),
        by = union(spatialunits, "subbasin_id")
      ) %>%
      group_by(!!!syms(union(spatialunits, c("subbasin_id", "enough_data", "target")))) %>%
      do(trend_mdl = lm(season_mean ~ year, data = .)) %>%
      mutate(
        expectedchange5yrs = coef(trend_mdl)["year"]*5,
        ## divide expected 5 year change in chla by respective basin target
        ## i.e get as proportion relative to target (target is status score of 100, so equivalent scales)
        ## then interpretation of trend is roughly expected (percent) change in status
        trend_score = ifelse(
          ind == "secchi",
          max(-1, min(1, expectedchange5yrs/target)),
          max(-1, min(1, -expectedchange5yrs/target))
      )) %>%
      filter(enough_data)


    if(!no_coastal){
      ## if calculating with COASTAL areas + targets and taking area-weighted mean:
      lm_estim <- lm_estim %>%
        left_join(select(coastal_wfd_regions, helcom_id, area_km2_wfd_coastal), by = "helcom_id") %>%
        group_by(subbasin_id) %>%
        summarize(trend_score = weighted.mean(trend_score, area_km2_wfd_coastal, na.rm = TRUE))
    }

    result[["basin_trend"]] <- lm_estim %>%
      ## joining regions lookup expands from subbasins to one row per BHI region
      full_join(mutate(rgns_complete, subbasin_id = as.character(subbasin_id))) %>%
      mutate(score = round(trend_score, 3), dimension = "trend") %>%
      select(score, subbasin_id, dimension, region_id)

    indicator <- dplyr::bind_rows(result$basin_status, result$basin_trend) %>%
      dplyr::mutate(indicator = ind)

    return(indicator)
  }


  ## Calculate the five contaminants indicators ----
  secchi_indicator <- ohicore::AlignDataYears(layer_nm="cw_eut_secchi", layers_obj=layers) %>%
    ## don't use archipelago sea data points...
    filter(region_id != 36) %>%
    rename(indicator_value = secchi_depth, year = scenario_year) %>%
    eut_indicators(
      scen_year,
      "secchi",
      "moving_avg_5yr",
      layers$data$cw_eut_targets %>%
        filter(indicator == "summer_secchi") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    ) %>%
    ## region 36 is archipelago sea not åland subbasin;
    ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
    mutate(score = ifelse(region_id == 36, NA, score))

  chla_indicator <- ohicore::AlignDataYears(layer_nm="cw_eut_chla", layers_obj=layers) %>%
    ## don't use archipelago sea data points...
    filter(region_id != 36) %>%
    rename(indicator_value = chla_conc, year = scenario_year) %>%
    eut_indicators(
      scen_year,
      "chla",
      "season_mean",
      layers$data$cw_eut_targets %>%
        filter(indicator == "summer_chla") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    ) %>%
    ## region 36 is archipelago sea not åland subbasin;
    ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
    mutate(score = ifelse(region_id == 36, NA, score))

  din_indicator <- ohicore::AlignDataYears(layer_nm="cw_eut_din", layers_obj=layers) %>%
    ## don't use archipelago sea data points...
    filter(region_id != 36) %>%
    mutate(winterofyear = ifelse(month == 12, scenario_year, scenario_year-1)) %>%
    rename(indicator_value = din_conc, year = winterofyear) %>%
    eut_indicators(
      scen_year,
      "din",
      "moving_avg_5yr",
      layers$data$cw_eut_targets %>%
        filter(indicator == "winter_DIN") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    ) %>%
    ## region 36 is archipelago sea not åland subbasin;
    ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
    mutate(score = ifelse(region_id == 36, NA, score))

  dip_indicator <- ohicore::AlignDataYears(layer_nm="cw_eut_dip", layers_obj=layers) %>%
    ## don't use archipelago sea data points...
    filter(region_id != 36) %>%
    mutate(winterofyear = ifelse(month == 12, scenario_year, scenario_year-1)) %>%
    rename(indicator_value = dip_conc, year = winterofyear) %>%
    eut_indicators(
      scen_year,
      "dip",
      "moving_avg_5yr",
      layers$data$cw_eut_targets %>%
        filter(indicator == "winter_DIP") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    ) %>%
    ## region 36 is archipelago sea not åland subbasin;
    ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
    mutate(score = ifelse(region_id == 36, NA, score))

  oxyg_indicator <- ohicore::AlignDataYears(layer_nm="cw_eut_oxydebt", layers_obj=layers) %>%
    ## don't include archipelago sea...
    filter(region_id != 36) %>%
    rename(season_mean = oxygendebt, year = scenario_year) %>%
    eut_indicators(
      scen_year,
      "oxydebt",
      "season_mean",
      layers$data$cw_eut_targets %>%
        filter(indicator == "oxyg_debt") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    ) %>%
    ## region 36 is archipelago sea not åland subbasin;
    ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
    mutate(score = ifelse(region_id == 36, NA, score))


  ## save individual indicators as intermediate results
  for(ind in c("secchi_indicator", "chla_indicator", "din_indicator", "dip_indicator", "oxyg_indicator")){
    write.table(
      get(ind) %>%
        mutate(scen_year = scen_year) %>%
        select(region_id, score, dimension, indicator, scen_year),
      file.path(dir_assess, "intermediate", sprintf("%s.csv", ind)),
      append = file.exists(file.path(dir_assess, "intermediate", sprintf("%s.csv", ind))),
      sep = ",",
      row.names = FALSE
    )
  }

  ## EUTROPHIATION SUB-GOAL SCORES ----
  ## join all five indicators, and take geometric mean
  eut_scores <- dplyr::bind_rows(secchi_indicator, chla_indicator, din_indicator, dip_indicator, oxyg_indicator) %>%
    select(subbasin_id, indicator, dimension, score) %>%
    distinct() %>%
    dplyr::group_by(subbasin_id, dimension) %>%
    dplyr::summarise(score = ifelse(
      sum(is.na(score)) == n(), NA,
      ## using an arithmetic mean here; maybe should use psych::geometric.mean
      ## then, would need to separate out and still use arithmetic mean for trend!
      mean(score, na.rm = TRUE)
    )) %>%
    dplyr::mutate(score = round(score, 2)) %>%
    ungroup() %>%
    ## rejoin with region ids
    full_join(mutate(rgns_complete, subbasin_id = as.character(subbasin_id))) %>%
    mutate(goal = "EUT", score = ifelse(is.nan(score), NA, score)) %>%
    select(region_id, goal, dimension, score)


  ## because most of archipelago sea is coastal, and coastal data were not used in this assessment
  ## will set archipelago sea (Åland sea Finland, BHI region 36) eutrophication scores to NA for now
  ## so as not to be misleading in presentation/visuzlization of results
  scores <- eut_scores %>%
    mutate(score = ifelse(region_id == 36, NA, score))


  return(scores)

} ## End EUT function

TRA <- function(layers){

  ## From code in 'functions.R TRA' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  tra_status <- ohicore::AlignDataYears(layer_nm="cw_tra_status", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score)


  ## Trend ----

  tra_trend <- ohicore::AlignDataYears(layer_nm="cw_tra_trend_scores", layers_obj=layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score)


  ## Return trash status and trend scores ----

  tra_status_and_trend <- dplyr::bind_rows(
    mutate(tra_status, dimension = "status", goal = "TRA"),
    mutate(tra_trend, dimension = "trend", goal = "TRA")
  ) %>%
  ## region 36 is archipelago sea not åland subbasin;
  ## because of issues with aggregating CON and EUT, will not include region 36 here
  ## (do not want region 36 to have Clean Waters score based only on Trash subgoal)
  ## will separate subbasins in future calculations, but for now, not calculating CW scores for region 36
  mutate(score = ifelse(region_id == 36, NA, score))

  scores <- select(tra_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End TRA function

CW <- function(layers, scores){

  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R CW' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years and layers$data$scenario_year

  ## see how scores and trends are calculated in prep files (bhi-prep repository 'CW' subfolders)
  ## status is calculated as geometric mean of EUT, CON, TRA status for most recent year, same for future and score
  ## trend is the arithmetic mean of EUT, CON, TRA trends because can not deal with 0 values in geometric mean
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
  ## function to deal with cases where want to take the arithmetic mean of a vector of NA values,
  ## will return NA instead of NaN
  mean_NAall <- function(x){
    if(sum(is.na(x)) == length(x)){
      mean_val <- NA
    } else { mean_val = mean(x, na.rm = TRUE)}

    return(mean_val)
  }

  ## calculate dimensions for CW goal, from scores subsetted to CW subgoals
  subsetscores <- scores %>%
    filter(
      goal %in% c("EUT", "TRA", "CON"),
      dimension %in% c("status", "trend", "future", "score")
    )

  cw_scores <- rbind(
    subsetscores %>%
      dplyr::filter(dimension %in% c("status", "future", "score")) %>%
      dplyr::group_by(region_id, dimension) %>%
      dplyr::summarize(score = round(geometric.mean2(score, na.rm = TRUE))) %>% # round status to 0 decimals
      ungroup(),
    subsetscores %>%
      dplyr::filter(dimension == "trend") %>%
      dplyr::group_by(region_id, dimension) %>%
      dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
      ungroup()) %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  return(rbind(scores, cw_scores))

} ## End CW function

TR <- function(layers){

  ## From code in 'functions.R TR' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep TR subfolders)
  ## possible alternative: base metric on EU blue growth reports?

  scen_year <- layers$data$scenario_year


  tr_accomm <- AlignDataYears(layer_nm="tr_accommodations", layers_obj=layers) %>%
    select(year = scenario_year, region_id, accom_per_area)

  tr_gva <- AlignDataYears(layer_nm="tr_coastal_tourism_gva", layers_obj=layers) %>%
    select(year = scenario_year, region_id, cntry_tourism_gva)

  coastal_tourism_value <- full_join(tr_accomm, tr_gva, by = c("year", "region_id")) %>%
    mutate(tourism_gva_per_accom = cntry_tourism_gva/accom_per_area)


  ## status ----

  ## scaling so that lower point is fixed
  ## max score of 1 corresponds to max ratio of revenue to coastal accomm. per area, across all data
  mn <- min(coastal_tourism_value$tourism_gva_per_accom, na.rm = TRUE)
  mx <- max(coastal_tourism_value$tourism_gva_per_accom, na.rm = TRUE)

  tr_status <- coastal_tourism_value %>%
    mutate(status = ((1-mn)/(mx-mn))*tourism_gva_per_accom + (1 - (1-mn)/(mx-mn)*mx))


  ## trend ----

  trendyrs <- (scen_year-4):scen_year

  tr_trend <- tr_status %>%
    filter(year %in% trendyrs) %>%
    group_by(region_id) %>%
    do(trend_mdl = lm(status ~ year, data = .)) %>%
    mutate(expectedchange5yrs = coef(trend_mdl)["year"]*5)


  ## return scores ----

  scores <- dplyr::bind_rows(
    tr_status %>%
      filter(year == scen_year) %>%
      mutate(score = status*100) %>%
      select(region_id, score) %>%
      tidyr::complete(region_id = 1:42) %>%
      mutate(dimension = "status", goal = "TR"),
    tr_trend %>%
      rename(score = expectedchange5yrs) %>%
      select(region_id, score) %>%
      tidyr::complete(region_id = 1:42) %>%
      mutate(dimension = "trend", goal = "TR")
  )

  return(scores)

} ## end TR function



CS <- function(layers){

  ## From code in 'functions.R CS' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## Status ----

  cs_status <- AlignDataYears(layer_nm="cs_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)


  ## Trend ----

  ## for now have NA carbon storage trends...
  cs_trend <- data.frame(
    region_id = seq(1, 42, 1),
    score = rep(NA, 42)
  )


  ## Return carbon storage status and trend scores ----

  cs_status_and_trend <- dplyr::bind_rows(
    mutate(cs_status, goal = "CS"),
    mutate(cs_trend, dimension = "trend", goal = "CS")
  )
  scores <- select(cs_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End CS function

LIV <- function(layers){

  ## From code in 'functions.R LIV' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## liv data layers include regional (by BHI region) and national employment rates
  regional_employ <- AlignDataYears(layer_nm="le_liv_regional_employ", layers_obj=layers) %>%
    select(year = scenario_year, bhi_employ_rate, region_id)
  country_employ <- AlignDataYears(layer_nm="le_liv_national_employ", layers_obj=layers) %>%
    select(year = scenario_year, employ_pop, region_id)

  liv_data <- full_join(regional_employ, country_employ, by = c("region_id", "year")) %>%
    mutate(bhi_employ_rate = bhi_employ_rate*100) %>%
    rename(country_employ_rate = employ_pop) %>%
    mutate(rgn_country_employ = bhi_employ_rate/country_employ_rate)

  ## some additional parameters
  rescale_between <- c(1, 0.1)
  countryemp_high <- max(liv_data$country_employ_rate, na.rm = TRUE)


  ## STATUS ----

  ## calculate livelihoods status using moving temporal reference point...
  liv_status <- liv_data %>%
    ## https://drsimonj.svbtle.com/running-a-model-on-separate-groups
    ## get max rgn:country employ by region in moving 5year windows
    tidyr::nest(data = c(year, bhi_employ_rate, country_employ_rate, rgn_country_employ)) %>%
    mutate(
      refpthigh = purrr::map(
        data, ~ zoo::rollapply(
          .$rgn_country_employ,
          5, max, na.rm = TRUE,
          align = "right", fill = NA
        )
      ),
      refptlow = purrr::map(
        data, ~ zoo::rollapply(
          .$rgn_country_employ,
          5, min, na.rm = TRUE,
          align = "right", fill = NA
        )
      )
    ) %>%
    tidyr::unnest(c(refpthigh, refptlow, data)) %>%
    mutate(
      refpthigh = ifelse(is.na(rgn_country_employ), NA, refpthigh),
      refptlow = ifelse(is.na(rgn_country_employ), NA, refptlow)
    ) %>%
    ## max rgn:country employ in moving 5year window across all regions
    ## (max of region maximums)
    group_by(year) %>%
    mutate(
      refpthigh = max(refpthigh, na.rm = TRUE),
      refptlow = min(refptlow, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(year >= min(year)+4) %>%

    ## now calculate status
    mutate(
      m = (rescale_between[1] - rescale_between[2])/(refpthigh-refptlow),
      b = (rescale_between[1] - m*refpthigh),
      scaledvalue = m*rgn_country_employ + b,
      emp_diff = (countryemp_high - country_employ_rate)/country_employ_rate,
      status = (scaledvalue)^emp_diff
    )


  ## TREND ----

  ## calculate liv trends from status timeseries
  ## trend based on current and four prior years
  trend_years <- (scen_year-4):scen_year

  ## trend multiplied by 5 to estimate 5 yrs out...
  liv_trend <- liv_status %>%
    dplyr::filter(year %in% trend_years, !region_id %in% c(19, 22, 30, 33)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::do(mdl = lm(status ~ year, data = .)) %>%
    dplyr::summarize(
      region_id = region_id,
      trend = coef(mdl)["year"] * 5
    ) %>%
    dplyr::ungroup() %>%
    left_join(
      liv_status %>%
        dplyr::filter(year == max(trend_years), !region_id %in% c(19, 22, 30, 33)) %>%
        select(region_id, status),
      by = "region_id"
    ) %>%
    ## calculate trend as percent change?
    mutate(trend = trend/status) %>%
    dplyr::mutate(trend = round(trend, 3))


  ## RETURN SCORES ----

  scores <- rbind(
    ## status scores
    liv_status %>%
      dplyr::filter(year == scen_year) %>%
      dplyr::select(region_id, score = status) %>%
      tidyr::complete(region_id = 1:42) %>%
      dplyr::mutate(dimension = "status", goal = "LIV", score = score*100),
    ## trend scores
    liv_trend %>%
      dplyr::select(region_id, score = trend) %>%
      tidyr::complete(region_id = 1:42) %>%
      dplyr::mutate(dimension = "trend", goal = "LIV")
  )

  return(scores)

} ## End LIV function

ECO <- function(layers){

  ## From code in 'functions.R ECO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  annual_growth_rates <- AlignDataYears(layer_nm="le_eco_bluegrowth_rates", layers_obj=layers) %>%
    select(year = scenario_year, region_id, sector, annual_growth_rate)
  annual_revenue_estimates <- AlignDataYears(layer_nm="le_eco_yearly_gva", layers_obj=layers) %>%
    select(year = scenario_year, region_id, sector, gva_sector_prop, country_gva_estim)

  eco_status <- full_join(annual_growth_rates, annual_revenue_estimates, by = c("region_id", "sector", "year")) %>%
    mutate(sector_status = case_when(
      annual_growth_rate <= -1.5 ~ 0,
      annual_growth_rate > -1.5 & annual_growth_rate < 1.5 ~ 100/3*annual_growth_rate + 50,
      annual_growth_rate >= -1.5 ~ 100
    )) %>%
    group_by(region_id, year) %>%
    summarize(status = weighted.mean(sector_status, gva_sector_prop, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(status = ifelse(is.nan(status), NA, status))


  ## can't really say anything about change in annual growth rate, only have one value for AGR...
  ## but trend also based on growth wouldnt really make sense in relation to the calculated status?

  scores <- rbind(
    ## status scores
    eco_status %>%
      dplyr::filter(year == scen_year) %>%
      dplyr::select(region_id, score = status) %>%
      dplyr::mutate(dimension = "status", goal = "ECO"),
    ## trend scores
    eco_status %>%
      dplyr::filter(year == scen_year) %>%
      dplyr::select(region_id, score = status) %>%
      dplyr::mutate(score = NA, dimension = "trend", goal = "ECO")
  )

  return(scores)

} ## End ECO function

LE <- function(layers, scores){

  ## From code in 'functions.R LE' of v2015 BHI assessment, see bhi-1.0-archive github repo

  scen_year <- layers$data$scenario_year

  le_scores <- scores %>%
    dplyr::filter(
      goal %in% c("LIV", "ECO"),
      dimension %in% c("status", "trend", "future", "score")
    ) %>%
    # reshape2::dcast(region_id + dimension ~ goal, value.var = "score") %>%
    # mutate(score = rowMeans(cbind(ECO, LIV), na.rm = TRUE)) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "LE") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score)

  return(rbind(scores, le_scores))

} ## End LE function

FinalizeScores <- function(layers, conf, scores){

  ## From code in 'functions.R FinalizeScores' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## modified from functions.R template to aggregte to EEZs and Subbasins of Baltic

  ## resilience and pressures for supragoals??
  supragoal_prs_res <- scores %>%
    left_join(conf$goals, by = "goal") %>%
    select("region_id", "dimension", "goal", "score", "parent", "weight") %>%
    filter(!(is.na(parent)|parent == "NA"), dimension %in% c("pressures", "resilience")) %>%
    group_by(region_id, parent, dimension) %>%
    summarize(score = weighted.mean(score, weight)) %>%
    select(region_id, dimension, goal = parent, score)

  scores <- bind_rows(scores, supragoal_prs_res)


  ## Calculate Scores for EEZs and SUBBASINS by area weighting ----

  ## regions_lookup_complete.csv does not need to be updated
  ## unless BHI regions are changed or additional subregions are created
  rgns_complete <- read.csv(file.path(dir_assess, "layers", "rgns_complete.csv")) %>%
    mutate(eez_id = case_when(
      eez == "Sweden" ~ 301,
      eez == "Denmark" ~ 302,
      eez == "Germany" ~ 303,
      eez == "Poland" ~ 304,
      eez == "Russia" ~ 305,
      eez == "Lithuania" ~ 306,
      eez == "Latvia" ~ 307,
      eez == "Estonia" ~ 308,
      eez == "Finland" ~ 309
    ))

  cat(sprintf("Calculating scores for EEZ and SUBBASIN AREAS by area weighting...\n"))


  ## For EEZs ----
  scores_eez <- scores %>%
    dplyr::filter(region_id %in% 1:42) %>%

    ## merge to the area (km2) of each region
    dplyr::left_join(rgns_complete, by = "region_id") %>%
    dplyr::group_by(goal, dimension, eez_id) %>%

    ## calculate weighted mean by area
    dplyr::summarise(score = weighted.mean(score, region_area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score)) %>%

    ## select eez ids
    dplyr::select(goal, dimension, score, region_id = eez_id)


  ## For SUBBASINS ----
  scores_subbasin <- scores %>%
    dplyr::filter(region_id %in% 1:42) %>%

    ## merge to the area (km2) of each region
    dplyr::left_join(rgns_complete, by = "region_id") %>%
    dplyr::group_by(goal, dimension, subbasin_id) %>%

    ## calculate weighted mean by area
    dplyr::summarise(score = weighted.mean(score, region_area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score)) %>%

    ## select subbasin ids
    dplyr::select(goal, dimension, score, region_id = subbasin_id)


  ## combine scores with EEZ and SUBBASIN scores ----
  scores <- bind_rows(scores, scores_eez, scores_subbasin)


  ## Add NAs to missing combos of region_id, goal, dimension ----
  explst <- list(
    region_id = c(0, 1:42, 301:309, 501:517),
    dimension = c("pressures", "resilience", "status", "trend", "future", "score"),
    goal = c(conf$goals$goal, "Index")
  )
  d <- expand.grid(explst, stringsAsFactors = FALSE)


  ## RETURN SCORES ----
  ## merge NAs dataframe with scores, and arrange
  scores <- dplyr::left_join(d, scores, by = c("goal",  "dimension", "region_id")) %>%
    dplyr::arrange(goal, dimension, region_id) %>%
    dplyr::mutate(score = ifelse(dimension == "trend", round(score, 3), round(score, 1))) %>%
    dplyr::mutate(score = ifelse(is.nan(score), NA, score))


  return(scores)

} ## End FinalizeScores function
