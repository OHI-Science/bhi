
FIS <- function(layers){

  ## From code in 'functions.R FIS' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scenario_year <- layers$data$scenario_year


  bbmsy <- AlignDataYears(layer_nm="fis_bbmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="bbmsy") %>%
    dplyr::rename(region_id = rgn_id)

  ffmsy <- AlignDataYears(layer_nm="fis_ffmsy", layers_obj=layers) %>%
    dplyr::mutate(metric="ffmsy") %>%
    dplyr::rename(region_id = rgn_id)

  landings <- AlignDataYears(layer_nm="fis_landings", layers_obj=layers) %>%
    dplyr::rename(region_id = rgn_id)

  cod_penalty <- AlignDataYears(layer_nm="fis_cod_penalty", layers_obj=layers) %>%
    dplyr::rename(region_id = rgn_id) %>%
    dplyr::mutate(
      stock = as.character(unique(bbmsy$stock)) %>%
        grep(pattern = "cod", value = TRUE) %>%
        grep(pattern = "32", value = TRUE)
    )

  ## combine bbmsy and ffmsy into single object

  metric_scores <- rbind(
    dplyr::select(bbmsy, -fis_bbmsy_year, -layer_name, year = scenario_year),
    dplyr::select(ffmsy, -fis_ffmsy_year, -layer_name, year = scenario_year)
  ) %>% dplyr::mutate(metric = as.factor(metric))

  metric_scores <- tidyr::spread(metric_scores, metric, score)


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


  ## STEP 5: calculate status and trend ----

  ## join scores and weights to calculate status
  status_with_penalty <- weights %>%

    ## start with overall score as an average of B-scores and F-scores
    ## these averages will be weighted by stock landings and have cod condition penalty applied
    left_join(status_scores, by = c("region_id", "year", "stock")) %>%
    filter(!is.na(score)) %>% # remove missing data

    ## apply penalty because bad cod condition in eastern baltic
    left_join(cod_penalty, by = c("year", "region_id", "stock")) %>%
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
    dplyr::summarize(
      region_id = region_id,
      trend = coef(mdl)["year"] * 5
    ) %>% # trend multiplied by 5 to predict 5 yrs out
    dplyr::ungroup() %>%
    dplyr::mutate(trend = round(trend, 2))

  scores <- rbind(
    ## status scores
    status_scores %>%
      dplyr::filter(year == scenario_year) %>%
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
