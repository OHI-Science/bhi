
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
  secchi_indicator <- AlignDataYears(layer_nm="cw_eut_secchi", layers_obj=layers) %>%
    rename(indicator_value = secchi_depth, year = scenario_year) %>%
    eut_indicators(
      scen_year,
      "secchi",
      "moving_avg_5yr",
      layers$data$cw_eut_targets %>%
        filter(indicator == "summer_secchi") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    )

  chla_indicator <- AlignDataYears(layer_nm="cw_eut_chla", layers_obj=layers) %>%
    rename(indicator_value = chla_conc, year = scenario_year) %>%
    eut_indicators(
      scen_year,
      "chla",
      "season_mean",
      layers$data$cw_eut_targets %>%
        filter(indicator == "summer_chla") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    )

  din_indicator <- AlignDataYears(layer_nm="cw_eut_din", layers_obj=layers) %>%
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
    )

  dip_indicator <- AlignDataYears(layer_nm="cw_eut_dip", layers_obj=layers) %>%
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
    )

  oxyg_indicator <- AlignDataYears(layer_nm="cw_eut_oxydebt", layers_obj=layers) %>%
    rename(season_mean = oxygendebt, year = scenario_year) %>%
    eut_indicators(
      scen_year,
      "oxydebt",
      "season_mean",
      layers$data$cw_eut_targets %>%
        filter(indicator == "oxyg_debt") %>%
        select(helcom_id, target = value),
      no_coastal = TRUE
    )


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
  scores <- dplyr::bind_rows(secchi_indicator, chla_indicator, din_indicator, dip_indicator, oxyg_indicator) %>%
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
  scores <- scores %>%
    mutate(score = ifelse(region_id == 36, NA, score))


  return(scores)

} ## End EUT function
