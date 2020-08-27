
CON <- function(layers){

  scen_year <- layers$data$scenario_year

  rgns_complete <- file.path(dir_assess, "conf", "rgns_complete.csv") %>%
    read_csv() %>%
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
      mutate(dimension = "status")

    result[["basin_matrix_status"]] <- basin_matrix_status %>%
      right_join(
        rgns_complete %>%
          select(subbasin_id, region_id) %>%
          mutate(matrix = list(c("bio", "sed"))) %>%
          tidyr::unnest(c(matrix)),
        by = c("matrix", "subbasin_id")
      )


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
      )

    result[["basin_trend"]] <- lm_estim %>%
      select(-trend_mdl) %>%
      group_by(subbasin_id) %>%
      summarize(score = mean(trend_score, na.rm = TRUE) %>% round(3)) %>%
      ## constrain trend values between one and negative one
      mutate(score = ifelse(-1 <= score & score <= 1, score, score*abs(1/score))) %>%
      right_join(rgns_complete, by = "subbasin_id") %>%
      mutate(dimension = "trend")


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
  pcb_indicator <- AlignDataYears(layer_nm="cw_con_pcb", layers_obj=layers) %>%
    rename(year = scenario_year) %>%
    con_indicators(yrs, bio_thresh = 75, sed_thresh = 4.1)

  pfos_indicator <- AlignDataYears(layer_nm="cw_con_pfos", layers_obj=layers) %>%
    rename(year = scenario_year) %>%
    con_indicators(yrs, bio_thresh = 9.1, sed_thresh = NULL)

  dioxin_indicator <- AlignDataYears(layer_nm="cw_con_dioxin", layers_obj=layers) %>%
    rename(year = scenario_year) %>%
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
    left_join(rgns_complete, by = "subbasin_id")


  ## concerning substances indicator (previously a uniform baltic-wide penalty factor) ----
  concern_subst_layer <- AlignDataYears(layer_nm="cw_con_penalty", layers_obj=layers) %>%
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
