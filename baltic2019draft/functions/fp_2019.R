
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
