
FP <- function(layers, scores){
  scen_year <- layers$data$scenario_year

  ## From code in 'functions.R FP' of v2015 BHI assessment
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


  ## some warning messages for potential mismatches in data
  ## NA score but there is a weight
  tmp <- s %>% dplyr::filter(goal == "FIS" &
                               is.na(score) &
                               dimension == "score" &
                               (!is.na(w_fis) & w_fis != 0))
  if (dim(tmp)[1] > 0) {
    warning(sprintf(
      "Check: these regions have a FIS weight but no score: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <- s %>% dplyr::filter(goal == 'MAR' &
                               is.na(score) &
                               dimension == "score" &
                               (!is.na(w_mar) & w_mar != 0))
  if(dim(tmp)[1] > 0){
    warning(sprintf(
      "Check: these regions have a MAR weight but no score: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  ## there is a score, but weight is NA or 0
  tmp <- s %>% dplyr::filter(goal == "FIS" &
                               dimension == "score" &
                               region_id != 0 &
                               (!is.na(score) & score > 0) &
                               (is.na(w_fis) | w_fis == 0))
  if(dim(tmp)[1] > 0) {
    warning(sprintf(
      "Check: these regions have a FIS score but no weight: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <- s %>% dplyr::filter(goal == "MAR" &
                               (!is.na(score) & score > 0) &
                               (is.na(w_mar) | w_mar == 0) &
                               dimension == "score" &
                               region_id != 0)
  if(dim(tmp)[1] > 0) {
    warning(sprintf(
      "Check: these regions have a MAR score but no weight: %s",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }


  ## summarize scores as FP based on MAR, and FIS weight
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
