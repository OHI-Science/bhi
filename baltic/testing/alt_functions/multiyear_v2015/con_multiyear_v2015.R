
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
