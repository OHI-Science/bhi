
MAR = function(layers){

  ## Based on code from 'functions.R MAR' of v2015 BHI assessment
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year
  regr_length <- 5 # number of years of data to use in trend calc
  regr_years <- seq(scen_year - regr_length + 1, scen_year)

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

  ## CALCULATE STATUS
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


  ## CALCULATE TREND
  mar_trend <- mar_status_score %>%
    dplyr::group_by(rgn_id) %>%
    dplyr::do({
      ## calculate trend iff have all yrs of data in last 5 (regr_length) years of time series
      ## future_year set in contants, this is the value 5 in the old code
      if(sum(!is.na(.$status)) >= regr_length)
        data.frame(trend_score = max(-1, min(1, coef(lm(status ~ scenario_year, .))["scenario_year"]*0.05)))
      else data.frame(trend_score = NA)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(trend_score = round(trend_score, 2)) %>%
    tidyr::complete(rgn_id = full_seq(c(1, 42), 1))


  ## ASSEMBLE DIMENSIONS & RETURN SCORES
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
