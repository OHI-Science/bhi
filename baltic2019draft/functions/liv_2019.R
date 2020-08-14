
LIV <- function(layers){

  ## From code in 'functions.R LIV' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  ## liv data layers include regional (by BHI region) and national employment rates
  regional_employ <- AlignDataYears(layer_nm="le_liv_regional_employ", layers_obj=layers)
  country_employ <- AlignDataYears(layer_nm="le_liv_national_employ", layers_obj=layers)

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
      dplyr::mutate(dimension = "status", goal = "LIV"),
    ## trend scores
    liv_trend %>%
      dplyr::select(region_id, score = trend) %>%
      tidyr::complete(region_id = 1:42) %>%
      dplyr::mutate(dimension = "trend", goal = "LIV")
  )

  return(scores)

} ## End LIV function
