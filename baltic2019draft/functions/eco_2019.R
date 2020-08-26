
ECO <- function(layers){

  ## From code in 'functions.R ECO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year


  annual_growth_rates <- AlignDataYears(layer_nm="le_eco_bluegrowth_rates", layers_obj=layers)
  annual_revenue_estimates <- AlignDataYears(layer_nm="le_eco_yearly_gva", layers_obj=layers)

  eco_status <- full_join(annual_growth_rates, annual_revenue_estimates, by = c("region_id", "sector", "year")) %>%
    mutate(sector_status = case_when(
      annual_growth_rate <= -1.5 ~ 0,
      annual_growth_rate > -1.5 & annual_growth_rate < 1.5 ~ 100/3*annual_growth_rate + 50,
      annual_growth_rate >= -1.5 ~ 100
    )) %>%
    group_by(region_id, year) %>%
    summarize(status = weighted.mean(sector_status, gva_sector_prop, na.rm = TRUE)) %>%
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
