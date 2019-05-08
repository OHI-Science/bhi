
AO <- function(layers){

  ## From code in 'functions.R AO' of v2015 BHI assessment
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  ao_stock_status <- AlignDataYears(layer_nm="ao_stock_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(scenario_year, rgn_id, dimension, score)
  ## status as NA indicates missing data, not that it is not applicable...

  ao_stock_slope <- AlignDataYears(layer_nm="ao_stock_slope", layers_obj=layers) %>%
    dplyr::mutate(dimension = as.character(dimension)) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(scenario_year, rgn_id, dimension, score)


  ## the AO goal defined by the OHI framework has 3 components: stock status, access, need
  ## currently the BHI only addresses the stock status

  ## STOCK STATUS
  ## status and trend slope are calculated in the ao_prep of the bhi-prep repo
  ## calculated on the HOLAS basin scale and applied to the BHI regions

  ## TREND
  ## the trend slope is not yet modified by the future year of interest (5 years from now)

  trend_n_years <- 5  # number of years for trend

  ao_stock_trend <- ao_stock_slope %>%
    dplyr::mutate(score = score * trend_n_years)


  ## join status and trend
  ao_stock <- dplyr::bind_rows(ao_stock_status, ao_stock_trend) %>%
    dplyr::rename(region_id = rgn_id)



  ## in future check whether data is available for other sub-componenets!
  ## if have access or need data layers, incorporate following example of ohi-global:
  # sustainability <- 1.0
  # r <- AlignDataYears(layer_nm="ao_access", layers_obj=layers) %>%
  #   dplyr::rename(region_id = rgn_id, access = value) %>%
  #   na.omit()
  #
  # ry <- AlignDataYears(layer_nm="ao_need", layers_obj=layers) %>%
  #   dplyr::left_join(r, by = c("region_id", "scenario_year")) %>%
  #   dplyr::mutate(Du = (1 - need) * (1 - access)) %>%
  #   dplyr::mutate(status = (1 - Du) * sustainability * 100)
  #
  # status <- ry %>%
  #   dplyr::filter(scenario_year == scen_year) %>%
  #   dplyr::select(region_id, score = status) %>%
  #   dplyr::mutate(dimension = "status")
  #
  # trend_years <- (scen_year - 4):(scen_year)
  # trend <- ohicore::CalculateTrend(status_data = ry, trend_years = trend_years)
  #
  # scores <- rbind(status, trend) %>%
  #   dplyr::mutate(goal = "AO")


  scores <- ao_stock %>%
    dplyr::select(region_id, dimension, score) %>%
    dplyr::mutate(goal = "AO") %>%
    dplyr::arrange(goal, region_id)

  return(scores)

} ## End AO function
