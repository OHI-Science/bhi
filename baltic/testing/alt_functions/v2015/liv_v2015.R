LIV = function(layers){

  # ## Updated 11 July 2016 by Jennifer Griffiths
  #
  # ##-------------------------------------------------##
  # ## Select Layers
  # liv_regional_employ = SelectLayersData(layers, layers='liv_regional_employ') %>%
  #   dplyr::select(rgn_id = id_num, year,  employ_pop_bhi= val_num)
  #
  #
  # liv_national_employ = SelectLayersData(layers, layers='liv_national_employ') %>%
  #   dplyr::select(rgn_id = id_num, year,  employ_pop= val_num)
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ###Set parameters
  # ## set lag window for reference point calculations
  # lag_win = 5  # 5 year lag
  # trend_yr = 4 # to select the years for the trend calculation, select most recent year - 4 (to get 5 data points)
  # bhi_rgn = data.frame(rgn_id = as.integer(seq(1,42,1))) #unique BHI region numbers to make sure all included with final score and trend
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ### STATUS CALCULATION
  #
  # ### prepare region and country layers
  #
  # ## LIV region: prepare for calculations with a lag
  # liv_region = liv_regional_employ %>%
  #   dplyr::rename(employ = employ_pop_bhi) %>%
  #   filter(!is.na(employ)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(employ, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(rgn_value = employ/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,rgn_value) %>%
  #   arrange(rgn_id,year)
  #
  #
  # ## LIV country
  # liv_country =   liv_national_employ %>%
  #   dplyr::rename(employ = employ_pop) %>%
  #   filter(!is.na(employ)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(employ, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(cntry_value = employ/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,cntry_value) %>%
  #   arrange(rgn_id,year)
  #
  # ## Calculate status time series
  # ## calculate status
  # liv_status_calc = inner_join(liv_region,liv_country, by=c("rgn_id","year"))%>% #join region and country current/ref ratios ## inner_join because need to have both region and country values to calculate
  #   mutate(Xliv = rgn_value/cntry_value)%>% #calculate status
  #   mutate(status = pmin(1, Xliv)) # status calculated cannot exceed 1
  #
  # ## Extract most recent year status
  # liv_status = liv_status_calc%>%
  #   group_by(rgn_id)%>%
  #   filter(year== max(year))%>%       #select status as most recent year
  #   ungroup()%>%
  #   full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for status, this should be 0 to indicate the measure is applicable, just no data
  #   mutate(score=round(status*100),   #scale to 0 to 100
  #          dimension = 'status')%>%
  #   select(region_id = rgn_id,score, dimension) #%>%
  # ##mutate(score= replace(score,is.na(score), 0)) #assign 0 to regions with no status calculated because insufficient or no data
  # ##will this cause problems if there are regions that should be NA (because indicator is not applicable?)
  #
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ##  CALCULATE TREND
  #
  # ## calculate trend for 5 years (5 data points)
  # ## years are filtered in liv_region and liv_country, so not filtered for here
  # liv_trend = liv_status_calc %>%
  #   filter(year >= max(year - trend_yr))%>%                #select five years of data for trend
  #   filter(!is.na(status)) %>%                              # filter for only no NA data because causes problems for lm if all data for a region are NA
  #   group_by(rgn_id) %>%
  #   mutate(regr_length = n())%>%                            #get the number of status years available for greggion
  #   filter(regr_length == (trend_yr + 1))%>%                   #only do the regression for regions that have 5 data points
  #   do(mdl = lm(status ~ year, data = .)) %>%             # regression model to get the trend
  #   summarize(rgn_id = rgn_id,
  #             score = coef(mdl)['year'] * lag_win)%>%
  #   ungroup() %>%
  #   full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for trend #should this stay NA?  because a 0 trend is meaningful for places with data
  #   mutate(score = round(score, 2),
  #          dimension = "trend") %>%
  #   select(region_id = rgn_id, dimension, score) %>%
  #   data.frame()
  # ##-------------------------------------------------##
  #
  #
  # ##-------------------------------------------------##
  # ## FINAL SCORES OBEJCT
  # ## create scores and rbind to other goal scores

  ########### updated 10.28.2016 by Ning Jiang from ohi-science ##############

  liv_status = layers$data[['liv_status']] %>%
    dplyr::select(region_id = rgn_id, score) %>%
    mutate(dimension = "status")

  liv_trend = layers$data[['liv_trend']] %>%
    dplyr::select(region_id = rgn_id, score) %>%
    mutate(dimension = "trend")

  scores = liv_status %>%
    bind_rows(.,liv_trend) %>%
    mutate(goal='LIV')

  return(scores)

} ## End LIV function
