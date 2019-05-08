ECO = function(layers){

  ## Status model: Xeco = (GDP_Region_c/GDP_Region_r)/(GDP_Country_c/GDP_Country_r)

  ## Updated 7 July 2016, Jennifer Griffiths. If there is no data for status or trend, is NA


  # ## read in data
  # ## in data prep, year range included is selected
  #   ##if different time periods exisit for region and country, NAs for status will be produced
  # le_gdp_region   = SelectLayersData(layers, layers='le_gdp_region') %>%
  #   dplyr::select(rgn_id = id_num, year, rgn_gdp_per_cap = val_num)
  #
  # le_gdp_country  = SelectLayersData(layers, layers='le_gdp_country') %>%
  #   dplyr::select(rgn_id = id_num, year, nat_gdp_per_cap = val_num)
  #
  #
  # ## temp readin TODO: SelectLayers()
  # # library(dplyr)
  # # le_gdp_region = read.csv('~/github/bhi/baltic2015/layers/le_gdp_region_bhi2015.csv'); head(le_gdp_region)
  # # le_gdp_country = read.csv('~/github/bhi/baltic2015/layers/le_gdp_country_bhi2015.csv'); head(le_gdp_country)
  #
  # ## set lag window for reference point calculations
  # lag_win = 5  # 5 year lag
  # trend_yr = 4 # to select the years for the trend calculation, select most recent year - 4 (to get 5 data points)
  # bhi_rgn = data.frame(rgn_id = as.integer(seq(1,42,1))) #unique BHI region numbers to make sure all included with final score and trend
  #
  # ## ECO region: prepare for calculations with a lag
  # eco_region = le_gdp_region %>%
  #   dplyr::rename(gdp = rgn_gdp_per_cap) %>%
  #   filter(!is.na(gdp)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(gdp, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(rgn_value = gdp/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,rgn_value)
  #
  # ## ECO country
  # eco_country = le_gdp_country %>%
  #   dplyr::rename(gdp = nat_gdp_per_cap) %>%
  #   filter(!is.na(gdp)) %>%
  #   group_by(rgn_id)%>%
  #   mutate(year_ref = lag(year, lag_win, order_by=year),
  #          ref_val = lag(gdp, lag_win, order_by=year)) %>% #create ref year and value which is value 5 years preceeding within a BHI region
  #   arrange(year)%>%
  #   filter(year>= max(year)- lag_win)%>% #select only the previous 5 years from the max year
  #   ungroup() %>%
  #   mutate(cntry_value = gdp/ref_val) %>% #calculate rgn_value per year, numerator of score function
  #   select(rgn_id,year,cntry_value)
  #
  # ## calculate status
  # eco_status_calc = full_join(eco_region,eco_country, by=c("rgn_id","year"))%>% #join region and country current/ref ratios
  #              mutate(Xeco = rgn_value/cntry_value)%>% #calculate status
  #              mutate(status = pmin(1, Xeco)) # status calculated cannot exceed 1
  #
  # eco_status = eco_status_calc%>%
  #   group_by(rgn_id)%>%
  #   filter(year== max(year))%>%       #select status as most recent year
  #   ungroup()%>%
  #   full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for status
  #   mutate(score=round(status*100),   #scale to 0 to 100
  #          dimension = 'status')%>%
  #   select(region_id = rgn_id,score, dimension)
  #
  # ## this is where could change NA value of status to zero
  # #%>%
  # ##mutate(score= replace(score,is.na(score), 0)) #assign 0 to regions with no status calculated because insufficient or no data
  # ##will this cause problems if there are regions that should be NA (because indicator is not applicable?)
  #
  #
  # ## calculate trend for 5 years (5 data points)
  #     ## years are filtered in eco_region and eco_country, so not filtered for here
  #     eco_trend = eco_status_calc %>%
  #       filter(year >= max(year - trend_yr))%>%                #select five years of data for trend
  #       filter(!is.na(status)) %>%                              # filter for only no NA data because causes problems for lm if all data for a region are NA
  #       group_by(rgn_id) %>%
  #       mutate(regr_length = n())%>%                            #get the number of status years available for greggion
  #       filter(regr_length == (trend_yr + 1))%>%                   #only do the regression for regions that have 5 data points
  #         do(mdl = lm(status ~ year, data = .)) %>%             # regression model to get the trend
  #           summarize(rgn_id = rgn_id,
  #                     score = coef(mdl)['year'] * lag_win)%>%
  #       ungroup() %>%
  #       full_join(bhi_rgn, .,by="rgn_id")%>%  #all regions now listed, have NA for trend #should this stay NA?  because a 0 trend is meaningful for places with data
  #       mutate(score = round(score, 2),
  #              dimension = "trend") %>%
  #       select(region_id = rgn_id, dimension, score) %>%
  #       data.frame()

  ### updated 14Nov, 2016 by Ning Jiang from ohi-science team

  eco_status = layers$data[['eco_status']] %>%
    mutate(dimension = 'status') %>%
    dplyr::select(-layer)

  eco_trend = layers$data[['eco_trend']] %>%
    mutate(dimension = 'trend') %>%
    dplyr::select(-layer)

  ## create scores and rbind to other goal scores
  scores = rbind(eco_status, eco_trend) %>%
    mutate(goal='ECO') %>%
    dplyr::select(region_id = rgn_id,
                  score,
                  dimension,
                  goal)

  return(scores)

} ## End ECO function
