MAR = function(layers){
  ##updated by Jennifer Griffiths 25Feb2016
  ##updated by Julie 26Feb2016
  ##updated by Jennifer Griffiths 29Feb2016 - make sure mar_status_score limited to status_years, change layer names
  ##updated by Jennifer Griffiths 29March2016 - added code for temporal reference point but this is commented out until final decision made
  ##updated by Jennifer Griffiths 05April2016 - made reference point temporal (removed spatial), made data unit tons of production, not per capita
  ##updated by Jennifer Griffiths 16June2016 - change code so areas with no data are NA for status (not zero)
  ##updated by Ning Jiang 3Nov2016 - changed status function, and ref point to: max production of most recent five years has sust coef of 1

  ##layers used: mar_harvest_tonnes, mar_harvest_species, mar_sustainability_score


  ## select layers for MAR
  harvest_tonnes = SelectLayersData(layers, layers='mar_harvest_tonnes', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
                  species_code = category,
                  year,
                  tonnes = val_num)

  harvest_species = SelectLayersData(layers, layers='mar_harvest_species', narrow=T) %>%
    dplyr::select(species_code = category,
                  species = val_chr)

  sustainability_score = SelectLayersData(layers, layers='mar_sustainability_score', narrow=T) %>%
    dplyr::select(rgn_id = id_num,
                  species = category,
                  sust_coeff = val_num)


  ## SETTING CONSTANTS
  regr_length =5          # number of years to use for regression for trend.  Use this to replace reading in the csv file "mar_trend_years_gl2014"
  status_years = 2010:2014 #this was originally set in goals.csv

  #####----------------------######
  ##harvest_tonnes has years without data but those years not present with NAs
  ##spread and gather data again which will result in all years present for all regions
  harvest_tonnes=harvest_tonnes %>% spread(key=year,value=tonnes) %>%
    gather(year, tonnes, -rgn_id,-species_code) %>%
    mutate(year=as.numeric(year) )%>%  #make sure year is not a character
    arrange(rgn_id,year) %>%
    filter(year %in% status_years)

  # Merge harvest (production) data with sustainability score
  # ref_point = half of max production achieve sust coeff of 1
  temp = left_join(harvest_tonnes, harvest_species, by = 'species_code') %>%
    left_join(., sustainability_score, by = c('rgn_id', 'species')) %>%
    group_by(rgn_id, species_code) %>%
    mutate(tonnes_sust = tonnes * sust_coeff,
           ref_value = max(tonnes) * 1) %>%
    ungroup()

  ###----------------------------###
  ## Xmar = Mar_current / Mar_ref
  ## if use this, need to decide if the production should be scaled per capita

  ## Calculate status
  mar_status_score = temp %>% group_by(rgn_id)%>%
    mutate(status = pmin(1, tonnes_sust/ref_value) * 100)%>% #calculate status per year
    dplyr::select(rgn_id, year, status)%>%
    ungroup()

  mar_status = mar_status_score %>%
    filter(year == max(year)) %>%
    dplyr::select(rgn_id, score = status) %>%
    complete(rgn_id = full_seq(c(1, 42), 1))

  ###----------------------------###
  ## Calulate trend
  mar_trend= mar_status_score %>%
    group_by(rgn_id) %>%
    do(tail(. , n = regr_length)) %>% #select the years for trend calculate (regr_length # years from the end of the time series)
    #regr_length replaces the need to read in the trend_yrs from the csv file
    do({if(sum(!is.na(.$status)) >= 5)      #calculate trend only if X years of data (min_regr_length) in the last Y years of time serie (regr_length)
      data.frame(trend_score = max(-1, min(1, coef(lm(status ~ year, .))['year'] * 0.05))) #future_year set in contants, this is the value 5 in the old code
      else data.frame(trend_score = NA)}) %>%
    ungroup() %>%
    mutate(trend_score = round(trend_score,2)) %>%
    complete(rgn_id = full_seq(c(1, 42), 1))

  #####----------------------######
  # return MAR scores
  scores = mar_status %>%
    dplyr::select(region_id = rgn_id,
                  score     = score) %>%
    mutate(dimension='status') %>%
    rbind(
      mar_trend %>%
        dplyr::select(region_id = rgn_id,
                      score     = trend_score) %>%
        mutate(dimension = 'trend')) %>%
    mutate(goal='MAR')
  return(scores)


} #end MAR function
