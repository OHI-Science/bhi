
BD <- function(layers){

  ## From code in 'functions.R BD' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## BD goal currently has no subgoals, though it contains info on species (SPP) and habitat (HAB)
  ## status is the geometric mean of each biodiversity component:
  ## benthic and pelagic habitats, fish, seals, and birds (wintering and breeding combined)

  ## biodiversity trend is temporarily substituted by OHI-global (2019) BD trend scores


  scen_year <- layers$data$scenario_year

  ## wrangle birds first, need to summarize across feeding and wintering vs breeding groups
  bd_spp_birds <- ohicore::AlignDataYears(layer_nm="bd_spp_birds", layers_obj=layers) %>%
    filter(scenario_year == scen_year) %>%
    mutate(helcom_id = as.character(helcom_id)) %>%
    mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
    select(-scenario_year)

  birds_spatial_units <- distinct(bd_spp_birds, region_id, spatial_group)
  birds <- bd_spp_birds %>%
    group_by(helcom_id, feeder_group) %>%
    mutate(helcom_id_area_km2 = sum(area_km2)) %>%
    ## each helcom_id region has 10 groups - 5 feeding groups, for each wintering and breeding
    ## what kind of average to use?? will use arithmetic mean of BQRs across groups for now...
    ## note: only benthic feeders are assessed for the open sea areas
    distinct(helcom_id, coastal, spatial_group, feeder_group, BQR, helcom_id_area_km2) %>%
    group_by(coastal, spatial_group, helcom_id_area_km2) %>%
    summarize(averageBQR = mean(BQR, na.rm = TRUE)) %>%
    ungroup() %>%
    ## will take area weighted means of BQRs within coastal areas of the spatial assessment area
    group_by(coastal, spatial_group) %>%
    summarize(averageBQR = weighted.mean(averageBQR, helcom_id_area_km2)) %>%
    ungroup() %>%
    ## combine coastal and open sea with equal weights rather than area-weighted average
    group_by(spatial_group) %>%
    summarize(averageBQR = mean(averageBQR, na.rm = TRUE)) %>%
    ungroup() %>%
    ## now rejoin the BHI regions based on which spatial unit they are within
    right_join(birds_spatial_units, by = "spatial_group") %>%
    mutate(indicator = "spp_birds") %>%
    select(-spatial_group)



  ## other layers are already ready to merge and summarize

  bd_all_data <- bind_rows(
    ## benthic habitat
    AlignDataYears(layer_nm="bd_hab_benthic", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "hab_benthic"),
    ## pelagic habitat
    AlignDataYears(layer_nm="bd_hab_pelagic", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "hab_pelagic"),
    ## fishes
    AlignDataYears(layer_nm="bd_spp_fish", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "spp_fishes"),
    ## seals
    AlignDataYears(layer_nm="bd_spp_seals", layers_obj=layers) %>%
      filter(scenario_year == scen_year) %>%
      mutate(coastal = str_detect(helcom_id, "^SEA")) %>%
      select(region_id, coastal, BQR, area_km2) %>%
      mutate(indicator = "spp_seals")
  )


  ## will use geometric mean to represent how components are not interchangeable
  ## https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
  gm_mean <- function(x, na.rm = TRUE, zero.propagate = FALSE){
    if(any(x < 0, na.rm = TRUE)){
      return(NaN)
    }
    if(zero.propagate){
      if(any(x == 0, na.rm = TRUE)){
        return(0)
      }
      exp(mean(log(x), na.rm = na.rm))
    } else {
      exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
    }
  }


  ## Calculate Status ----

  ## at BQR of 0.6 is the reference point except for birds, which have a ref point of 0.75
  ## will rescale so BQR = 0.6 or above is a status of 1, while BQR = 0 is a status of zero
  bd_status <- bd_all_data %>%
    group_by(region_id, coastal, indicator) %>%
    summarize(averageBQR = weighted.mean(BQR, area_km2, na.rm = TRUE)) %>%
    ungroup() %>%
    ## combine coastal and open sea with equal weights rather than area-weighted average
    group_by(region_id, indicator) %>%
    summarize(averageBQR = mean(averageBQR, na.rm = TRUE)) %>%
    ungroup() %>%
    ## bind birds data with other biodiversity layers
    bind_rows(birds) %>%
    mutate(averageBQR = ifelse(is.nan(averageBQR), NA, averageBQR)) %>%
    ## calculate the status for individual indicators
    mutate(indicator_status = case_when(
      indicator == "spp_birds" ~ pmin(1, (4/3)*averageBQR),
      indicator != "spp_birds" ~ pmin(1, (5/3)*averageBQR)
    )) %>%
    mutate(indicator_status= 100*indicator_status) %>%
    select(-averageBQR)

  ## save individual indicators as intermediate results
  for(ind in unique(bd_status$indicator)){
    savefp <- file.path(
      dir_assess, "intermediate",
      sprintf("%s.csv", ind)
    )
    if(!file.exists(savefp)){
      write_csv(
        bd_status %>%
          filter(indicator == ind) %>%
          select(region_id, indicator_status),
        savefp
      )
    }
  }

  ## calculate overall biodiversity status scores
  bd_status <- bd_status %>%
    group_by(region_id) %>%
    summarize(status = gm_mean(indicator_status)) %>%
    ungroup() %>%
    rename(score = status)


  ## Trend ----

  bd_trend <- AlignDataYears(layer_nm="bd_spp_trend", layers_obj=layers) %>%
    filter(scenario_year == scen_year) %>%
    select(region_id, score) %>%
    mutate(score = round(score, 3))


  ## Return biodiversity status and trend scores ----

  bd_status_and_trend <- dplyr::bind_rows(
    mutate(bd_status, dimension = "status"),
    mutate(bd_trend, dimension = "trend")
  )

  scores <- bd_status_and_trend %>%
    dplyr::mutate(goal = "BD") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)

} ## End BD Function
