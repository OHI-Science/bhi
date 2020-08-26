
BD <- function(layers){

  ## From code in 'functions.R BD' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## BD goal currently has no subgoals; it is only species (SPP)
  ## status is the geometric mean of each taxa group status by basin
  ## trend is temporarily substituted by OHI-global (2019) BD trend scores


  scen_year <- layers$data$scenario_year


  spp_subbasin_assessments <- AlignDataYears(layer_nm="bd_spp_assessments", layers_obj=layers) %>%
    ## scenario year here is the assessment_year_red_list:
    ## because of infrequent iucn assessments, there will inevitably be a lag in biodiversity status...
    rename(year = scenario_year)

  spp_norec <- AlignDataYears(layer_nm="bd_spp_norec", layers_obj=layers) %>%
    rename(year = scenario_year)

  ## Create vulnerability lookup table ----
  ## weights from Halpern et al 2012, SI
  vuln_lookup <- mutate(
      data.frame(
        red_list_category = c(
          "EX - Extinct",
          "RE - Regionally Extinct",
          "CR - Critically Endangered",
          "EN - Endangered",
          "VU - Vulnerable",
          "NT - Near Threatened",
          "LC - Least Concern"
        ),
        stringsAsFactors = FALSE
      ),
      wgt = case_when(
        red_list_category == "EX - Extinct" ~ 1,
        red_list_category == "RE - Regionally Extinct" ~ 1,
        red_list_category == "CR - Critically Endangered" ~ 0.8,
        red_list_category == "EN - Endangered" ~ 0.6,
        red_list_category == "VU - Vulnerable" ~ 0.4,
        red_list_category == "NT - Near Threatened" ~ 0.2,
        red_list_category == "LC - Least Concern" ~ 0
      )
    )
  ## join vulnerability score to subbasin assessments dataframe
  spp_assess_w_num <- left_join(spp_subbasin_assessments, vuln_lookup, by = "red_list_category")



  ## Calculate Status ----

  spp_status <- spp_assess_w_num %>%
    ## 1:
    ## for each species, take only values from most recent redlist assessment
    ## s.t. the year is less than or equal to the scenario year being evaluated...
    group_by(scientific_name) %>%
    filter(assessment_year_red_list <= scen_year) %>%
    filter(assessment_year_red_list == max(assessment_year_red_list)) %>%
    ungroup() %>%
    ## 2:
    ## join info for penalty/scaling based on proportion spp 'unobserved in this decade'...
    left_join(spp_norec, by = c("region_id", "scientific_name")) %>%
    group_by(region_id, species_group, wgt, no_rec_this_dec) %>%
    summarize(nspp = n()) %>%
    ungroup() %>%
    mutate(no_observ = ifelse(no_rec_this_dec & wgt > 0 | wgt == 1, nspp, 0)) %>%
    ## 3:
    ## sum threat weights for each region and taxa (species_group),
    ## normalizing by number distinct spp
    group_by(region_id, species_group) %>%
    rename(weights = red_list_category_numeric) %>%
    summarise(
      wi_spp = sum(weights*nspp)/sum(nspp),
      nspp = sum(nspp),
      pct_not_obs = 1 - (sum(nspp) - sum(no_observ))/sum(nspp)
    ) %>%
    ungroup() %>%
    mutate(status_taxa_initial = (1 - wi_spp)) %>%
    ## 4:
    ## Rescale/penalize using 'spp not observed this decade'
    ## with 75 percent representing score of zero
    mutate(status_taxa_penalized = status_taxa_initial*(0.75-pct_not_obs)/0.75) %>%
    ## 5:
    ## summarize status values across taxa by region, using geometric mean
    ## normalizing by number disinct species in each taxa
    group_by(region_id) %>%
    summarize(status = 100*round(exp(weighted.mean(log(status_taxa_penalized), nspp), 2)))

  ## note: penalty has negligible effect on statuses calculated as geometric mean of taxa statuses


  ## Trend ----

  spp_trend <- AlignDataYears(layer_nm="bd_spp_trend", layers_obj=layers) %>%
    dplyr::rename(year = ohiglobal_assess_year)


  ## Return biodiversity status and trend scores ----

  spp_status_and_trend <- dplyr::bind_rows(
    mutate(spp_status, dimension = "status"),
    mutate(spp_trend, dimension = "trend")
  )

  scores <- spp_status_and_trend %>%
    dplyr::mutate(goal = "BD") %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)

} ## End BD Function
