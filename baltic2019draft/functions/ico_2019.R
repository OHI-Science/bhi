
ICO <- function(layers){

  ## From code in 'functions.R ICO' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  scen_year <- layers$data$scenario_year

  ico_subbasin_assessments <- AlignDataYears(layer_nm="sp_ico_assessments", layers_obj=layers) %>%
    ## scenario year here is the assessment_year_red_list:
    ## because of infrequent iucn assessments, there will inevitably be a lag in iconic species status...
    rename(year = scenario_year)


  ## Create vulnerability lookup table ----
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
    red_list_category_numeric = case_when(
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
  ## filter where red_list_category_numeric is NA, as these correspond to unused categories:
  ## NA - Not Applicable, NE - Not Evaluated, DD - Data Deficient
  ico_assess_w_num <- left_join(ico_subbasin_assessments, vuln_lookup, by = "red_list_category") %>%
    filter(!is.na(red_list_category_numeric))



  ## Calculate Status ----

  ico_status <- ico_assess_w_num %>%
    ## 1:
    ## for each species, take only values from most recent redlist assessment
    ## s.t. the year is less than or equal to the scenario year being evaluated...
    filter(year <= scen_year) %>%
    group_by(scientific_name) %>%
    filter(year == max(year)) %>%
    ungroup() %>%

    ## 2:
    ## sum threat weights for each region and taxa (species_group),
    ## normalizing by number distinct spp
    group_by(region_id, species_group, red_list_category_numeric) %>%
    summarize(nspp = n()) %>%
    ungroup() %>%
    group_by(region_id, species_group) %>%
    rename(weights = red_list_category_numeric) %>%
    summarise(
      wi_spp = sum(weights*nspp)/sum(nspp),
      nspp = sum(nspp)
    ) %>%
    ungroup() %>%
    mutate(status_taxa_initial = (1 - wi_spp)) %>%

    ## 3:
    ## summarize status values across taxa by region, using geometric mean
    ## normalized by numbers of species in each species group
    group_by(region_id) %>%
    summarize(status = 100*round(exp(weighted.mean(log(status_taxa_initial), nspp, na.rm = TRUE)), 2))



  ## Trend ----

  ## for now have NA iconic species trends...
  ico_trend <- data.frame(
    region_id = seq(1, 42, 1),
    score = rep(NA, 42)
  )

  ## Return iconic species status and trend scores ----

  ico_status_and_trend <- dplyr::bind_rows(
    ico_status %>%
      rename(score = status) %>%
      mutate(dimension = "status", goal = "ICO"),
    ico_trend %>%
      mutate(dimension = "trend", goal = "ICO")
  )
  scores <- select(ico_status_and_trend, region_id, goal, dimension, score)

  return(scores)

} ## End ICO function
