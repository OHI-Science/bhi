
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
  spp_assess_w_num <- left_join(spp_subbasin_assessments, vuln_lookup, by = "red_list_category") %>%
    filter(!is.na(red_list_category_numeric))


  ## Calculate Status ----

  spp_status <- spp_assess_w_num %>%
    ## 1:
    ## for each species, take only values from most recent redlist assessment
    ## s.t. the year is less than or equal to the scenario year being evaluated...
    filter(assessment_year_red_list <= scen_year) %>%
    group_by(scientific_name) %>%
    filter(assessment_year_red_list == max(assessment_year_red_list)) %>%
    ungroup() %>%

    ## 2:
    ## sum threat weights for each region and taxa (species_group),
    ## normalizing by number distinct spp
    group_by(region_id, species_group, red_list_category_numeric) %>%
    summarize(nspp = n()) %>%
    ungroup() %>%
    rename(weights = red_list_category_numeric) %>%
    group_by(region_id, species_group) %>%
    summarise(
      wi_spp = sum(weights*nspp)/sum(nspp),
      nspp = sum(nspp)
    ) %>%
    ungroup() %>%
    mutate(status_taxa_initial = (1 - wi_spp))


  ## save individual indicators as intermediate results
  for(grp in unique(spp_status$species_group)){
    savefp <- file.path(
      dir_assess, "intermediate",
      sprintf("%s.csv", stringr::str_to_lower(stringr::str_replace_all(grp, " ", " ")))
    )
    if(!file.exists(savefp)){
      write_csv(
        spp_status %>%
          filter(species_group == grp) %>%
          select(region_id, species_group_score = status_taxa_initial, num_species = nspp),
        savefp
      )
    }
  }

  ## 3:
  ## summarize status values across taxa by region, using geometric mean
  ## normalizing by number disinct species in each taxa
  spp_status <- spp_status %>%
    group_by(region_id) %>%
    summarize(status = 100*round(exp(weighted.mean(log(status_taxa_initial), nspp, na.rm = TRUE)), 2))


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
