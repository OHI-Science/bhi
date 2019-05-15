
LSP <- function(layers){

  ## From code in 'functions.R LSP' of v2015 BHI assessment, see bhi-1.0-archive github repo
  ## Revised to use multi-year framework, incorporating scenario_data_years
  ## Uses ohicore::AlignDataYears() rather than ohicore::SelectLayersData()

  ## scores calculated in prep file (bhi-prep repository, data and prep LSP subfolders)

  scen_year <- layers$data$scenario_year

  lsp_status <- AlignDataYears(layer_nm="lsp_status", layers_obj=layers) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  lsp_trend <- AlignDataYears(layer_nm="lsp_trend", layers_obj=layers) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score, dimension)

  scores <- rbind(lsp_status, lsp_trend) %>%
    mutate(goal = "LSP") %>%
    select(region_id, goal, dimension, score)

  return(scores)

} ## End LSP function
