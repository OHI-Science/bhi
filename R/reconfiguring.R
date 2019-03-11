## Libraries
library(tidyverse)
library(here)

## Functions

#' update scenario years and/or layers' names in scenario_data_years table
#'
#' @param scen_data_years scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv
#' @param scen_yrs the scenario years to be included in the updated scenario_data_years.csv
#' @param new_lyrs names (character vector) of any layers not yet in table, for which to create scenario-data year information
#' @param rename_lyrs a list with two elements: 'layer_name' vector of layers to be renamed and 'to' vector of new names with placement (index) matching current layer name
#'
#' @return

scenario_data_include <- function(scen_data_years, scen_yrs, new_lyrs = "", rename_lyrs = ""){

  cols <- names(scen_data_years)
  new_scen_yrs <- setdiff(scen_yrs, scen_data_years$scenario_year %>% unique())

  ## the new scenario years rows for existing layers
  add_scen_rows <- scen_data_years %>%
    dplyr::select(layer_name) %>%
    unique() %>%
    dplyr::mutate(scenario_year = list(new_scen_yrs), data_year = NA) %>%
    tidyr::unnest()

  ## rows for new layers with the given range of scenario years
  add_lyr_rows <- tibble::tibble(layer_name = new_lyrs,
                                 scenario_year = list(scen_yrs),
                                 data_year = NA) %>%
    tidyr::unnest() %>%
    dplyr::filter(layer_name != "")

  ## bind new rows to old table and rename layers if specified
  scenario_data_years_updated <- scen_data_years %>%
    rbind(add_scen_rows) %>%
    rbind(add_lyr_rows) %>%
    dplyr::arrange(layer_name, scenario_year, data_year)


  if(rename_lyrs != ""){

    rename_df <- as.data.frame(rename_lyrs, stringsAsFactors = FALSE)

    scenario_data_years_updated <- scenario_data_years_updated %>%
      dplyr::left_join(rename_df, by = "layer_name") %>%
      dplyr::mutate(layer_name = ifelse(is.na(to), layer_name, to)) %>%
      dplyr::select(-to) %>%
      dplyr::arrange(layer_name, scenario_year, data_year)
  }

  ## return the updated table
  ## when using this function, overwrite file with new table
  return(scenario_data_years_updated)

}


#' aligning scenario and data years for a given layer
#'
#' maps years within the layer dataset to a "scenario year" for a given layer
#' because of time lags in or aperiodic data collection...
#'
#' @param scen_data_years scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv
#' @param layer_name name of the layer for which to align scenario and data years
#' @param data_yrs the years of data for the specified layer, i.e. all the years in the layer data file
#' @param scen_yrs the scenario years to be included in the updated scenario_data_years.csv
#'
#' @return

scenario_data_align <- function(scen_data_years, lyr_name, data_yrs, scen_yrs, approach = ""){

  ## rows of scenario_data_years we are not updating
  keep_rows <- scen_data_years %>%
    dplyr::filter(layer_name != lyr_name)

  if(length(scen_yrs) != length(data_yrs)){
    print("unequal numbers of scenario years and data years")
  }
  all_data_yrs <- data_yrs

  ## data_yrs we may actually want to match with scen_yrs
  data_yrs <- data_yrs[data_yrs <= max(scen_yrs)] %>%
    sort(decreasing = TRUE) %>% unique()
  data_yrs <- data_yrs[1:length(scen_yrs)]

  ## matrix M with potential edge-weights
  M <- array(sort(rep(scen_yrs, length(data_yrs)),
                  decreasing = TRUE) - rep(data_yrs, length(scen_yrs)),
             c(length(data_yrs), length(scen_yrs)),
             list(data_yrs, sort(scen_yrs, decreasing = TRUE)))

  match_years <- data.frame(scenario_year = sort(scen_yrs, decreasing = TRUE),
                            data_year = NA)

  if(str_detect(approach, "intervals|steps|timestep")){
    ## don't actually want to assign if si to dj if |si-dj| > reasonable_diff
    reasonable_diff <- ifelse(str_detect(approach, "max step|maximum step|with step|max diff|maximum|max.|difference"),
                              as.numeric(gsub("\\D", "", approach), 5))
    for (s in 1:length(scen_yrs)){
      match_years[s, "data_year"] <- ifelse(abs(M[s,s]) < reasonable_diff,
                                            dimnames(M)[[1]][s],
                                            which.min(M[,s][M[,s] >= 0]) %>%
                                              names()) %>% as.integer()
    }
    print(sprintf(
      "matching scenario years with data so when reasonable (<%syr difference), there are more discrete timesteps",
      reasonable_diff))

  } else {
    ## default is to assign closest years
    print("matching each scenario year with its next closest data year")

    ## find min sum of edge-weights where F: S->D is onto and deg(si)=1 for all si in scen_yrs
    for (s in 1:length(scen_yrs)){
      match_years[s, "data_year"] <- which.min(M[,s][M[,s] >= 0]) %>%
        names() %>% as.integer()
    }
  }

  layer_years_align <- tibble::tibble(
    layer_name = lyr_name,
    scenario_year = list(scen_yrs)) %>%
    tidyr::unnest() %>%
    dplyr::left_join(match_years, by = "scenario_year")
  data_yrs_unused <- setdiff(all_data_yrs, unique(layer_years_align$data_year))

  years_align <- layer_years_align %>%
    rbind(keep_rows) %>%
    dplyr::arrange(layer_name, scenario_year, data_year)

  return(years_align)
}


#' update `alt_layers_full_table.csv` to include additional versions of layers created
#'
#' @param assessment_path file path to assessment folder within bhi repo
#' @param prep_path file path to the prep folder within bhi-prep repo
#' @param assess_year assessment year i.e. the year for/during which the assessment is being conducted
#'
#' @return

update_alt_layers_tab <- function(assessment_path, prep_path, assess_year){

  # alt_layers <- readr::read_delim(sprintf("%s/testing/alt_layers_full_table.csv", assessment_path), delim = ";")

  alt_layers <- readr::read_csv(sprintf("%s/testing/%s", assessment_path, "alt_layers_full_table.csv"))
  layer_names <- alt_layers$layer %>% unique() %>% as.list()

  ## identify new layers to add to the 'alt_layers_full_table.csv' table
  add_alt_layers <- list.files(prep_path, recursive = TRUE) %>%
    grep(pattern = sprintf(".*v%s/output/[a-z0-9_]+.csv$", assess_year), value = TRUE) %>%
    enframe(name = NULL) %>%
    dplyr::mutate(filename = basename(as.character(value)),
                  goal = stringr::str_extract(value, pattern = "[a-zA-Z]+"),
                  use = NA,
                  layer_names = list(layer_names),
                  subfolder = stringr::str_extract(value, pattern = "(\\w+ ?){1}/v2") %>%
                    stringr::str_remove("/v2")) %>%

    dplyr::rowwise() %>%
    mutate(layer = stringr::str_match(filename, as.character(layer_names)) %>%
             lapply(function(x){x[!is.na(x)]}) %>%
             unlist(),
           subfolder = ifelse(subfolder == goal, NA, subfolder)) %>%
    ungroup() %>%

    dplyr::select(layer, filename, use, goal, subfolder) %>%
    dplyr::filter(!filename %in% alt_layers$filename)

  ## rowbind new layer versions to the 'alt_layers_full_table.csv' and save updated table
  alt_layers_full_table <- alt_layers %>% rbind(add_alt_layers)
  readr::write_csv(alt_layers_full_table,
                   sprintf("%s/testing/%s", assessment_path, "alt_layers_full_table.csv"))
}


#' update rows in layers.csv for a given layer
#'
#' registers a specified file is to the given layer, after checking registration against layers object created by ohicore::Layers
#' written originally mostly for setting up bhi multiyear assessments repo from the archived repo...
#'
#' @param layers_object a layers object created with ohicore::Layers for the bhi repo assessment folder we aim to reconfigure
#' @param lyr_file file path to where layer data file is located, including the name of the csv file itself to be incorporated into bhi layers object
#' @param lyr_name single character string with name of the layer (not the filename) to be associated with the
#' @param assessment_path file path to assessment folder within bhi repo
#' @param update_with a dataframe or tibble with at least one row, with the information to be added to layers.csv for the layer
#' @param write if TRUE then the function will automatically overwrite layers.csv and write lyr_file to the 'layers' folder
#'
#' @return

layers_edit <- function(layers_object, lyr_file, lyr_name, assessment_path, update_with = NULL, write = FALSE){

  lyr_file_data <- readr::read_csv(lyr_file)
  lyr_file_name <- basename(lyr_file)

  ## 0. using the layers object, check if layer is already registerd with the specified layer file
  chk1 <- lyr_file_name %in% layers_object$meta$filename
  chk2 <- lyr_name %in% layers_object$meta$layer
  if(chk1 & chk2){
    chk3 <- which(layers_object$meta$filename==lyr_file_name)==which(layers_object$meta$layer==lyr_name)
  }

  if(chk1 & chk2 & chk3){
    print("layer already registered/configured with given filename")
  } else {

    ## 1. register layer in layers.csv

    layers_csv <- readr::read_csv(file.path(assessment_path, "layers.csv")) # matches w layers_object 'meta' slot contents
    layers_metadata_csv <- readr::read_csv(file.path(assessment_path, "layers_metadata.csv")) %>%
      dplyr::select(-data_source, -gapfill_file)

    ## create or extract 'update_with' information for layer, i.e. row entry for layer.csv
    if(is.null(update_with)){
      ## need to generate information to fill layers.csv row
      update_with <- data.frame(array(NA, dim = c(1, ncol(layers_csv))))
      colnames(update_with) <- names(layers_csv)
      update_with[1, "layer"] <- lyr_name
      update_with[1, "filename"] <- lyr_file_name

      ## add other layer information if it exists in the layers_metadata.csv
      if(lyr_name %in% layers_metadata_csv$layer){
        update_with <- update_with %>%
          dplyr::select(-name, -description, -units, -targets) %>%
          dplyr::left_join(layers_metadata_csv, by = "layer") %>%
          dplyr::select(names(layers_csv)) # original column ordering
      } else {
        print("check layers.csv and layers_metadata.csv: some critical information must be entered manually")
      }

    } else {
      update_with <- update_with %>%
        dplyr::filter(grepl(sprintf("^%s.*", lyr_name), layer))
    }
    updated_tab <- update_with %>%
      rbind(layers_csv %>%
              dplyr::filter(!grepl(sprintf("^%s.*", lyr_name), layer)) %>%
              dplyr::mutate(clip_n_ship_disag = NA,
                     clip_n_ship_disag_description = NA,
                     rgns_in = NA))

    ## 2. write updated layers.csv and write layer file to the layers folder, overwriting if it is already there
    if(write == TRUE){
      readr::write_csv(updated_tab, file.path(assessment_path, "layers.csv"))
      readr::write_csv(lyr_file_data, file.path(assessment_path, "layers", lyr_file_name))
    }

    ## final messages and return results
    print(sprintf("file '%s' is registered to be used for the '%s' layer", lyr_file_name, lyr_name))
    print("IMPORTANT: now you must update scenario_data_years.csv!")

    return(list(updated_tab, lyr_file_data))
  }
}
