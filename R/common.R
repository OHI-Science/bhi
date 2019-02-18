## For common libraries, directories, functions


## Libraries
library(tidyverse)
library(here)
library(tools)
library(purrr)
library(broom)

library(ggmap) # install.packages("ggmap")
library(here)

library(odbc) # devtools::install_github("rstats-db/odbc")
library(DBI) # install.packages("DBI")
library(RMySQL)


## Directories
dir_baltic <- here::here()
dir_layers <- file.path(dir_baltic, "layers")
dir_R <- file.path(dir_baltic, "R")
dir_spatial <- file.path(dir_baltic, "prep/spatial")


#' copy layers from 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder
#'
#' @param assessment_path
#' @param repo_location
#'
#' @return
#' @export
#'
#' @examples
copy_layers_for_assessment <- function(assessment_path, repo_location){

  repo_loc <- repo_location

  req <- GET("https://api.github.com/repos/OHI-Science/bhi-prep/git/trees/master?recursive=1")
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F) %>%
    stringr::str_extract_all("prep/layers/.*.csv", simplify = TRUE) %>%
    as.data.frame()

  for(i in 1:length(filelist$V1)){
    tmp <- filelist[i,1] %>% as.character()
    if(str_length(tmp) != 0){
      url <- sprintf("%s/%s", repo_loc, tmp)
      name_of_layer <- tmp %>% str_extract("[a-z0-9_]+.csv$")
      GET(url, write_disk(sprintf("%s/layers/%s", assessment_path, name_of_layer), overwrite = TRUE))
    }
  }
}



#' calculate BHI scores for each scenario year and save to a single csv file
#'
#' @param assessment_path the bhi repository 'baltic' subfolder, wherever that is on your local computer
#' @param scenario_yrs which years scores are to be calculated for
#' @param scores_path the directory where to save the scores.csv, by default in the 'baltic' (assessment) folder
#'
#' @return
#' @export
#'
#' @examples
calculate_scores <- function(assessment_path, scenario_yrs, scores_path = "."){

  currentwd <- getwd()
  setwd(assessment_path)

  ## load scenario configuration
  conf <- ohicore::Conf("conf")

  ## check that scenario layers files in the \layers folder match layers.csv registration
  ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)

  ## load scenario layers for ohicore to access
  layers <- ohicore::Layers("layers.csv", "layers")

  scorelist <- lapply(scenario_yrs, function(yr){

    print(sprintf("For assessment year %s", yr))
    layers$data$scenario_year <- yr
    scores_scenario_year <- ohicore::CalculateAll(conf, layers) %>%
      dplyr::mutate(year = yr)}) %>% dplyr::bind_rows()

  ## write csv by default to 'assessment_path'
  readr::write_csv(scorelist,
                   sprintf("%s/scores.csv", scores_path),
                   na = "")

  setwd(currentwd)
}



#' update `alt_layers_full_table.csv` to include additional versions of layers created
#'
#' @param assessment_path
#' @param prep_path
#' @param assess_year
#'
#' @return
#' @export
#'
#' @examples
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




#' update scenario years and/or layers' names in scenario_data_years table
#'
#' @param scen_data_years
#' @param scen_yrs
#' @param new_lyrs
#' @param rename_lyrs a list with two elements: 'layer_name' vector of layers to be renamed and 'to' vector of new names with placement (index) matching current layer name
#'
#' @return
#' @export
#'
#' @examples
scenario_data_include <- function(scen_data_years, scen_yrs, new_lyrs = "", rename_lyrs = ""){

  cols <- names(scen_data_years)
  new_scen_yrs <- setdiff(scen_yrs, scen_data_years$scenario_year %>% unique())

  ## the new scenario years rows for existing layers
  add_scen_rows <- scen_data_years %>%
    select(layer_name) %>%
    unique() %>%
    dplyr::mutate(scenario_year = list(new_scen_yrs), data_year = NA) %>%
    tidyr::unnest()

  ## rows for new layers with the given range of scenario years
  add_lyr_rows <- tibble::tibble(layer_name = new_lyrs,
                             scenario_year = list(scen_yrs),
                             data_year = NA) %>%
    tidyr::unnest() %>%
    filter(layer_name != "")

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
      select(-to) %>%
      dplyr::arrange(layer_name, scenario_year, data_year)
  }

  ## return the updated table
  ## when using this function, overwrite file with new table
  return(scenario_data_years_updated)

}

scenario_data_align <- function(scen_data_file, align_basis_file){

}



layers_csv_edit <- function(tab_to_update, update_using_tab, prefix){

  replacements <- update_using_tab %>%
    filter(grepl(sprintf("^%s_.*", prefix), layer)) %>%
    select(layer, filename)

  updated_tab <- update_using_tab %>%
    filter(grepl(sprintf("^%s_.*", prefix), layer)) %>%
    rbind(tab_to_update %>%
            filter(!grepl(sprintf("^%s_.*", prefix), layer)) %>%
            mutate(clip_n_ship_disag = NA,
                   clip_n_ship_disag_description = NA,
                   rgns_in = NA))

  return(list(updated_tab, replacements))
}





