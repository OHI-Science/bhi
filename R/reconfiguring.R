
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


#' aligning scenario and data years for a given layer
#'
#' @param scen_data_years
#' @param layer_name
#' @param data_yrs
#' @param scen_yrs
#'
#' @return
#' @export
#'
#' @examples
scenario_data_align <- function(scen_data_years, lyr_name, data_yrs, scen_yrs){

  keep_rows <- scen_data_years %>%
    dplyr::filter(layer_name != lyr_name)

  layer_years_align <- tibble::tibble(layer_name = lyr_name,
                                      scenario_year = list(scen_yrs)) %>%
    tidyr::unnest() %>%
    dplyr::rowwise() %>%
    mutate(data_year = scenario_year %>%
             lapply(function(scenario_year){
               d = ifelse(length(data_yrs) == 1,
                          data_yrs,
                          data_yrs[data_yrs < scenario_year])
               ind = which.min(abs(d - scenario_year))
               d[ind]
             }) %>% unlist())

  if(length(scen_yrs) != length(data_yrs)){
    print("unequal numbers of scenario years and data years")
  }

  years_align <- layer_years_align %>%
    rbind(keep_rows) %>%
    dplyr::arrange(layer_name, scenario_year, data_year)

  return(years_align)
}


#' update rows in layers.csv for a given layer
#'
#' I am mostly using this function just to set up bhi multiyear assessments repo from the archived repo...
#'
#' @param tab_to_update
#' @param update_using_tab
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
layers_csv_edit <- function(tab_to_update, update_using_tab, prefix_or_layer){

  replacements <- update_using_tab %>%
    filter(grepl(sprintf("^%s.*", prefix_or_layer), layer)) %>%
    select(layer, filename)

  updated_tab <- update_using_tab %>%
    dplyr::filter(grepl(sprintf("^%s.*", prefix_or_layer), layer)) %>%
    rbind(tab_to_update %>%
            filter(!grepl(sprintf("^%s.*", prefix_or_layer), layer)) %>%
            mutate(clip_n_ship_disag = NA,
                   clip_n_ship_disag_description = NA,
                   rgns_in = NA))

  return(list(updated_tab, replacements))
}


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
