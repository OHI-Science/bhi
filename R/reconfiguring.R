## Libraries
source(file.path(here::here(), "R", "setup.R"))
library(tools)
library(tibble)


## Functions

#' update rows in layers.csv for a given layer
#'
#' registers a specified file to the given layer,
#' after checking registration against layers object created by ohicore::Layers
#' written originally/mostly for setting up bhi multiyear assessments repo from the archived repo...
#'
#' @param layers_obj a layers object created with ohicore::Layers,
#' for the repo assessment folder to reconfigure, not the archived version
#' @param lyr_file file path to where layer data file is located,
#' including the name of the csv file itself to be incorporated into bhi layers object
#' @param lyr_name single character string with name of the layer (not the filename) to be associated with the layer data file
#' @param dir_assess file path to assessment folder within bhi repo
#' @param lyr_meta file path to lyr_metadata.csv that contains info corresponding to lyr_file;
#' should have layer, name, description, units, and targets fields
#' @param update_w a dataframe or tibble with at least one row,
#' with the information to be added to layers.csv for the layer
#' @param write if TRUE then the function will automatically overwrite layers.csv and write lyr_file to the 'layers' folder
#'
#' @return updated layers.csv table is first output, the second output is the contents of the layer file specified by lyr_file arg

layerscsv_edit <- function(layers_obj, lyr_file, lyr_name, dir_assess, lyr_meta, update_w = NULL, write = FALSE){

  lyr_file_data <- readr::read_csv(lyr_file, col_types = cols())
  lyr_file_name <- basename(lyr_file)

  ## using the layers object, check if layer is already registerd with the specified layer file
  chk1 <- lyr_file_name %in% layers_obj$meta$filename
  chk2 <- lyr_name %in% layers_obj$meta$layer
  if(chk1 & chk2){
    chk3a <- which(layers_obj$meta$filename == lyr_file_name)
    chk3b <- which(layers_obj$meta$layer == lyr_name)
    chk3 <- chk3a == chk3b
  } else { chk3 <- FALSE }

  if(chk1 & chk2 & chk3){
    message("layer already registered/configured with given filename")
  } else {

    ## register layer in layers.csv

    layers_csv <- readr::read_csv(
      file.path(dir_assess, "layers.csv"),
      col_types = cols()) # matches w layers_obj 'meta' slot contents
    lyr_metadata_csv <- readr::read_csv(
      file.path(lyr_meta),
      col_types = cols(.default = "c")) %>%
      dplyr::select(-data_source, -gapfill_file)

    ## create or extract 'update with' information for layer, i.e. row entry for layer.csv
    if(is.null(update_w)){

      ## need to generate information to fill layers.csv row
      update_w <- data.frame(array(NA, dim = c(1, ncol(layers_csv))))
      colnames(update_w) <- names(layers_csv)
      update_w[1, "layer"] <- lyr_name
      update_w[1, "filename"] <- lyr_file_name

      ## add other layer information if it exists in the lyr_metadata.csv
      if(lyr_name %in% lyr_metadata_csv$layer){
        update_w <- update_w %>%
          dplyr::select(-name, -description, -units, -targets) %>%
          dplyr::left_join(lyr_metadata_csv, by = "layer") %>%
          dplyr::select(names(layers_csv)) # original column ordering
      } else {
        message("check layers.csv and lyr_metadata.csv: some critical information must be entered manually")
      }
    } else {
      update_w <- update_w %>%
        dplyr::filter(grepl(sprintf("^%s.*", lyr_name), layer))
    }
    updated_tab <- update_w %>%
      rbind(layers_csv %>%
              dplyr::filter(!grepl(sprintf("^%s.*", lyr_name), layer)) %>%
              dplyr::mutate(clip_n_ship_disag = NA,
                     clip_n_ship_disag_description = NA,
                     rgns_in = NA))

    ## write updated layers.csv and write layer file to the layers folder, overwriting if it is already there
    if(write == TRUE){
      readr::write_csv(updated_tab, file.path(dir_assess, "layers.csv"))
      readr::write_csv(lyr_file_data, file.path(dir_assess, "layers", lyr_file_name))
    }

    ## final messages and return results
    message(sprintf("file '%s' has been registered to be used for the '%s' layer", lyr_file_name, lyr_name))
    print("note: update scenario_data_years.csv to match new layer data!")

    return(list(updated_tab, lyr_file_data))
  }
}


#' configure functions.R for testing
#'
#' compiles files listed in a local 'test_functions.csv' into the single \code{functions.R} that ohicore needs
#'
#' @param dir_assess filepath specifying the assessment main folder containing 'conf' and 'testing' folders
#' @param test_funs_list a list of filepaths from (not including) 'testing/alt_functions' to the functions we want to use in analysis
#' @return revises functions.R at existing location in 'conf' folder; no direct output

configure_functions <- function(dir_assess, test_funs_list){

  ## get filepaths list of functions we want to collect into functions.R
  fun_scripts_lst <- list(file.path(dir_assess, "functions", test_funs_list))

  ## confirm all specified function versions actually exist before overwriting functions.R
  chk <- file.exists(fun_scripts_lst[[1]])
  if(any(!chk)){
    ## file.exists is FALSE i.e., functions aren't at given location
    m <- basename(fun_scripts_lst[[1]][!chk])
    stop(sprintf(
      "the following do not exist in given locations: %s",
      paste(m, collapse = ", ")
    ))
  }

  functionsR_path <- file.path(dir_assess, "conf", "functions.R")
  sink(file = functionsR_path, append = FALSE) # overwrite functions.R with first function
  cat(
    scan(
      file = fun_scripts_lst[[1]][1],
      what = "character",
      sep = "\n",
      blank.lines.skip = FALSE
    ),
    sep = "\n"
  )

  sink(file = functionsR_path, append = TRUE) # append subsequent functions
  for(i in fun_scripts_lst[[1]][-1]){
    cat(
      scan(
        file = i,
        what = "character",
        sep = "\n",
        blank.lines.skip = FALSE
      ),
      sep = "\n"
    )
  }
  closeAllConnections() # stop the sinking

  ## preindex_function and postindex_function columns of goals.csv have to match functions.R
  txt <- scan(file = functionsR_path, what = "character", sep = "\n")
  breaks_str <- "^[A-Z]{2,3}\\s<-\\sfunction\\(|^[A-Z]{2,3}\\s=\\sfunction\\("

  goal <- stringr::str_extract(grep(breaks_str, txt, value = TRUE), "^[A-Z]{2,3}") %>%
    as.data.frame() %>%
    mutate(goal = as.character(.)) %>%
    select(goal)
  index_fun <- txt[grep(breaks_str, txt)] %>%
    str_extract(pattern = "\\([^()]+\\)") %>%
    as.data.frame() %>%
    mutate(index_fun = as.character(.)) %>%
    select(index_fun)
  index_function_tab <- cbind(goal, index_fun)

  int_goals_csv <- read_csv(
    file.path(dir_assess, "conf", "goals.csv"),
    col_types = cols(.default = "c")) %>%
    full_join(index_function_tab, by = "goal") %>%
    mutate(
      preindex_function = ifelse(
        !str_detect(index_fun, "scores")|(is.na(preindex_function) & is.na(postindex_function)),
        paste0(goal, index_fun),
        NA),
      postindex_function = ifelse(
        is.na(preindex_function) & !is.na(index_fun),
        paste0(goal, index_fun),
        NA)) %>%
    filter(!(is.na(preindex_function) & is.na(postindex_function))) %>%
    select(-index_fun)

  write_csv(int_goals_csv, file.path(dir_assess, "conf", "goals.csv"))
}


# configure_pressures <- function(){}
#
#
# configure_resilience <- function(){}


#' copy prepared layers from bhi-prep to bhi repo
#'
#' copies layers from prep 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder, from github repo url
#'
#' @param dir_assess file path to assessment folder within bhi repo
#' @param copy_layers
#' @param repo_loc url pointing to the bhi repo on github
#'
#' @return

copy_lyrs_from_bhiprep <- function(dir_assess, copy_layers = list("all"), repo_loc = NULL){

  if(is.null(repo_loc)){
    repo_loc <- paste(bhiprep_gh_raw, "master", sep = "/") # raw bhi-prep github repo url, bhiprep_gh_raw from setup.R
  }
  if(unlist(copy_layers) == "all"){
    print("copying over all layers from bhi-prep repository 'layers' folder")
    print("note: will overwrite layers in bhi repo 'layers' folder with bhi-prep versions from github, of same name")
    filelist <- bhiprep_github_layers(github_api_url = bhiprep_gh_api)
    ## bhiprep_api defined in setup.R, function in common.R
  } else {
    filelist <- data.frame(V1 = unlist(copy_layers))
  }
  for(i in 1:length(filelist$V1)){
    tmp <- filelist[i,1] %>% as.character()

    if(stringr::str_length(tmp) != 0){
      url <- sprintf("%s/%s", repo_loc, tmp)
      name_of_layer <- stringr::str_extract(tmp, "[a-z0-9_]+.csv$")

      httr::GET(
        url,
        write_disk(
          sprintf("%s/layers/%s", dir_assess, name_of_layer),
          overwrite = TRUE
        )
      )
    }
  }
}


#' update scenario years and/or layers' names in scenario_data_years table
#'
#' @param scen_data_years scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv
#' @param scenario_yrs the scenario years to be included in the updated scenario_data_years.csv
#' @param new_lyrs names (character vector) of any layers not yet in table, for which to create scenario-data year information
#' @param rename_lyrs a list with two elements: 'layer_name' vector of layers to be renamed and 'to' vector of new names with placement (index) matching current layer name
#'
#' @return

scenario_data_include <- function(scen_data_years, scenario_yrs, new_lyrs = "", rename_lyrs = ""){

  cols <- names(scen_data_years)
  new_scen_yrs <- setdiff(scenario_yrs, scen_data_years$scenario_year %>% unique())

  ## the new scenario years rows for existing layers
  add_scen_rows <- scen_data_years %>%
    dplyr::select(layer_name) %>%
    unique() %>%
    dplyr::mutate(scenario_year = list(new_scen_yrs), data_year = NA) %>%
    tidyr::unnest()

  ## rows for new layers with the given range of scenario years
  add_lyr_rows <- tibble::tibble(layer_name = new_lyrs,
                                 scenario_year = list(scenario_yrs),
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
#' @param scenario_yrs the scenario years to be included in the updated scenario_data_years.csv
#' @param approach
#'
#' @return

scenario_data_align <- function(scen_data_years, lyr_name, data_yrs, scenario_yrs, approach = ""){

  ## rows of scenario_data_years we are not updating
  keep_rows <- scen_data_years %>%
    dplyr::filter(layer_name != lyr_name)

  if(length(scenario_yrs) != length(data_yrs)){
    print("unequal numbers of scenario years and data years")
  }
  all_data_yrs <- data_yrs

  ## data_yrs we may actually want to match with scenario_yrs
  data_yrs <- data_yrs[data_yrs <= max(scenario_yrs)] %>%
    sort(decreasing = TRUE) %>% unique()
  data_yrs <- data_yrs[1:length(scenario_yrs)]

  ## matrix M with potential edge-weights
  M <- array(sort(rep(scenario_yrs, length(data_yrs)),
                  decreasing = TRUE) - rep(data_yrs, length(scenario_yrs)),
             c(length(data_yrs), length(scenario_yrs)),
             list(data_yrs, sort(scenario_yrs, decreasing = TRUE)))

  match_years <- data.frame(scenario_year = sort(scenario_yrs, decreasing = TRUE),
                            data_year = NA)

  if(str_detect(approach, "intervals|steps|timestep")){
    ## don't actually want to assign if si to dj if |si-dj| > reasonable_diff
    reasonable_diff <- ifelse(str_detect(approach, "max step|maximum step|with step|max diff|maximum|max.|difference"),
                              as.numeric(gsub("\\D", "", approach)), 5)
    for (s in 1:length(scenario_yrs)){
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

    ## find min sum of edge-weights where F: S->D is onto and deg(si)=1 for all si in scenario_yrs
    for (s in 1:length(scenario_yrs)){
      match_years[s, "data_year"] <- which.min(M[,s][M[,s] >= 0]) %>%
        names() %>% as.integer()
    }
  }

  layer_years_align <- tibble::tibble(
    layer_name = lyr_name,
    scenario_year = list(scenario_yrs)) %>%
    tidyr::unnest() %>%
    dplyr::left_join(match_years, by = "scenario_year")
  data_yrs_unused <- setdiff(all_data_yrs, unique(layer_years_align$data_year))

  years_align <- layer_years_align %>%
    rbind(keep_rows) %>%
    dplyr::arrange(layer_name, scenario_year, data_year)

  return(years_align)
}
