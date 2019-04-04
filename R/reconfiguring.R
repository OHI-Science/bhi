## Libraries
source(file.path(here::here(), "R", "common.R"))
library(tools)
library(tidyr)
library(tibble)
library(httr)

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

update_alt_layers <- function(assessment_path = dir_baltic, prep_path = dir_prep, assess_year){

  print("if creating an entirely new layer not just a different version, you must have added it in layers.csv")
  lyrscsv <- readr::read_csv(file.path(assessment_path, "layers.csv"))

  alt_layers <- readr::read_csv(file.path(assessment_path, "testing", "alt_layers.csv"))
  if(dim(alt_layers)[2] == 1 & stringr::str_detect(names(alt_layers), ";")){
    alt_layers <- readr::read_delim(file.path(assessment_path, "testing", "alt_layers.csv"), delim = ";")
  }
  if(dim(alt_layers)[2] == 1){
    stop(print("double check that alt_layers.csv is delimited with commas or semicolons"))
  }
  layer_names <- lyrscsv$layer %>% unique() %>% as.list()

  ## identify new layers (in the bhi-prep repo) to add to the full 'alt_layers.csv' table
  add_alt_layers <- list.files(prep_path, recursive = TRUE) %>%
    grep(pattern = file.path(sprintf(".*v%s", assess_year), "output", "[a-z0-9_]+.csv$"),
         value = TRUE) %>% # gets things of form v2019/output/goal_layer_final.csv
    stringr::str_remove("prep") %>%
    enframe(name = NULL) %>% # makes into a tibble
    dplyr::mutate(filename = basename(as.character(value)),
                  goal = stringr::str_extract(value, pattern = "[a-zA-Z]+"),
                  use = NA,
                  layer_names = list(layer_names),
                  subfolder = stringr::str_extract(value, pattern = "(\\w+ ?){1}/v2") %>% # ...but beyond year 2999?
                    stringr::str_remove("/v2")) %>%

    dplyr::rowwise() %>%
    dplyr::mutate(match_found = list(stringr::str_match(filename, as.character(layer_names) %>%
                                                          stringr::str_remove_all("\"")))) %>%
    dplyr::mutate(layer = match_found %>%
             lapply(function(x){ x[!is.na(x)] }) %>%
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

layerscsv_edit <- function(layers_object, lyr_file, lyr_name, assessment_path, update_with = NULL, write = FALSE){

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


#' configure functions.R for testing
#'
#' compiles files listed in a local 'test_functions.csv' into the single \code{functions.R} that ohicore needs
#'
#' @param assessment_path filepath specifying the assessment main folder containing 'conf' and 'testing' folders
#' @param test_funs_list a list of filepaths from (not including) 'testing/alt_functions' to the functions we want to use in analysis
#' @return revises functions.R at existing location in 'conf' folder; no direct output

configure_functions <- function(assessment_path, test_funs_list = NULL){

  functionsR_path <- file.path(assessment_path, "conf", "functions.R")
  fun_scripts_path <- file.path(assessment_path, "testing", "alt_functions")

  ## get filepaths list of functions we want to collect into functions.R
  if(is.null(test_funs_list)){ # only works if 'test_functions.csv' exists in directory
    if(file.exists("test_functions.csv")){
      fun_scripts_lst <- readr::read_csv("test_functions.csv", col_names = FALSE) %>%
        as.list() %>%
        lapply(function(x) file.path(assessment_path, "testing", "alt_functions", x))
    } else { stop("no 'test functions' list provided, and no 'test_function.csv' found") }
  } else { # better to actually provide the list
    fun_scripts_lst <- test_funs_list %>%
      lapply(function(x) file.path(assessment_path, "testing", "alt_functions", x))
  }

  ## confirm all specified function versions actually exist before overwriting functions.R
  chk <- file.exists(fun_scripts_lst[[1]])
  if(any(!chk)){
    m <- basename(fun_scripts_lst[[1]][!chk]) # file.exists is FALSE ie, functions aren't at given location
    stop(sprintf("the following do not exist in given locations: %s",
                 paste(m, collapse = ", ")))
  }

  sink(file = functionsR_path, append = FALSE) # overwrite functions.R with first function
  cat(scan(file = fun_scripts_lst[[1]][1], what = "character", sep = "\n"), sep = "\n")

  sink(file = functionsR_path, append = TRUE) # append subsequent functions
  for(i in fun_scripts_lst[[1]][-1]){
    cat(scan(file = i, what = "character", sep = "\n"), sep = "\n")
  }
  closeAllConnections() # stop the sinking
}


#' configure layers for testing
#'
#' copies the versions of layers we want to use for testing (as specified in the 'testing/alt_layers.csv') into `bhi-prep/prep/layers`
#' the `copy_layers_for_assessment` function in `bhi/R/common.R` is used afterwards to copy layers to the layers folder in the assessment repo (bhi/baltic)
#' compares the specified layers to the layers named in \code{functions.R} to confirm all layers needed given the models, will be copied to 'layers'
#' automatically writes the name of copied file into 'filename' column of 'layers.csv'
#'
#' @param assessment_path filepath specifying the assessment main folder containing 'conf' and 'testing' folders
#' @param prep_path bhi-prep filepath specifying the main folder containing goal/pressure/resilience subfolders
#' @param test_path filepath to subfolder of 'testing' that contains everything fro a sigle analysis; any outputs are saved to/read from here
#'
#' @return a table comparing layers required by \code{functions.R} to those specified in alt_layers_full_table.csv; revises 'layers.csv' and contents of 'layers' folder

configure_layers <- function(assessment_path, prep_path, test_path){

  test_layers_path <- file.path(assessment_path, "testing", "alt_layers.csv")
  functionsR_path <- file.path(assessment_path, "conf", "functions.R")

  print("this function assumes you've already made sure alt_layers.csv is up to date!")
  use_layers <- readr::read_delim(test_layers_path, delim = ";") %>%
    dplyr::filter(use == "YES")

  ## extract names of layers specified in functions.R (i.e. which do functions.R require)
  ## then, compare layers required by functions.R to the layers specified in table
  ## save compare layers table to testing folder for review/reference
  functionsR_layers <- goal_layers(functionsR_path, goal_code = "all")

  check_layers_table <- tibble::tibble(layer = functionsR_layers, fun_layer = "YES") %>%
    dplyr::full_join(use_layers, by = "layer") %>%
    dplyr::mutate(required = ifelse(is.na(fun_layer) == TRUE, "not required", "required")) %>%
    dplyr::mutate(specified = ifelse(is.na(filename) == TRUE, "specify version", "exists")) %>%
    dplyr::group_by(layer) %>%
    dplyr::add_tally() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(multiple = ifelse(n > 1, "overspecified", "unique")) %>%
    dplyr::select(-fun_layer, -use, -n)
  readr::write_csv(check_layers_table, file.path(test_path, "test_layers_used.csv"))

  ## checks: stop if error, don't proceed to layer-copying step...
  one_to_one <- ifelse(length(unique(check_layers_table$multiple)) > 1, FALSE, TRUE)
  onto <- ifelse(length(unique(check_layers_table$specified)) > 1, FALSE, TRUE)
  if(!one_to_one){
    stop("instances found where multiple files are specified for a single layer")
  }
  if(!onto){
    stop("instances found where no files are specified for a layer")
  }

  ## copy over the layer versions for calculations
  use_layers <- check_layers_table %>%
    dplyr::filter(required == "required") %>%
    dplyr::mutate(
      path = ifelse(specified == "specify version", NA,
                    ifelse(is.na(subfolder),
                           paste(goal, layer, filename, sep = "/"),
                           paste(goal, subfolder, layer, filename, sep = "/"))
      ))
  # continue <- readline(prompt = sprintf("overwrite current contents of '%s/layers' (continue: yes/no)? ", prep_path))
  # stopifnot(continue == "yes")

  ## copy layers to 'layers' folder, and write version into 'filename' column of 'layers.csv'
  for(i in 1:nrow(use_layers)){
    use_layer_path <- sprintf("%s/%s", prep_path, use_layers$paths[i]) # the layer file to copy is located here
    put_layer_path <- sprintf("%s/layers/%s", prep_path, use_layers$filename[i]) # where to put copied layer
    file.copy(use_layer_path, put_layer_path, overwrite = TRUE)

    ## regex notes: layer must be left of filename col, and filename is assumed to be 5-50 alphanumeric char
    tmp <- scan(sprintf("%s/layers.csv", assessment_path), what = "character", sep = "\n") %>%
      stringr::str_replace(pattern = sprintf("%s\\\",\\\"\\S{5,50}.csv", use_layers$layer[i]),
                           sprintf("%s\\\",\\\"%s", use_layers$layer[i], use_layers$filename[i])) %>%
      stringr::str_split(",(?!\\s+)", simplify = TRUE) %>%
      as.data.frame()
    colnames(tmp) <- c(scan(sprintf("%s/layers.csv", assessment_path), what = "character", sep = ",")[1:26])
    write.csv(tmp[-1,], sprintf("%s/layers.csv", assessment_path), row.names = FALSE)

    print(sprintf("copied %s to layers folder", use_layers$filename[i]))
  }
  return(check_layers_table) # return so can visually inspect
}


# configure_pressures <- function(){}
#
#
# configure_resilience <- function(){}


#' copy prepared layers from bhi-prep to bhi repo
#'
#' copies layers from prep 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder, from github repo url
#'
#' @param assessment_path file path to assessment folder within bhi repo
#' @param copy_layers
#' @param repo_loc url pointing to the bhi repo on github
#'
#' @return

copy_lyrs_from_bhiprep <- function(assessment_path, copy_layers = list("all"), repo_loc = NULL){

  if(is.null(repo_loc)){
    repo_loc <- paste0(prep_repo_raw, "/master") # raw bhi-prep github repo url, prep_repo_raw from common.R
  }
  if(unlist(copy_layers) == "all"){
    print("copying over all layers from bhi-prep repository 'layers' folder")
    print("note: will overwrite layers in bhi repo 'layers' folder with bhi-prep versions from github, of same name")
    filelist <- bhiprep_github_layers(github_api_url = bhi_prep_api) # function defined and bhi_prep_api assigned in common.R
  } else {
    filelist <- data.frame(V1 = unlist(copy_layers))
  }
  for(i in 1:length(filelist$V1)){
    tmp <- filelist[i,1] %>% as.character()
    if(stringr::str_length(tmp) != 0){
      url <- sprintf("%s/%s", repo_loc, tmp)
      name_of_layer <- tmp %>% stringr::str_extract("[a-z0-9_]+.csv$")
      httr::GET(url, write_disk(sprintf("%s/layers/%s", assessment_path, name_of_layer),
                                overwrite = TRUE))
    }
  }
}
