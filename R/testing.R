## Libraries
library(tidyverse)
library(here)
library(tools)

## Directories
dir_bhi <- here::here()
dir_R <- file.path(dir_bhi, "R")

## Functions

#' configure functions.R for testing
#'
#' compiles files listed in a local 'test_functions.csv' into the single \code{functions.R} that ohicore needs
#'
#' @param assessment_path filepath specifying the assessment main folder containing 'conf' and 'testing' folders
#' @return revises functions.R at existing location in 'conf' folder; no output

configure_functions <- function(assessment_path){

  functionsR_path <- sprintf("%s/conf/functionsR_testing_setup.R", assessment_path)
  fun_scripts_path <- sprintf("%s/testing/alt_functions", assessment_path)

  ## note: there must be a 'test_functions.csv' in directory where this function is called
  fun_scripts_lst <- readr::read_csv("test_functions.csv", col_names = FALSE) %>%
    as.list() %>%
    lapply(function(x) paste0(assessment_path, "/testing/alt_functions/", x))

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
#' copies the versions of layers we want to use for testing (as specified in the 'alt_layers_full_table.csv') into `bhi-prep/prep/layers`
#' the `copy_layers_for_assessment` function in `bhi/R/common.R` is used afterwards to copy layers to the layers folder in the assessment repo (bhi/baltic)
#' compares the specified layers to the layers named in \code{functions.R} to confirm all layers needed given the models, will be copied to 'layers'
#' automatically writes the name of copied file into 'filename' column of 'layers.csv'
#'
#' @param assessment_path filepath specifying the assessment main folder containing 'conf' and 'testing' folders
#' @param prep_path bhi-prep filepath specifying the main folder containing goal/pressure/resilience subfolders
#'
#' @return a table comparing layers required by \code{functions.R} to those specified in alt_layers_full_table.csv; revises 'layers.csv' and contents of 'layers' folder

configure_layers <- function(assessment_path, prep_path, test_path){

  test_layers_path <- sprintf("%s/testing/alt_layers_full_table.csv", assessment_path)
  functionsR_path <- sprintf("%s/conf/functionsR_testing_setup.R", assessment_path)

  print("this function assumes you've already made sure alt_layers_full_table.csv is up to date!")

  use_layers <- readr::read_delim(test_layers_path, delim = ";") %>% # test_layers = "test_layers_table.csv"
    dplyr::filter(use == "YES")

  ## extract names of layers specified in functions.R (i.e. which do functions.R require)
  functionsR_layers <- scan(file = functionsR_path, what = "character", sep = "\n") %>%
    grep(pattern = "\\s*#{1,}.*", value = TRUE, invert = TRUE) %>% # remove commented lines
    stringr::str_split(" ") %>% unlist() %>% stringr::str_subset("^layers.*") %>%
    stringr::str_remove("layers=\'") %>%
    stringr::str_extract("[a-z_]*") %>%
    stringr::str_sort()

  ## error checking: compare layers required by functions.R to the layers specified in table
  check_layers_table <- tibble::tibble(layer = functionsR_layers, fun_layer = "YES") %>%
    dplyr::full_join(use_layers, by = "layer") %>%
    dplyr::mutate(required = ifelse(is.na(fun_layer) == TRUE, "NOT REQUIRED", "")) %>%
    dplyr::mutate(specified = ifelse(is.na(filename) == TRUE, "SPECIFY VERSION", "")) %>%
    dplyr::group_by(layer) %>%
    dplyr::add_tally() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(multiple = ifelse(n > 1, "OVERSPECIFIED", "")) %>%
    dplyr::select(-fun_layer, -use, -n)
  readr::write_csv(check_layers_table,
                   sprintf("%s/test_layers_used.csv", test_path),
                   append = FALSE)

  one_to_one <- ifelse(length(unique(check_layers_table$multiple)) > 1, FALSE, TRUE)
  onto <- ifelse(length(unique(check_layers_table$specified)) > 1, FALSE, TRUE)
  stopifnot(one_to_one, onto) # stop if error- don't proceed to layer-copying step

  ## copy over the layer versions for calculations
  use_layers <- check_layers_table %>%
    dplyr::filter(required != "NOT REQUIRED") %>%
    dplyr::mutate(path = ifelse(is.na(subfolder) == FALSE,
                                paste(goal, subfolder, layer, filename, sep = "/"),
                                paste(goal, layer, filename, sep = "/")))

  continue <- readline(prompt = sprintf("overwrite current contents of '%s/layers' (continue: yes/no)? ", prep_path))
  stopifnot(continue == "yes")

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
