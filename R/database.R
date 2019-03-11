## Libraries
library(tidyverse)
library(here)
library(httr)
library(RCurl)

## Directories
dir_bhi <- here::here()
dir_spatial <- file.path(dir_bhi, "spatial")

## Functions

#' copy layers from 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder
#'
#' @param assessment_path file path to assessment folder within bhi repo
#' @param repo_location url pointing to the bhi repo on github
#'
#' @return

copy_layers_for_assessment <- function(assessment_path, repo_location){

  repo_loc <- repo_location # url to bhi prep github repository

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
