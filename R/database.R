## Libraries
source(file.path("R", "common.R"))
library(dbplyr)
library(DBI)
library(odbc)
library(config)
library(httr)

## helpful database functions in dplyr: copy_to,
## functions in DBI: dbConnect,
## from dbplot: creates ggplot object using data within database


## Extra Info

## https://db.rstudio.com/best-practices/drivers/
## https://db.rstudio.com/databases/postgresql/
## https://db.rstudio.com/odbc/
## Connecting to database w/ method supporting DBI package makes using dplyr as a front-end possible
## odbc connects via open database connectivity protocol

## Setting up OBCD drivers (MacOS instructions):
## first install homebrew if not installed (https://brew.sh/)
## then via terminal, install the unixODBC library and PostgreSQL ODBC ODBC Drivers:
# brew install unixodbc
# brew install psqlodbc

## edit odbcinst.ini (driver options) and odbc.ini (connection options) files
## to find exact location use: odbcinst -j
## or rather than depending on DSNs use config package:
## 'allows connection code in R to reference external file that defines values based on the environment'


## Functions

#' copy prepared assessment layers to bhi repo
#'
#' copies layers from 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder, via github repo url
#'
#' @param assessment_path file path to assessment folder within bhi repo
#' @param repo_location url pointing to the bhi repo on github
#'
#' @return

copy_layers_for_assessment <- function(assessment_path, repo_location){

  repo_loc <- repo_location # url to bhi prep github repository

  req <- httr::GET("https://api.github.com/repos/OHI-Science/bhi-prep/git/trees/master?recursive=1")
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F) %>%
    stringr::str_extract_all("prep/layers/.*.csv", simplify = TRUE) %>%
    as.data.frame()

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
