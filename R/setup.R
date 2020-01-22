## Libraries
library(here)
library(tidyverse)
library(zoo)
library(ohicore)
library(httr)


## General
assess_year <- 2019 # CHANGE BHI ASSESSMENT YEAR HERE!
rgn_ids_vec <- 1:42
subbasin_ids_vec <- 501:517


## Connections and Directories

## CHANGE BHI ASSESSMENT DIRECTORY HERE!
dir_assess <- here("baltic")


bhi_gh <- "https://github.com/OHI-Science/bhi"
bhiprep_gh <- "https://github.com/OHI-Science/bhi-prep"

bhi_gh_raw <- stringr::str_replace(bhi_gh, "github.com", "raw.githubusercontent.com")
bhiprep_gh_raw <- stringr::str_replace(bhiprep_gh, "github.com", "raw.githubusercontent.com")

bhi_gh_api <- stringr::str_replace(bhi_gh, "github.com", "api.github.com/repos") %>%
  paste("git/trees/master?recursive=1", sep = "/")
bhiprep_gh_api <- stringr::str_replace(bhiprep_gh, "github.com", "api.github.com/repos") %>%
  paste("git/trees/master?recursive=1", sep = "/")


bhi_db_file <- "/Users/eleanorecampbell/Desktop/bhi-config.sqlite" # for now... depends on local path to sqlite db...
bhi_db_con <- DBI::dbConnect(RSQLite::SQLite(), bhi_db_file)


## CHANGE MAIN AUX BHI DIRECTORY HERE!
dir_B <- file.path(
  c("Darwin" = "/Volumes/BHI_share",
    # "Windows" = "?",
    "Linux" = "/home/shares/ohi")[[ Sys.info()[["sysname"]] ]],
  "BHI 2.0"
)
if(Sys.info()[["sysname"]] != "Linux" & !file.exists(dir_B)){
  ## warning if BHI shared internal directory doesn't exist
  paste(
    "The BHI directory dir_share set in R/common.R does not exist.",
    sprintf("Do you need to mount the BHI server: %s?", dir_B)
  )
}
