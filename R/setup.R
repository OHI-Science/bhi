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

bhi_gh <- "https://github.com/OHI-Science/bhi"
bhiprep_gh <- "https://github.com/OHI-Science/bhi-prep"

bhi_gh_raw <- stringr::str_replace(
  bhi_gh,
  pattern = "github.com",
  replacement = "raw.githubusercontent.com"
)
bhiprep_gh_raw <- stringr::str_replace(
  bhiprep_gh,
  pattern = "github.com",
  replacement = "raw.githubusercontent.com"
)

bhi_gh_api <- stringr::str_replace(
  bhi_gh,
  pattern = "github.com",
  replacement = "api.github.com/repos"
) %>% paste(
  "git/trees/master?recursive=1", sep = "/"
  )
bhiprep_gh_api <- stringr::str_replace(
  bhiprep_gh,
  pattern = "github.com",
  replacement = "api.github.com/repos"
) %>% paste(
  "git/trees/master?recursive=1", sep = "/"
  )

bhi_db_file <- "/Users/eleanorecampbell/Desktop/bhi-config.sqlite" # for now... depends on local path to sqlite db...
bhi_db_con <- DBI::dbConnect(RSQLite::SQLite(), bhi_db_file)

dir_assess <- file.path(here::here(), "baltic") # CHANGE BHI ASSESSMENT DIRECTORY HERE!
dir_prep <- file.path(dirname(here::here()), "bhi-prep", "prep")
dir_test <- file.path(dir_assess, "testing")

dir_B <- file.path(c("Darwin" = "/Volumes/BHI_share", # "Windows" = ?
                     "Linux" = "/home/shares/ohi")[[ Sys.info()[["sysname"]] ]], "BHI 2.0") # CHANGE MAIN AUX BHI DIRECTORY HERE!
if(Sys.info()[["sysname"]] != "Linux" & !file.exists(dir_B)){ # warning if BHI internal, shared directory doesn't exist
  paste("The BHI directory dir_share set in R/common.R does not exist.",
        sprintf("Do you need to mount the BHI server: %s?", dir_B))
}

## Apply theme
thm <- apply_bhi_theme()
