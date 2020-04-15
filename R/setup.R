## Libraries ----
library(here)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ohicore)

## Global ----

## SET BHI GITHUB URLS HERE
bhi_gh <- "https://github.com/OHI-Science/bhi"
bhiprep_gh <- "https://github.com/OHI-Science/bhi-prep"

bhi_gh_raw <- stringr::str_replace(bhi_gh, "github.com", "raw.githubusercontent.com")
bhi_gh_api <- bhi_gh %>%
  stringr::str_replace("github.com", "api.github.com/repos") %>%
  paste("git/trees/master?recursive=1", sep = "/")

bhiprep_gh_raw <- stringr::str_replace(bhiprep_gh, "github.com", "raw.githubusercontent.com")
bhiprep_gh_api <- bhiprep_gh %>%
  stringr::str_replace("github.com", "api.github.com/repos") %>%
  paste("git/trees/master?recursive=1", sep = "/")

## SET MAIN AUX BHI DIRECTORY HERE
dir_B <- file.path(dirname(here()), "bhi-data", "BHI 2.0")
if(!file.exists(dir_B)){
  ## warning if BHI internal, shared directory doesn't exist
  warning(sprintf(
    "Shared BHI directory does not exist. Do you need to mount the server:\n%s",
    dir_B
  ))
}

bhi_db_file <- "/Users/eleanorecampbell/Desktop/bhi-config.sqlite" # local path to sqlite db for now...
bhi_db_con <- DBI::dbConnect(RSQLite::SQLite(), bhi_db_file)
