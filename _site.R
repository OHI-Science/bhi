## Libraries ----
## load any libraries needed across website pages
suppressPackageStartupMessages({
  library(readr)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(knitr)
  library(httr)
  library(timevis)
  library(widgetframe)
  library(geojsonio)
  library(leaflet)
  library(formattable)
})


## General ----

## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

key <- "bhi"
study_area <- "Baltic"

assess_year <- 2019 # CHANGE BHI ASSESSMENT YEAR HERE!
dir_assess <- "baltic" # SET BHI ASSESSMENT FOLDER HERE!
rgn_ids_vec <- 1:42
subbasin_ids_vec <- 501:517
# projstringCRS <- raster::crs("+proj=longlat +datum=WGS84 +no_defs") # spatial data use lat/long coords on WGS84
# filesep <- .Platform$file.sep

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


# ## read in variables if they exist
# scores_csv <- file.path(dir_scenario_gh, "scores.csv")
# layers_csv <- file.path(dir_scenario_gh, "layers.csv")
# conf_csv <- file.path(dir_scenario_gh, "conf", "goals.csv")
#
# if(RCurl::url.exists(scores_csv)){
#   scores <- readr::read_csv(scores_csv)
# }
# if(RCurl::url.exists(layers_csv)){
#   layers <- readr::read_csv(layers_csv)
# }
# if(RCurl::url.exists(conf_csv)){
#   weight <- readr::read_csv(conf_csv) %>%
#     select(goal, weight)
# }


## save local copies of Rmds to knit-child
to_copy <- c("goals.Rmd", "layers.Rmd")

for(f in to_copy){
  fp <- file.path(bhi_gh_raw, "master", "supplement", "web", f)

  ## if the url exists, save a copy
  if(RCurl::url.exists(fp)){

    f_web <- readr::read_lines(fp)
    if(tools::file_ext(fp) == "Rmd"){
      f_local <- paste0("local_", basename(fp))
    } else {f_local <- basename(fp)}

    readr::write_lines(f_web, path = f_local, append = FALSE)
    message(sprintf("saving %s", f_local))

  } else {message(sprintf("%s does not exist", fp))}
}
