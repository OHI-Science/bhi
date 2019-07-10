## load any libraries needed across website pages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(knitr)
  library(timevis)
  library(widgetframe)
  library(geojsonio)
  library(leaflet)
})

## brewed vars
key <- "bhi"
study_area <- "Baltic"

## github paths...
dir_gh <- "https://raw.githubusercontent.com/OHI-Science/bhi"
dir_scenario_gh <- file.path(dir_gh, "master/baltic")
dir_spatial_gh <- file.path(dir_gh, "master/spatial")
dir_supplement_gh <- file.path(dir_gh, "blob/master/supplement/web")

## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

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
#
# ## save local copies of Rmds to knit-child
# to_copy <- c()
#
# for(f in to_copy){
#   fp <- file.path(dir_scenario_gh, f)
#
#   ## if the url exists, save a copy
#   if(RCurl::url.exists(fp)){
#
#     f_web <- readr::read_lines(fp)
#     if(tools::file_ext(fp) == "Rmd"){
#       f_local <- paste0("local_", basename(fp))
#     } else {f_local <- basename(fp)}
#
#     readr::write_lines(f_web, path = f_local, append = FALSE)
#     message(sprintf("saving %s", f_local))
#
#   } else {message(sprintf("%s does not exist", fp))}
# }
#
#
# ## save local copy of bhi/supplement/web/goals.Rmd
# local_goals <- "https://github.com/OHI-Science/bhi/blob/master/supplement/web/goals.Rmd"
#
#
# conf_goals_rmd <- file.path(dir_scenario_gh, "conf", "goals.Rmd")
#
# if(RCurl::url.exists(conf_goals_rmd)){
#   conf_goals <- readr::read_lines(conf_goals_rmd)
#   readr::write_lines(conf_goals, path = "conf_goals.Rmd", append = FALSE)
# }
