## Libraries
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

## General
assess_year <- 2019 # CHANGE BHI ASSESSMENT YEAR HERE!
projstringCRS <- raster::crs("+proj=longlat +datum=WGS84 +no_defs") # spatial data use lat/long coords on WGS84
filesep <- .Platform$file.sep

bhi_repo_loc <- "https://github.com/OHI-Science/bhi"
bhiprep_repo_loc <- "https://github.com/OHI-Science/bhi-prep/prep"
bhi_repo_raw <- "https://raw.githubusercontent.com/OHI-Science/bhi"
bhiprep_repo_raw <- "https://raw.githubusercontent.com/OHI-Science/bhi-prep"
bhi_api <- "https://api.github.com/repos/OHI-Science/bhi/git/trees/master?recursive=1"
bhiprep_api <- "https://api.github.com/repos/OHI-Science/bhi-prep/git/trees/master?recursive=1"

## Directories
dir_bhi <- here::here()
dir_baltic <- file.path(dir_bhi, "baltic") # CHANGE BHI ASSESSMENT DIRECTORY HERE!
dir_spatial <- file.path(dir_bhi, "spatial") # spatial folder of bhi repo
dir_prep <- file.path("..", "bhi-prep") # only works if assessment and prep repos are in same main (github) directory...
dir_B <- file.path(c("Darwin" = "/Volumes/BHI_share", # "Windows" = ?
           "Linux" = "/home/shares/ohi")[[ Sys.info()[["sysname"]] ]], "BHI 2.0") # CHANGE MAIN AUX BHI DIRECTORY HERE!
if(Sys.info()[["sysname"]] != "Linux" & !file.exists(dir_B)){ # warning if BHI internal, shared directory doesn't exist
  paste("The BHI directory dir_share set in R/common.R does not exist.",
        sprintf("Do you need to mount the BHI server: %s?", dir_B))
}

## Functions

funsR_goals_list <- function(functionsR_path = NULL, funs_text = NULL){
  if(is.null(functionsR_path) & is.null(funs_text)){
    stop("must provide path to functions.R, or its contents as a character vector")
  }
  if(!is.null(functionsR_path)){
    txt <- scan(file = functionsR_path, what = "character", sep = "\n")
  } else {txt <- funs_text}
  breaks_str <- "^[A-Z]{2,3}\\s<-\\sfunction\\(|^[A-Z]{2,3}\\s=\\sfunction\\("
  funs_goals <- stringr::str_extract(grep(breaks_str, txt, value = TRUE), "^[A-Z]{2,3}")
  return(funs_goals)
}


#' extract from functions.R the text of a specific goal function
#'
#' @param functionsR_path location of functions.R to extract goal from
#' @param goal_code the two or three letter code indicating the goal or subgoal
#'
#' @return lines of the goal function as a character vector
goal_function <- function(functionsR_path = NULL, funs_text = NULL, goal_code, comments = FALSE){
  goal_code <- stringr::str_to_upper(goal_code)

  if(is.null(functionsR_path) & is.null(funs_text)){
    stop("must provide path to functions.R, or its contents as a character vector")
  }
  if(!is.null(functionsR_path)){
    txt <- scan(file = functionsR_path, what = "character", sep = "\n")
  } else {txt <- funs_text}
  breaks_str <- "^[A-Z]{2,3}\\s<-\\sfunction\\(|^[A-Z]{2,3}\\s=\\sfunction\\("
  funs_breaks <- grep(breaks_str, txt)
  funs_goals <- stringr::str_extract(grep(breaks_str, txt, value = TRUE), "^[A-Z]{2,3}")
  if(!(goal_code %in% funs_goals)){
    stop("function for the given goal code not found in functions.R")
  }
  fun_start <- grep(pattern = sprintf("^%s\\s<-\\sfunction\\(|^%s\\s=\\sfunction\\(",
                                      goal_code, goal_code), txt)
  fun_end <- funs_breaks[1 + which.min(abs(fun_start - funs_breaks))] - 1
  goal_fun <- txt[fun_start:fun_end]

  ## remove commented lines if comments arg is falls
  if(comments == FALSE){
    goal_fun <- goal_fun %>%
      grep(pattern = "\\s*#{1,}.*", value = TRUE, invert = TRUE)
  }
  return(goal_fun) # cat(goal_fun, sep = "\n")
}


#' extract from functions.R layers associated with a specific goal function
#'
#' @param functionsR_path location of functions.R to extract goal from
#' @param goal_code the two or three letter code indicating the goal or subgoal
#'
#' @return character vector naming layers
goal_layers <- function(functionsR, goal_code = "all"){

  goal_code <- stringr::str_to_upper(goal_code) %>% unlist()
  if(goal_code != "ALL" & any(!goal_code %in% funsR_goals_list(functionsR))){
    print("note: no function found for some of the given goals")
  }
  if(stringr::str_to_upper(goal_code) == "ALL"){
    txt <- scan(file = functionsR_path, what = "character", sep = "\n") %>%
      grep(pattern = "\\s*#{1,}.*", value = TRUE, invert = TRUE)
  } else {
    txt <- vector()
    for(gc in goal_code){
      txt <- c(txt, goal_function(functionsR, goal_code = gc, comments = FALSE))
    }
  }

  ## extract names of layers specified in functions.R (i.e. which do functions.R require)
  functionsR_layers <- txt %>%
    gsub(pattern = "layer_nm\\s{1,}=\\s{1,}", replacement = "layer_nm=") %>%
    # gsub(pattern = "", replacement = "layer_nm=") %>% # to catch pressure + resilience layers given with a different pattern
    stringr::str_split(" ") %>%
    unlist() %>%
    stringr::str_subset("layer_nm.*") %>% # pattern to ID layer fed into a function within fuctions.R
    stringr::str_extract("\"[a-z0-9_]*\"|\'[a-z0-9_]*\'") %>%
    stringr::str_sort() %>%
    stringr::str_remove_all("\"|\'") # remove any quotation marks around

  return(functionsR_layers)
}


#' create dataframe with names of the prepared layers within bhi-prep layers folder on github
#'
#' @return creates
bhiprep_github_layers <- function(github_api_url = bhi_prep_api){
  req <- httr::GET(github_api_url)
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = FALSE) %>%
    stringr::str_extract_all("prep/layers/.*.csv", simplify = TRUE) %>%
    as.data.frame()
  return(filelist)
}


#' compare columns and/or rows between two tables
#'
#' @param tab1 the table to compare to (what we hope table 2 looks like)
#' @param tab2 the table to -within some specified columns- check for missing/extra rows with respect to some 'key' variable
#' @param check_cols the columns to look for differences within; essentially taking tab1 %>% select(check_cols) to compare tab2 against
#' @param key_row_var the variable for groupings of interest e.g. by which you would join or maybe gather the data
#'
#' @return
compare_tabs <- function(tab1, tab2, key_row_var, check_cols = "all", check_for_nas = NA){

  ## setup, load tables, get key variable info
  if(!is.data.frame(tab1)){tab1df <- read.csv(tab1, stringsAsFactors = FALSE)
  } else {tab1df <- tab1}
  if(!is.data.frame(tab2)){tab2df <- read.csv(tab2, stringsAsFactors = FALSE)
  } else {tab2df <- tab2}

  if("all" %in% check_cols){
    check_cols <- names(tab1)
    print("using all columns of table 1 in the comparision")
  }

  tab1_keys <- unique(tab1df[, key_row_var])
  tab2_keys <- unique(tab2df[, key_row_var])
  keys <- dplyr::union(tab1_keys, tab2_keys)

  ## checks and messages
  checks <- list(
    chk_nrows = nrow(tab1df) != nrow(tab2df), # true if number of rows unequal
    chk_missing_key = setdiff(tab1_keys, tab2_keys), # key vars missing from tab2
    chk_extra_key = setdiff(tab2_keys, tab1_keys)) # unexpected key vars in tab2

  not_in_tab1 <- setdiff(check_cols, names(tab1df)) # check_cols not in tab1
  not_in_tab2 <- setdiff(check_cols, names(tab2df)) # check_cols not in tab2
  if(length(not_in_tab1) != 0 | length(not_in_tab2) != 0){
    stop(sprintf("given 'check' column(s) not found in one or both of the tables: %s",
                 dplyr::union(not_in_tab1, not_in_tab2) %>%
                   paste(collapse = ", ")))
  }
  if(!(key_row_var %in% names(tab1df) & key_row_var %in% names(tab2df))){
      stop(paste("one or both tables are missing the key row variable:", key_row_var))
  }
  if(!is.na(check_for_nas)){
    chk_na <- array(NA, dim = c(nrow(tab1df), length(check_for_nas)),
                    dimnames = list(tab1df[, key_row_var], check_for_nas))
    for(k in 1:length(check_for_nas)){
      chk_na[, k] <- is.na(tab2df[, check_for_nas[k]]) # TRUE where are NAs in column
    }
  } else {chk_na <- NA}

  ## do 'key' var groupings have missing or extra rows within each 'check_column' var
  comparisons <- list(missing = list(), extra = list())

  if(length(checks$chk_missing_key) > 0 | length(checks$chk_extra_key) > 0){
    message("levels/groupings of key var don't match; comparisons results will apply only to intersection")
  }
  for(i in check_cols){ # there's probably a better way with purrr or apply funs...
    e <- list()
    m <- list()
    for(j in keys){ # from stops above check_cols incl. key_row_var must be in both tabs
      e[[j]] <- setdiff(unique(tab2df[tab2df[key_row_var] == j, i]),
                        unique(tab1df[tab1df[key_row_var] == j, i]))
      m[[j]] <- setdiff(unique(tab1df[tab1df[key_row_var] == j, i]),
                        unique(tab2df[tab2df[key_row_var] == j, i]))
    }
    comparisons$extra[[i]] <- e
    comparisons$missing[[i]] <- m
  }
  diffs_found <- ifelse(length(c(unlist(missing), unlist(extra))) > 0, TRUE, FALSE)
  return(list(diffs_found, checks, chk_na, comparisons))
}


#' quickly filter score data
#'
#' @param score_data dataframe of OHI score data with goal, dimension, region_id, year and score columns e.g. output of ohicore::CalculateAll
#' @param dims the dimensions to extract score data for
#' @param goals the goals to extract score data for
#' @param rgns the regions to extract score data for
#' @param years the years to extract score data for
#'
#' @return a dataframe of OHI scores filtered by the given conditions
filter_score_data <- function(score_data, dims = "all", goals = "all", rgns = NA, years = NA){

  filter_scores <- score_data
  if(dims != "all"){
    filter_scores <- filter_scores %>%
      dplyr::filter(dimension %in% unlist(dims))
    if(length(filter_scores$score) == 0){
      message("filtering score data by these dimensions returns zero rows")
    }
  }
  if(goals != "all" & length(filter_scores$score) > 0){
    goals <- stringr::str_to_upper(goals) %>% unlist()
    filter_scores <- filter_scores %>%
      dplyr::filter(goal %in% goals)
    if(length(filter_scores$score) == 0){
      message("filtering score data by these goals returns zero rows")
    }
  }
  if(!is.na(rgns) & length(filter_scores$score) > 0){
    filter_scores <- filter_scores %>%
      dplyr::filter(region_id %in% unlist(rgns))
    if(length(filter_scores$score) == 0){
      message("filtering score data by these region ID values returns zero rows")
    }
  }
  if(!is.na(years) & "year" %in% names(filter_scores) & length(filter_scores$score) > 0){
    if(years == "max" | years == "latest"){
      years <- max(filter_scores$year)
    }
    filter_scores <- filter_scores %>%
      dplyr::filter(year %in% unlist(years))
    if(length(filter_scores$score) == 0){
      message("filtering score data by these years returns zero rows")
    }
  }
  if(!is.na(years) & !("year" %in% names(filter_scores))){
    print("there is no year column in the scores data table provided")
  }

  ## checking completeness
  chk1a <- identical(
    names(filter_scores),
    c("goal","dimension","region_id","score"))
  chk1b <- identical(
    names(filter_scores),
    c("goal","dimension","region_id","score","year"))

  if(chk1b){
    summary_tab <- filter_scores %>%
      group_by(goal, dimension, year)
  } else {
    summary_tab <- filter_scores %>%
      group_by(goal, dimension)
  }
  summary_tab <- summary_tab %>%
    summarize(
      missing_rgn = list(setdiff(0:42, region_id)),
      num_NAs = sum(is.na(score)),
      scores_range = list(range(score, na.rm = TRUE))
    ) %>%
    ungroup()
  chk2 <- length(unlist(summary_tab$missing_rgn)) == 0

  if(!(chk1a|chk1b)){
    message("double check columns included in score_data input")
  }
  if(!chk2){
    message("missing for some regions and/or for some goals; see 2nd output summary_tab")
  }

  return(list(filter_scores, summary_tab))
}
