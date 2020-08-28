## Libraries
source(here::here("R", "setup.R"))


## Functions

#' list functions.R goals
#'
#' @param functionsR_path location of functions.R to extract goal from
#' @param functionsR_text list of funcitons.R lines read eg using scan (if functionsR_path is not provided)
#'
#' @return goal codes character vector for goals having functions defined in functions.R

funsR_goals_list <- function(functionsR_path = NULL, functionsR_text = NULL){
  if(is.null(functionsR_path) & is.null(functionsR_text)){
    stop("must provide path to functions.R, or its contents as a character vector")
  }
  if(!is.null(functionsR_path)){
    txt <- scan(file = functionsR_path, what = "character", sep = "\n")
  } else {txt <- functionsR_text}
  breaks_str <- "^[A-Z]{2,3}\\s<-\\sfunction\\(|^[A-Z]{2,3}\\s=\\sfunction\\("
  funs_goals <- stringr::str_extract(grep(breaks_str, txt, value = TRUE), "^[A-Z]{2,3}")
  return(funs_goals)
}


#' extract from functions.R the text of a specific goal function
#'
#' @param functionsR_path location of functions.R to extract goal from
#' @param goal_code the two or three letter code indicating the goal or subgoal
#' @param functionsR_text list of funcitons.R lines read eg using scan (if functionsR_path is not provided)
#' @param comments a boolean indicating whether to include in the result commented lines of the function
#'
#' @return lines of the goal function as a character vector

goal_function <- function(functionsR_path = NULL, functionsR_text = NULL, goal_code, comments = FALSE){
  goal_code <- stringr::str_to_upper(goal_code)

  if(is.null(functionsR_path) & is.null(functionsR_text)){
    stop("must provide path to functions.R, or its contents as a character vector")
  }
  if(!is.null(functionsR_path)){
    txt <- scan(file = functionsR_path, what = "character", sep = "\n")
  } else {txt <- functionsR_text}
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

goal_layers <- function(functionsR_path, goal_code = "all"){

  goal_code <- stringr::str_to_upper(goal_code) %>% unlist()
  if(goal_code != "ALL" & any(!goal_code %in% funsR_goals_list(functionsR_path))){
    print("note: no function found for some of the given goals")
  }
  if(stringr::str_to_upper(goal_code) == "ALL"){
    txt <- scan(file = functionsR_path, what = "character", sep = "\n") %>%
      grep(pattern = "\\s*#{1,}.*", value = TRUE, invert = TRUE)
  } else {
    txt <- vector()
    for(gc in goal_code){
      txt <- c(txt, goal_function(functionsR_path, goal_code = gc, comments = FALSE))
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


#' prepared layers in bhi-prep github
#'
#' @param github_api_url github api url from which to extract list of layers at location prep/layers/filename
#'
#' @return dataframe with names of the prepared layers within bhi-prep layers folder on github

bhiprep_github_layers <- function(github_api_url = bhiprep_gh_api){
  req <- httr::GET(github_api_url)
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = FALSE) %>%
    grep(pattern = sprintf("layers/v%s/.*.csv", assess_year), value = TRUE) %>%
    as.data.frame()
  return(filelist)
}


#' revise csv file from semicolon to comma delimiters
#'
#' @param csv_filepath the filepath to the csv file to edit, including the filename
#' @param remove_na_cols boolean indicating whether to remove all columns with only NA values
#' @param overwrite boolean indicating whether to immediately overwrite the csv file with updated version
#'
#' @return if overwritten, returns head of updated file read with read_csv, else original table with NA cols removed

semicolon_to_comma <- function(csv_filepath, remove_na_cols = TRUE, overwrite = FALSE){

  ## read with semicolon delimiter
  file_semicolon <- read_delim(csv_filepath, delim =  ";")
  remove_cols <- c()

  chk <- head(file_semicolon, 15) %>%
    mutate(n_comma = str_count(as.name(names(file_semicolon)), ","))
  comma_delim <- ncol(file_semicolon)==1 & length(unique(chk$n_comma))==1

  ## if already comma delimited
  if(comma_delim){
    message(sprintf(
      "it appears %s is already comma-delimited",
      basename(csv_filepath)
    ))
    file_commas <- readr::read_csv(csv_filepath, col_types = cols())
    chk_cols <- file_commas

    ## if semicolon delimited
  } else { chk_cols <- file_semicolon }

  if(remove_na_cols){
    for(i in ncol(chk_cols)){
      column <- chk_cols[, i]
      if(nrow(column) == sum(is.na(column))){
        remove_cols <- c(remove_cols, names(column))
      }
    }
    if(length(remove_cols) > 0){
      print(
        sprintf(
          "removing columns with only NAs: %s",
          paste(remove_cols, collapse = ", ")
        )
      )
    }
  }
  file_commas <- chk_cols %>%
    dplyr::select(setdiff(names(chk_cols), remove_cols))

  if(overwrite & !(comma_delim & !remove_na_cols)){
    readr::write_csv(file_commas, csv_filepath)
    file_commas <- head(readr::read_csv(csv_filepath, col_types = cols()))
  }

  ## remove any rows that are all NAs...
  file_commas <- cbind(
    file_commas,
    chk = apply(file_commas, MARGIN = 1, FUN = function(x) !all(is.na(x)))
  )
  file_commas <- filter(file_commas, chk)


  return(file_commas)
}


#' compare columns and/or rows between two tables
#'
#' @param tab1 the table to compare to (what we hope table 2 looks like)
#' @param tab2 the table to -within some specified columns- check for missing/extra rows with respect to some 'key' variable
#' @param key_row_var the variable for groupings of interest e.g. by which you would join or maybe gather the data
#' @param check_cols the columns to look for differences within; essentially taking tab1 %>% select(check_cols) to compare tab2 against
#' @param check_for_nas character vector of column names to check for NAs
#'
#' @return a boolean indicating if differences were found, list of checks results including scan for NAs,
#' comparisons ie missing or extra unique values within key columns

compare_tables <- function(tab1, tab2, key_row_var, check_cols = "all", check_for_nas = NA){

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
    stop(sprintf(
      "given 'check' column(s) not found in one or both of the tables: %s",
      dplyr::union(not_in_tab1, not_in_tab2) %>%
        paste(collapse = ", ")
    ))
  }
  if(!(key_row_var %in% names(tab1df) & key_row_var %in% names(tab2df))){
      stop(paste("one or both tables are missing the key row variable:", key_row_var))
  }
  if(!is.na(check_for_nas)){
    chk_na <- array(
      NA,
      dim = c(nrow(tab1df), length(check_for_nas)),
      dimnames = list(tab1df[, key_row_var], check_for_nas)
    )
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
    for(j in keys %>% unlist()){ # from stops above check_cols incl. key_row_var must be in both tabs
      e[[j]] <- setdiff(
        unique(tab2df[tab2df[key_row_var] == j, i]),
        unique(tab1df[tab1df[key_row_var] == j, i])
      )
      m[[j]] <- setdiff(
        unique(tab1df[tab1df[key_row_var] == j, i]),
        unique(tab2df[tab2df[key_row_var] == j, i])
      )
    }
    comparisons$extra[[i]] <- e
    comparisons$missing[[i]] <- m
  }
  diffs_found <- ifelse(length(c(unlist(comparisons$missing), unlist(comparisons$extra))) > 0, TRUE, FALSE)
  return(
    list(
      diffs_found = diffs_found,
      checks = checks,
      chk_na = chk_na,
      comparisons = comparisons
    )
  )
}
