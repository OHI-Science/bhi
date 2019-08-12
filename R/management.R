## Libraries
source(file.path(here::here(), "R", "common.R"))
library(sf)
library(rgdal)
library(tidytext)
library(dbplyr)
library(DBI)
library(odbc)
library(config)
library(httr)


## Functions

#' compile readme information associated with functions defined in a script
#'
#' written to generate readme content for functions in bhi/R scripts, but could be used elsewhere...
#'
#' @param dir_R file path to the directory containing the script of interest
#' @param script_name the name of the script with functions you want readme documentation for
#'
#' @return text for readme content is returned in the console, but output is also configured as a character vector

bhiRfun_readme <- function(dir_R, script_name){

  funs_text <- scan(file = file.path(dir_R, script_name), what = "character", sep = "\n")

  funs_names <- funs_text %>%
    grep(pattern = "^[a-z_]+.*function.*", value = TRUE) %>%
    stringr::str_extract("^\\S+") %>%
    stringr::str_pad(width = str_length(.)+4, side = "both", pad = "*")
  funs_info <- funs_text %>%
    grep(pattern = "^#'\\s", value = TRUE) %>%
    stringr::str_remove_all("#' ")
  sep <- c(0, which(stringr::str_detect(funs_info, pattern = "@return|@return.*\n\n"))) # last roxygen element assumed @return... if have anything else after...

  out <- vector()
  if(length(sep) == length(funs_names)+1 & length(sep) > 1){

    for(i in 1:length(funs_names)){
      funs_doc <- c(funs_names[i], funs_info[(sep[i]+1):(sep[i+1])], "<br/>")
      cat(funs_doc, sep = " <br/> \n")
      out <- c(out, paste0(funs_doc, sep = " <br/>"))
    }
    return(out)

  } else { print("cannot parse... check script for missing roxygen documentation") }
}


#' generate basic readme outline
#'
#' look within a folder and create structure dependent on content and file tree
#' if has subfolders...
#' result not to be the end all be all, just a starting point or rough outline to start from
#' will still have to actually open and manually edit some fields, but readme_content function will help with that
#' describe_objects could be: file, table, folder, function, script
#'
#' could use `sink` function to write readme outline output directly to a specified readme file
#'
#' @param folder_filepath file path to folder where README will be located and which contains objects to document
#' @param type_objects character string with type of thing to document in the readme: folders, functions, files, tables, or scripts
#'
#' @return text for readme outline is printed to the console, and can be copied from there or sunk to a file

readme_outline <- function(folder_filepath, type_objects = "files", delim = ","){

  ## setup and extract file tree
  S <- .Platform$file.sep
  title <- basename(folder_filepath)

  tree <- list.files(folder_filepath, recursive = TRUE, full.names = TRUE) %>%
    gsub(pattern = folder_filepath, replacement = "")
  tree <- tree[tree != paste0(S, "README.md")]

  obj_info <- list()
  subtitles <- NULL
  general <- paste0(c("Created on:",
                      "Last modified on:",
                      "Used by or referenced in functions/scripts: <br/>\n\n"),
                    collapse = " <br/>\n") ## can change and include more/different fields later...

  ## if we want subfolders listed (with summary descriptions for each)
  if(type_objects == "folders"){
    obj_names <- tree[stringr::str_count(tree, pattern = S) >= 2] %>%
      stringr::str_extract(paste0("^", S, "[a-z0-9_]+", S)) %>%
      gsub(pattern = S, replacement = "") %>%
      unique() # non-empty immediately-adjacent subdirectories
    for(n in obj_names){
      obj_info[[n]] <- "add short description here"
    }
  }
  ## if we want all functions within a set of R scripts listed (with summary of what each does)
  if(type_objects == "functions"){
    subtitles <- tree[stringr::str_detect(tree, pattern = "\\.R")] %>%
      gsub(pattern = S, replacement = "")
    general <- vector() # for functions get rid of 'general' fields...
    for(s in subtitles){
      obj_names <- c(scan(file = file.path(folder_filepath, s),
                          what = "character", sep = "\n") %>%
                       grep(pattern = "^[a-z_]+.*function.*", value = TRUE) %>%
                       stringr::str_extract("^\\S+"))
      obj_info[[s]][["obj_names"]] <- list(obj_names)
    }
  }
  ## if we want each file listed (with summary of purpose and description of its contents)
  if(type_objects %in% c("files", "tables", "scripts")){
    tree <- tree[stringr::str_count(tree, pattern = S) < 2] # eliminate files in subdirectories
    obj_names <- basename(tree)[basename(tree) != "README.md"] %>%
      gsub(pattern = S, replacement = "")
    for(n in obj_names){
      obj_info[[n]] <- "add a short description here"
    }

    if(type_objects == "tables"){
      obj_names <- obj_names[grep(".csv$|.shp$", obj_names)]
      obj_info <- list() # ignore bit above about including description in obj_info...

      for(j in obj_names){
        if(stringr::str_detect(j, "\\.csv$")){ # for csv files
          if(delim == ","){
            tmp <- read.csv(file.path(folder_filepath, j), stringsAsFactors = FALSE)
          } else {
            tmp <- readr::read_delim(file.path(folder_filepath, j), delim = delim) %>%
              as.data.frame()
          }
        }
        if(stringr::str_detect(j, "\\.shp$")){ # for shapefiles; requires sf package
          tmp <- sf::st_read(folder_filepath, substr(j, 1, nchar(j)-4))
          st_geometry(tmp) <- NULL # get just table; coerces to dataframe
        }
        nms <- names(tmp) # names of columns in table
        cla <- vector() # classes of each attribute or column
        lvl <- vector() # levels or categories (or a place for descriptions)
        for(k in 1:ncol(tmp)){
          lvls <- ifelse(length(unique(tmp[, k])) < 10 & # these won't work if tmp is a tibble
                                 all(str_length(unique(tmp[, 1])) < 10),
                         paste(unique(tmp[, k]), collapse = ", "), "")
          cla <- c(cla, paste0("* ", nms[k], ": ", class(tmp[, k])))
          lvl <- c(lvl, paste0("* ", nms[k], ": ", paste(lvls, collapse = ", ")))
        }
        obj_info[[j]][["class"]] <- cla
        obj_info[[j]][["levels"]] <- lvl
      }
    }
  }
  ## put everything in order and print out
  t <- paste0("# `", title, "`") # title
  cat(t, "\n\n<br/>\n\n")
  out <- vector()
  for(i in names(obj_info)){
    line <- unlist(obj_info[[i]])
    if(i %in% subtitles){
      subt <- paste0("### ",
                    i %>% stringr::str_pad(width = str_length(.) + 2,
                                            side = "both", pad = "`"))
      line <- line %>% stringr::str_pad(width = str_length(.) + 4, side = "both", pad = "*")
    } else {
      subt <- paste0(i %>% stringr::str_pad(width = str_length(.) + 2,
                                    side = "both", pad = "`") %>%
                       stringr::str_pad(width = str_length(.) + 4,
                                        side = "both", pad = "*"))
    }
    cat(subt, "\n\n")
    cat(general)
    if(type_objects == "tables"){
      cat(paste0(line, collapse = " <br/>\n"),
          ifelse(length(line) == 0, "\n", " <br/>\n\n<br/>\n\n"))
    } else {
      cat(paste0(line, collapse = " <br/>\n\n<br/>\n\n"),
          ifelse(length(line) == 0, "<br/>\n\n", "<br/>\n\n<br/>\n\n"))
    }
    out <- c(out, subt, general, line)
  }
  out <- c(t, out)
  return(out)
}


#' from readme markdown file to dataframe
#'
#' @param folder_filepath file path to folder where README will be located and which contains objects to document
#' @param write a boolean variable indicating whether to write the dataframe to csv file in the folder_filepath location
#'
#' @return dataframe created from a readme markdown file with structure outlined by `readme_outline` function above

readme_to_df <- function(folder_filepath, write_df = FALSE){

  ## to parse look for structure (heading styles) as created by readme_outline function...
  mdtext <- scan(file = file.path(folder_filepath, "README.md"),
                 what = "character", sep = "\n") %>%
    grep(pattern = "^<br/>$", value = TRUE, invert = TRUE) %>%
    stringr::str_remove_all(" <br/>") # Corpus would be better for string processing maybe...

  h2 <- mdtext %>% grep(pattern = "^###") # subtitles start with three hashtags
  h3 <- mdtext %>% grep(pattern = "^\\*\\*") # object names start with two asterisks
  h4 <- mdtext %>% grep(pattern = "^\\*\\s") # another sublevel from documented csvs etc

  ## create a row/tuple for each object-descriptive element pair
  nrws <- ifelse(length(h4) > 0, length(h4)/2, length(h3)) # 2 additional characteristics for csvs
  info <- array(NA, dim = c(nrws, 4)) # 4 cols max: "object", "description", "classes", "attributes"
  indx <- c(h3, length(mdtext)+1)
  recorded_in <- vector()
  r0 <- 0

  ## ooph yikes lots of indexing
  ## would probably be better to use list-columns...
  for(i in 1:length(h3)){ # just have descriptions for objects
    r <- i
    dim_nms <- c("object", "description")
    tmp_raw <- mdtext[(indx[i]+1):(indx[i+1]-1)]
    tmp_edt <- paste(tmp_raw, collapse = " \n ")

    if(length(h4) > 0){ # have additional subcategories: attributes and classes
      dim_nms <- c("object", "description", "classes", "attributes")
      tmp_edt <- tmp_raw %>%
        grep(pattern = "^\\*", value = TRUE, invert = TRUE) %>%
        paste(collapse = " \n ")
      tmp_edt1 <- tmp_raw %>%
        grep(pattern = "^\\*", value = TRUE) %>%
        gsub(pattern = "\\* ", replacement = "")
      r <- 1:(length(tmp_edt1)/2)+r0
      r0 <- max(r)
      info[r, 3:length(dim_nms)] <- array(tmp_edt1, dim = c(length(tmp_edt1)/2, 2))
    }
    info[r, 1] <- stringr::str_extract(mdtext[h3[i]], pattern = "[A-Za-z0-9_\\.]+")
    info[r, 2] <- tmp_edt

    if(length(h2) > 0){
      recorded_in <- c(recorded_in, gsub("### ", "", mdtext[h2][which.min(h3[i] > h2) -1]))
    }
  }
  readme_df <- data.frame(info, stringsAsFactors = FALSE)
  readme_df <- readme_df[, unlist(lapply(readme_df, function(x) !all(is.na(x))))]
  colnames(readme_df) <- dim_nms

  if(length(h2) > 0){
    readme_df <- readme_df %>% cbind(recorded_in)
  }
  if(write_df){
    ti <- gsub("[^a-z]", "", mdtext[1])
    readr::write_csv(readme_df,
                     file.path(folder_filepath, sprintf("%s_readme_metadata.csv", ti)))
  }
  return(readme_df)
}


#' check up-to-date-ness status of readme
#'
#' check whether readme actually reflects the current state of the directory and files it is written for
#' compare lines within readme and outline that would be generated via the `readme_outline` function above
#' the comparison is done via the `readme_to_csv` function, also above...
#'
#' @param folder_filepath file path to folder where README will be located and which contains objects to document
#' @param type_objects character string with type of thing to document in the readme: folders, functions, files, tables, or scripts
#' @param temp_filepath file path to subfolder of assessment folder called 'temp' where ephemeral, non-critical files are temporarily put
#'
#' @return no returned value, just printed messages about status of readme in comparison to expected structure

readme_status <- function(folder_filepath, type_objects, temp_filepath){

  ## current readme
  readme <- readme_to_df(folder_filepath) # depends on readme_to_df function

  ## generate expected readme
  sink(file = file.path(temp_filepath, "README.md"))
  outline <- readme_outline(folder_filepath, type_objects) # depends on readme_outline function
  closeAllConnections() # stop the sinking!
  readme_new_struc <- readme_to_df(temp_filepath)

  ## using compare_tabs function defined in common.R
  if(type_objects == "tables"){
    readme <- readme %>% dplyr::mutate(
      attributes = stringr::str_extract_all(
        attributes, pattern = "^[a-z]+"))
    readme_new_struc <- readme_new_struc %>% dplyr::mutate(
      attributes = stringr::str_extract_all(
        attributes, pattern = "^[a-z]+"))
    check_cols = c("object", "classes", "attributes")
  }
  check_cols <- c("object", "classes")
  comp <- compare_tabs(tab1 = readme_new_struc, tab2 = readme,
                       key_row_var = "object", check_cols = check_cols,
                       check_for_nas = "description")
  checks <- comp[[2]]
  chk_na <- comp[[3]]
  comparisons <- comp[[4]]

  if(checks$chk_nrow){
    print("different numbers of rows in current readme and expected readme")
  }
  if(length(checks$chk_missing_key) != 0){
    stop(sprintf("objects missing from current readme: %s",
                 paste(checks$chk_missing_key, collapse = ", ")))
  }
  if(length(checks$chk_extra_key) != 0){
    stop(sprintf("unexpected objects found in current readme: %s",
                 paste(checks$chk_extra_key, collapse = ", ")))
  }
  if(any(chk_na[, "description"])){
    sprintf("some NAs found in place of descriptions for objects: %s",
            grep(TRUE, chk_na[, "description"], value = TRUE) %>%
              names() %>% paste(collapse = ", "))
  }

  ## if documented objects are tables, have an additional column (attributes) to compare
  if(type_objects == "tables"){
    if(length(unlist(comparisons$missing)) != 0){
      for(i in comparisons$missing){
        sprintf("object '%s' is missing attributes: %s",
                paste(unlist(missing[i]), collapse = "\n "),
                names(comparisons$missing[i]))
      }
    }
    if(length(unlist(comparisons$extra)) != 0){
      for(i in comparisons$extra){
        sprintf("object '%s' is missing attributes: %s",
                paste(unlist(missing[i]), collapse = "\n "),
                names(comparisons$missing[i]))
      }
    }
  }
}


#' confirm layers in layers.csv have corresponding entries in layers_metadata.csv
#'
#' @param layers_csv layers.csv dataframe
#' @param lyr_metadata layers_metadata.csv dataframe
#'
#' @return

layerscsv_metadata_exists <- function(layers_csv, lyr_metadata){

  compare_tabs(tab1 = layers_csv, tab2 = lyr_metadata, key_row_var = "layer")

  chk <- "a boolean, true if correct matching exists"
  return(chk)
}


#' update links to bhi-prep docs in goals.Rmd
#'
#' @param dir project root of bhi-prep
#' @param version_year the assessment year with a preceeding "v", specified as a string
#'
#' @return no immediate output; effect of the function is updated links to prep files in supplement/web/goals.Rmd

update_goalsRmd_links <- function(dir, version_year){

  if(basename(dir) == "prep"){dir <- dirname(dir)}
  goalsRmd_path <- file.path(here::here(), "supplement", "web", "goals.Rmd")

  txt_updated <- readLines(goalsRmd_path)
  replace <- grep(pattern = "https://github.com/OHI-Science/.*/prep/.*", x = txt_updated)

  for(r in replace){

    w <- str_replace(
      str_extract(txt_updated[r], pattern = "/prep/.*.md"),
      pattern = ifelse(str_detect(txt_updated[r], pattern = "v[0-9]{4}"),
                       str_extract(txt_updated[r], pattern = "v[0-9]{4}/.*.md"),
                       basename(str_extract(txt_updated[r], pattern = "/prep/.*.md"))),
      replacement = paste(
        version_year,
        basename(str_extract(txt_updated[r], pattern = "/prep/.*.md")),
        sep = "/")
    )

    txt_updated[r] <- stringr::str_replace(
      txt_updated[r],
      "https://github.com/OHI-Science/.*/prep/.*",
      sprintf("https://github.com/OHI-Science/%s/blob/master%s.rmd)  ",
              basename(dir), tools::file_path_sans_ext(w))
    )
  }

  writeLines(txt_updated, goalsRmd_path)
}


#' fill in roxygen param documentation from a common params_index
#'
#' the objective of this function is to facilitate consistent use of parameter names throughout the BHI repositories
#' relevant roxygen documentation can be automatically filled in using this function and the supplemental 'params_index.md' document
#'
#' @param script_name the name of the script with functions you want to create documentation for
#' @param params_index a character string giving the file path to this document
#' @param script_function specific functions to create documentation for, if not for entire script
#'

fill_oxygendoc_param <- function(script_name, params_index = NA, script_function = NA){

  if(is.na(params_index)){
    param_txt <- readLines(file.path(here::here(), "supplement", "documents", "params_index.md")) %>%
      grep(pattern = "\\*.*", value = TRUE)
  } else {
    if(file.exists(params_index)){
      param_txt <- readLines(params_index) %>%
        grep(pattern = "\\*.*", value = TRUE)
    } else {
      if(!class(params_index) == "list"){
        stop("cannot interpret params_index input, must be filepath to document or character list")
      }
      param_txt <- params_index
    }
  }
  fun_path <- file.path(here::here(), "R", script_name)
  script_txt <- readLines(fun_path)

  ## identify lines containting the function of interest
  ## and of those lines, which contain its parameter roxygen docuemntation
  breaks_param <- "#'\\s@param\\s[A-Za-z0-9_]+$"
  params <- grep(pattern = breaks_param, x = script_txt)

  if(!is.na(script_function)){
    start_fun <- grep(pattern = sprintf("^%s\\s<-\\sfunction\\(|^%s\\s=\\sfunction\\(",
                                        script_function, script_function), x = script_txt)
    if(length(start_fun) == 0){
      stop(sprintf("given function '%s' not contained in '%s' script", script_function, script_name))
    }
    breaks_fun <- c(0, grep(pattern = "^[A-Za-z0-9_]+\\s<-\\sfunction\\(|^[A-Za-z0-9_]+\\s=\\sfunction\\(", x = script_txt))
    count <- grep(x = breaks_fun, pattern = start_fun)
    params <- params[params %in% breaks_fun[count-1]:breaks_fun[count]]
  }

  ## match parameter documentation
  ## and replace rows of function within script with the updated info
  script_txt_updated <- script_txt
  for(p in params){
    param_match <- paste0("^\\*{2}", str_extract(script_txt[p], pattern = "[A-Za-z0-9_]+$"))
    replace_formated <- grep(pattern = param_match, x = param_txt, value = TRUE) %>%
      str_replace_all(pattern = "^\\*{2}", replacement = "#' @param ") %>%
      str_replace_all(pattern = "\\*{2}\\s--", replacement = "")

    script_txt_updated[p] <- ifelse(
      length(replace_formated) == 0,
      script_txt[p],
      replace_formated
    )
    if(length(replace_formated) == 0){
      message(sprintf("no parameter definition found for '%s' in given param_index",
                      str_extract(script_txt[p], pattern = "[A-Za-z0-9_]+$")))
    }
  }

  ## write the updated script in the original location
  script_name_updated <- paste0(str_extract(script_name, pattern = "[A-Za-z0-9]+"), "_docupdate.R")
  writeLines(script_txt_updated, file.path(here::here(), "R", script_name_updated))
}

