## Libraries
library(tidyverse) # install.packages("tidyverse")
library(sf) # install.packages("sf")

## Functions

#' compile readme information associated with functions defined in a script
#'
#' written to generate readme content for functions in bhi/R scripts, but could be used elsewhere...
#'
#' @param bhiR_dir file path to the directory containing the script of interest
#' @param script_name the name of the script with functions you want readme documentation for
#'
#' @return text for readme content is returned in the console, but output is also configured as a character vector

bhiRfun_readme <- function(bhiR_dir, script_name){

  funs_text <- scan(file = file.path(bhiR_dir, script_name), what = "character", sep = "\n")

  funs_names <- funs_text %>%
    grep(pattern = "^[a-z_]+.*function.*\\{", value = TRUE) %>%
    stringr::str_extract("^\\S+") %>%
    stringr::str_pad(width = str_length(.)+4, side = "both", pad = "*")
  funs_info <- funs_text %>%
    grep(pattern = "^#'\\s", value = TRUE) %>%
    stringr::str_remove_all("#' ")
  sep <- c(0, which(stringr::str_detect(funs_info, pattern = "@return"))) # last roxygen element assumed @return... if have anything else after...

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


#' function for creating metadata/readme content
#'
#' @param folder_filepath file path to folder where README will be located and which contains objects to document
#' @param file name of the file to generate basic content for
#' @param file_type
#'
#' @return

readme_content <- function(folder_filepath, file, file_type){
  ## look at a file and create some information depending on file type...
  ## eg if csv tell me about the rows and columns
  ## if a spatial file tell me what kind, spatial extent, something about attribute table if has one...
  ## maybe if vector spatial file use sf package and tell me about the attrib table as if were csv
  ## with raster can use metadata() function
  ## don't want too much in readme, just enough to know if its what Im looking for!
  ## documentation maybe to be used later in communication with the SQL database, so write a flexible function with that in mind...

  # if(file_type %in% c("raster", "tiff", "geotiff")){
  # }
  # if(file_type %in% c("csv", "shapefile")){
  # }
  # if(file_type %in% c("image", "picture", "png", "jpg")){
  # } else {
  #   print("sadly, this function isn't able to tell you anything about that kind of file")
  #   meaningful_stuff <- NULL
  #   other_info_lumped <- NULL
  # }
  # return(list(meaningful_stuff, other_info_lumped))
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

readme_outline <- function(folder_filepath, type_objects = "files"){

  ## setup and extract file tree
  S <- .Platform$file.sep
  title <- basename(folder_filepath)
  print(sprintf("generating README outline designed to document object type '%s' in '%s' directory", type_objects, title))

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
      stringr::str_extract(paste0("^", S, "[a-z0-9]+", S)) %>%
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
                       grep(pattern = "^[a-z_]+.*function.*\\{", value = TRUE) %>%
                       stringr::str_extract("^\\S+"))
      obj_info[[s]][["obj_names"]] <- list(obj_names)
    }
  }
  ## if we want each file listed (with summary of purpose and description of its contents)
  if(type_objects %in% c("files", "tables", "scripts")){
    obj_names <- basename(tree)[basename(tree) != "README.md"] %>%
      gsub(pattern = S, replacement = "")
    for(n in obj_names){
      obj_info[[n]] <- "add a short description here"
    }

    if(type_objects == "tables"){
      obj_names <- obj_names[grep(".csv$|.shp$", obj_names)]
      obj_info <- list() # ignore bit above about including description...

      for(j in obj_names){
        if(stringr::str_detect(j, "\\.csv$")){ # for csv files
          tmp <- read.csv(file.path(folder_filepath, j), stringsAsFactors = FALSE)
        }
        if(stringr::str_detect(j, "\\.shp$")){ # for shapefiles; requires sf package
          tmp <- sf::st_read(folder_filepath, substr(j, 1, nchar(j)-4))
          st_geometry(tmp) <- NULL # get just table; coerces to dataframe
        }
        nms <- names(tmp) # names of columns in table
        cla <- vector() # classes of each attribute or column
        lvl <- vector() # levels or categories (or a place for descriptions)
        for(k in 1:ncol(tmp)){
          lvls <- ifelse(length(unique(tmp[, k])) < 10 &
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

readme_to_df <- function(folder_filepath, write = FALSE){

  ## to parse look for structure (heading styles) as created by readme_outline function...
  mdtext <- scan(file = file.path(folder_filepath, "README.md"),
                 what = "character", sep = "\n") %>%
    grep(pattern = "^<br/>$", value = TRUE, invert = TRUE) %>%
    stringr::str_remove_all(" <br/>") # Corpus would be better for string processing...

  h2 <- mdtext %>% grep(pattern = "^###") # subtitles start with three hashtags
  h3 <- mdtext %>% grep(pattern = "^\\*\\*") # object names start with two asterisks
  h4 <- mdtext %>% grep(pattern = "^\\*\\s") # another sublevel from documented csvs etc

  ## create a row/tuple for each object-descriptive element pair
  nrws <- ifelse(length(h4) > 0, length(h4)/2, length(h3)) # 2 additional characteristics for csvs
  info <- array(NA, dim = c(nrws, 5))
  indx <- c(h3, length(mdtext)+1)
  recorded_in <- vector()

  ## ooph yikes lots of indexing
  ## would probably be better to use list-columns...
  for(i in 1:length(h3)){ # just have descriptions for objects
    r <- i
    dim_nms <- c("object", "description")
    tmp_raw <- mdtext[(indx[i]+1):(indx[i+1]-1)]
    tmp_edt <- paste(tmp_raw, collapse = " \n ")

    if(length(h4) > 0){ # have additional subcategories: attributes and classes
      r <- ((2*i)-1):(2*i)
      dim_nms <- c("object", "description", "classes", "attributes")
      tmp_edt <- tmp_raw %>%
        grep(pattern = "^\\*", value = TRUE, invert = TRUE) %>%
        paste(collapse = " \n ")
      tmp_edt1 <- tmp_raw %>%
        grep(pattern = "^\\*", value = TRUE) %>%
        gsub(pattern = "\\* ", replacement = "")

      info[r, 3:length(dim_nms)] <- array(tmp_edt1, dim = c(2, 2))
    }
    info[r, 1] <- stringr::str_extract(mdtext[h3[i]], pattern = "[A-Za-z0-9_\\.]+")
    info[r, 2] <- tmp_edt

    if(length(h2) > 0){
      recorded_in <- c(recorded_in, gsub("### ", "", mdtext[h2][which.min(h3[i] > h2) -1]))
    }
  }
  readme_df <- data.frame(info)
  readme_df <- readme_df[, unlist(lapply(readme_df, function(x) !all(is.na(x))))]
  colnames(readme_df) <- dim_nms

  if(length(h2) > 0){
    readme_df <- readme_df %>% cbind(recorded_in)
  }
  if(write){
    title <- gsub("[^a-z]", "", mdtext[1])
    readr::write_csv(readme_df,
                     file.path(folder_filepath,sprintf("%s_readme_metadata.csv", title)))
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

  ## expected readme
  sink(file = file.path(temp_filepath, "README.md"))
  outline <- readme_outline(folder_filepath, type_objects) # depends on readme_outline function
  closeAllConnections() # stop the sinking!
  readme_new_struc <- readme_to_df(temp_filepath)

  ## checks and messages
  chk_nrows <- nrow(readme_new_struc) != nrow(readme) # do they have same number of rows
  chk_missing_obj <- setdiff(unique(readme_new_struc$object), unique(readme$object))
  chk_extra_obj <- setdiff(unique(readme$object), unique(readme_new_struc$object))
  chk_na_descrip <- is.na(readme$description) # are there any NAs in place of description

  if(chk_nrows){
    print("different numbers of rows in current readme and expected readme")
  }
  if(length(chk_missing_obj) != 0){
    stop(sprintf("objects missing from current readme: %s",
                 paste(chk_missing_obj, collapse = ", ")))
  }
  if(length(chk_extra_obj) != 0){
    stop(sprintf("unexpected objects found in current readme: %s",
                 paste(chk_extra_obj, collapse = ", ")))
  }
  if(any(chk_na_descrip)){
    sprintf("there are NAs in place of descriptions for objects: %s",
            paste(readme$object[chk_na_descrip], collapse = ", "))
  }
  ## disproportionate amout of space for checking objects with class and attribute information...
  if(type_objects == "tables"){
    for(i in unique(readme$object)){
      readme_classes <- readme %>% filter(object == i)
      readme_new_classes <- readme_new_struc %>% filter(object == i)

      chk_missing_classes <- setdiff(
        unique(readme_new_classes$classes),
        unique(readme_classes$classes))
      chk_extra_classes <- setdiff(
        unique(readme_classes$classes),
        unique(readme_new_classes$classes))

      if(length(chk_missing_classes) != 0){
        sprintf("object '%s' is missing classes: %s", i,
                paste(chk_missing_classes, collapse = ", "))
      }
      if(length(chk_extra_classes) != 0){
        sprintf("object '%s' has unexpected classes: %s", i,
                paste(chk_extra_classes, collapse = ", "))
      }

      readme_attrib <- readme %>% filter(object == i) %>%
        dplyr::mutate(attributes = stringr::str_extract_all(attributes, pattern = "^[a-z]+"))
      readme_new_attrib <- readme_new_struc %>% filter(object == i) %>%
        dplyr::mutate(attributes = stringr::str_extract_all(attributes, pattern = "^[a-z]+"))

      chk_missing_attrib <- setdiff(
        unique(readme_attrib$attributes),
        unique(readme_new_attrib$attributes))
      chk_extra_attrib <- setdiff(
        unique(readme_new_attrib$attributes),
        unique(readme_attrib$attributes))

      if(length(chk_missing_attrib) != 0){
        sprintf("object '%s' is missing classes: %s", i,
                paste(chk_missing_attrib, collapse = ", "))
      }
      if(length(chk_extra_attrib) != 0){
        sprintf("object '%s' has unexpected classes: %s", i,
                paste(chk_extra_attrib, collapse = ", "))
      }
    }
  }
}
