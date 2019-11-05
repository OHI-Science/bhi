## Libraries
source(file.path(here::here(), "R", "setup.R"))
library(ohicore)


## Functions

#' calculate BHI scores for each scenario year and save to a single csv file
#'
#' @param dir_assess filepath to the current assessment directory, an immediate subdirectory of the project root folder; set in `setup.R`
#' @param scenario_yrs which years scores are to be calculated for
#' @param scores_path the directory where to save the scores.csv, by default in the assessment (dir_assess) folder
#'
#' @return OHI scores

calculate_scores <- function(dir_assess, scenario_yrs, scores_path = ".", write_scores = TRUE){

  currentwd <- getwd()
  setwd(dir_assess)

  ## usually use excel to edit these files, need to make sure for ohicore funcs etc they are comma delim.
  lapply(
    list(
      file.path(dir_assess, "layers.csv"),
      file.path(dir_assess, "layers_metadata.csv"),
      file.path(dir_assess, "conf", "scenario_data_years.csv")
    ),
    function(x){semicolon_to_comma(x, remove_na_cols = TRUE, overwrite = TRUE)}
  )

  ## load scenario configuration
  conf <- ohicore::Conf("conf")

  ## check that scenario layers files in the \layers folder match layers.csv registration
  ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)

  ## load scenario layers for ohicore to access
  layers <- ohicore::Layers("layers.csv", "layers")

  scorelist <- lapply(scenario_yrs, function(yr){
    print(sprintf("For assessment year %s", yr))
    layers$data$scenario_year <- yr
    scores_scenario_year <- ohicore::CalculateAll(conf, layers) %>%
      dplyr::mutate(year = yr)
    }) %>%
    dplyr::bind_rows()

  ## write csv by default to 'dir_assess'
  if(is.TRUE(write_scores)){
    readr::write_csv(
      scorelist,
      sprintf("%s/scores.csv", scores_path),
      na = ""
    )
  }

  setwd(currentwd)
  return(scorelist)
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
    file_commas <- read_csv(csv_filepath, col_types = cols())
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
    select(setdiff(names(chk_cols), remove_cols))

  if(overwrite & !(comma_delim & !remove_na_cols)){
    write_csv(file_commas, csv_filepath)
    file_commas <- read_csv(csv_filepath, col_types = cols()) %>% head()
  }

  return(file_commas)
}
