#' copy layers from 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder
#'
#' @param assessment_path
#' @param repo_location
#'
#' @return
#' @export
#'
#' @examples
copy_layers_for_assessment <- function(assessment_path, repo_location){

  repo_loc <- repo_location

  req <- GET("https://api.github.com/repos/OHI-Science/bhi-prep/git/trees/master?recursive=1")
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F) %>%
    stringr::str_extract_all("prep/layers/.*.csv", simplify = TRUE) %>%
    as.data.frame()

  for(i in 1:length(filelist$V1)){
    tmp <- filelist[i,1] %>% as.character()
    if(str_length(tmp) != 0){
      url <- sprintf("%s/%s", repo_loc, tmp)
      name_of_layer <- tmp %>% str_extract("[a-z0-9_]+.csv$")
      GET(url, write_disk(sprintf("%s/layers/%s", assessment_path, name_of_layer), overwrite = TRUE))
    }
  }
}



#' calculate BHI scores for each scenario year and save to a single csv file
#'
#' @param assessment_path the bhi repository 'baltic' subfolder, wherever that is on your local computer
#' @param scenario_yrs which years scores are to be calculated for
#' @param scores_path the directory where to save the scores.csv, by default in the 'baltic' (assessment) folder
#'
#' @return
#' @export
#'
#' @examples
calculate_scores <- function(assessment_path, scenario_yrs, scores_path = "."){

  currentwd <- getwd()
  setwd(assessment_path)

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
      dplyr::mutate(year = yr)}) %>% dplyr::bind_rows()

  ## write csv by default to 'assessment_path'
  readr::write_csv(scorelist,
                   sprintf("%s/scores.csv", scores_path),
                   na = "")

  setwd(currentwd)
}



#' update `alt_layers_full_table.csv` to include additional versions of layers created
#'
#' @param assessment_path
#' @param prep_path
#' @param assess_year
#'
#' @return
#' @export
#'
#' @examples
update_alt_layers_tab <- function(assessment_path, prep_path, assess_year){

  # alt_layers <- readr::read_delim(sprintf("%s/testing/alt_layers_full_table.csv", assessment_path), delim = ";")

  alt_layers <- readr::read_csv(sprintf("%s/testing/%s", assessment_path, "alt_layers_full_table.csv"))
  layer_names <- alt_layers$layer %>% unique() %>% as.list()

  ## identify new layers to add to the 'alt_layers_full_table.csv' table
  add_alt_layers <- list.files(prep_path, recursive = TRUE) %>%
    grep(pattern = sprintf(".*v%s/output/[a-z0-9_]+.csv$", assess_year), value = TRUE) %>%
    enframe(name = NULL) %>%
    dplyr::mutate(filename = basename(as.character(value)),
                  goal = stringr::str_extract(value, pattern = "[a-zA-Z]+"),
                  use = NA,
                  layer_names = list(layer_names),
                  subfolder = stringr::str_extract(value, pattern = "(\\w+ ?){1}/v2") %>%
                    stringr::str_remove("/v2")) %>%

    dplyr::rowwise() %>%
    mutate(layer = stringr::str_match(filename, as.character(layer_names)) %>%
             lapply(function(x){x[!is.na(x)]}) %>%
             unlist(),
           subfolder = ifelse(subfolder == goal, NA, subfolder)) %>%
    ungroup() %>%

    dplyr::select(layer, filename, use, goal, subfolder) %>%
    dplyr::filter(!filename %in% alt_layers$filename)

  ## rowbind new layer versions to the 'alt_layers_full_table.csv' and save updated table
  alt_layers_full_table <- alt_layers %>% rbind(add_alt_layers)
  readr::write_csv(alt_layers_full_table,
                   sprintf("%s/testing/%s", assessment_path, "alt_layers_full_table.csv"))
}
