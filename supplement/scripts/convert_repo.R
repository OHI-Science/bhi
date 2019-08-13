#' single to sequential multiyear repo
#'
#' converts single-year assessment repo for sequential multiyear assessments
#' i.e. to update framework for a second Baltic Sea assessment
#'
#' @param new_repo filepath to a standard starter/template repo, as created by OHI team;
#' this will be come the new expanded repo
#' @param archive_filepath filepath to the original/archived repository
#' @param original_funs_dir directory of original goal-functions as individual scripts
#' @param new_funs_dir directory of new goal-functions (able to accomodate multiple years;
#' using e.g. ohicore::AlignDataYears rather than SelectLayersData) as individual scripts
#' @param scenario_yrs the years for which 'scenarios' are to be calculated;
#' includes typically the assessment year and any back-calculated years
#' @param dummy_data_yr a 'placeholder' to include in the raw input while in order to test conversion;
#' to be replaced with real years during subsequent data prep and assessment!
#'
#' @return list of outputs including: updated_layers, track_layers_added table, intermediate pressure and resilience matrices

convert_repo <- function(new_repo, archive_filepath,
                         original_funs_dir, new_funs_dir,
                         scenario_yrs = 2015:2018, dummy_data_yr = 2014){

  ## SET UP / PREAMBLE ----
  setwd(archive_filepath)
  archive_layers <- ohicore::Layers("layers.csv", "layers") # layers object created from archive
  archive_layerscsv <- read_csv(file.path(archive_filepath, "layers.csv"), col_types = cols())
  previous_scores <- read_csv(file.path(archive_filepath, "scores.csv")) %>% mutate(year = 2014)
  setwd(new_repo)

  goal_code_list_archive <- funsR_goals_list(file.path(archive_filepath, "conf", "functions.R")) %>% str_to_lower()
  goal_code_list_new <- funsR_goals_list(file.path(new_repo, "conf", "functions.R")) %>% str_to_lower()


  ## WORK GOAL BY GOAL ----
  for(consider_goal in goal_code_list_archive){ # consider_goal = goal_code_list_archive[10]

    cat(paste("\n\nNOW CONVERTING GOAL: ", str_to_upper(consider_goal), "\n\n"))

    ## CREATE INTERM PRESSURE AND RESILIENCE MATRICES ----
    ## want to replace pressure and resilience weightings of layers for goal being transferred
    ## need all pressure layers in pressure_matrix.csv to be in layers.csv and layers folder

    ## pressure and resilience matrices
    goal_press <- read_csv(file.path(archive_filepath, "conf", "pressures_matrix.csv"),
                           col_types = cols()) %>%
      filter(goal == str_to_upper(consider_goal)) %>%
      select(-element, -element_name) # View(goal_press)

    goal_res <- read_csv(file.path(archive_filepath, "conf", "resilience_matrix.csv"),
                         col_types = cols()) %>%
      filter(goal == str_to_upper(consider_goal)) %>%
      select(-element) # View(goal_res)

    ## pressure and resilience layers to copy over and add to layers.csv
    layers_most_recent <- read_csv(file.path(new_repo, "layers.csv"), col_types = cols())
    press_res_lyrs <- c(
      goal_press %>%
        select_if(!is.na(.)) %>%
        select(-goal) %>%
        names(),
      goal_res %>%
        select_if(!is.na(.)) %>%
        select(-goal) %>%
        names())

    cat("pressure and resilience layers associated with this goal:\n",
        paste(press_res_lyrs, collapse = "\n"),
        sep = "")

    ## create the 'interm' pressure + resilience matrices
    ## fuse the rows/cols for 'consider_goal' from archive matrices to all other goals' rows of new repo matrices
    ## remove columns where all rows are NA ie deprecated/unused pressure or resilience layers

    int_pressure_mat <- read_csv(file.path(new_repo, "conf", "pressures_matrix.csv"),
                                 col_types = cols()) %>%
      filter(goal != str_to_upper(consider_goal)) %>%
      full_join(goal_press, by = c("goal", intersect(names(goal_press), colnames(.)))) %>%
      select_if(function(x) {!all(is.na(x))})

    int_resilience_mat <- read_csv(
      file.path(new_repo, "conf", "resilience_matrix.csv"),
      col_types = cols(.default = "c")) %>%
      filter(goal != str_to_upper(consider_goal)) %>%
      full_join(goal_res, by = c("goal", intersect(names(goal_res), colnames(.)))) %>%
      select_if(function(x) {!all(is.na(x))})


    ## INITIAL INVENTORY OF LAYERS TO REVIEW ----
    add_press_res_lyrs <- setdiff(
      press_res_lyrs,
      layers_most_recent$layer)

    cat("pressure and resilience layers needing to be added: \n",
        ifelse(length(add_press_res_lyrs) == 0, "none\n", paste(add_press_res_lyrs, collapse = "\n")),
        sep = "")

    ## get names of data layers for goal status from layers object created from the archive version
    goal_specific_layers <- names(archive_layers$data) %>%
      grep(pattern = paste0(consider_goal, "_.*"),
           value = TRUE) %>%
      c(add_press_res_lyrs) # include pressure and resilience layers

    ## initial inventory of layers to remove from or add to scenario_data_years table
    conf <- ohicore::Conf("conf")
    current_conf_scen_data_yrs <- conf$scenario_data_years


    ## WORK LAYER BY LAYER ----
    for(consider_lyr_nm in goal_specific_layers){ # consider_lyr_nm = goal_specific_layers[3]

      cat(sprintf("incorporating and/or reviewing '%s' layer for '%s' goal\n\n", consider_lyr_nm, consider_goal))

      ## REPLACE WITH ARCHIVED LAYER VERSION ----
      ## check filename (fn) to use, and whether it already exists in repo
      ## view archived layer, if needs a year column add it and save to layers folder, otherwise just copy over

      fn <- str_match(archive_layerscsv$filename, paste0(consider_lyr_nm, ".*.csv")) %>%
        lapply(function(x){x[!is.na(x)]}) %>%
        unlist()

      if(length(fn) >= 1){
        if(length(fn) > 1){
          message("more than one layer found in archive, using the first one found")
          fn <- fn[1]
        } else {
          cat(sprintf("filename for '%s' layer found in archive layers folder: %s\n",
                      consider_lyr_nm, fn))
        }
        layer_archive_version <- read_csv(file.path(archive_filepath, "layers", fn),
                                          col_types = cols())
        # layer_archive_version <- archive_layers$data[[consider_lyr_nm]]
      } else {stop(sprintf("no apparent matching layer found in archive for '%s'", consider_lyr_nm)) }


      ## IF NO YEAR COLUMN IN LAYER...
      ## adding dummy year cols only for purpose of transitioning repo!
      ## especially for trend, will be more carefully reviewed when doing actual data prep!
      ## remember to review how years are actually included when doing actual data prep!

      if(!"year" %in% names(layer_archive_version)){
        yr_col_added <- layer_archive_version %>%
          mutate(year = dummy_data_yr) # temporary for setting up repo!
        lyr_w_yr <- yr_col_added
        message("no year column; adding dummy year col only for purpose of transitioning repo, review during data prep!")
        y <- dummy_data_yr
      } else {
        lyr_w_yr <- layer_archive_version
        ## parse layer data to see what years should go into scenario_data_years table
        y <- layer_archive_version$year %>% unique() %>% sort() # years we can have in scenario_data_years.csv as data years
        cat("years found in layer:\n", paste(c(y, "\n"), collapse = "\n"), sep = "")
      }

      if(!file.exists(file.path(new_repo, "layers", fn))){
        write_csv(lyr_w_yr, file.path(new_repo, "layers", fn))
      } else {
        cat(sprintf("'%s' already exists in the new repo 'layers' folder; not replaced \n", fn))
      }

      ## UPDATE LAYERS.CSV ----
      ## layerscsv_edit function defined in reconfiguring.R, can run line-by-line there for troubleshooting

      updated_layers <- layerscsv_edit(
        layers_object = ohicore::Layers("layers.csv", "layers"),
        lyr_file = file.path(new_repo, "layers", fn),
        lyr_name = consider_lyr_nm,
        assessment_path = new_repo,
        metadata_path = file.path(new_repo, "layers_metadata.csv"),
        update_with = archive_layerscsv,
        write = FALSE
      ) # View(updated_layers[[1]])
      write_csv(updated_layers[[1]], file.path(new_repo, "layers.csv"))

      track_layers_entry <- data.frame(
        layer = consider_lyr_nm,
        filename = fn,
        day_added = Sys.Date(),
        dummy_data_yr = ifelse("year" %in% names(layer_archive_version), FALSE, TRUE),
        added_w_goal = str_to_upper(consider_goal)
      )

      track_layers_added <- read_csv(
        file.path(new_repo, "temp", "track_layers_added.csv"),
        col_types = cols()) %>%
        rbind(track_layers_entry) # track_layers_added %>% filter(added_w_goal == str_to_upper(consider_goal))
      write_csv(track_layers_added, file.path(new_repo, "temp", "track_layers_added.csv"))


      ## UPDATE SCENARIO_DATA_YEARS ----
      ## need to recreate rm_lyrs and incl_new_lyrs with each layer reviewed
      ## since otherwise will overwrite goal-related layer rows of scenario_data_years tab!
      conf <- ohicore::Conf("conf")
      current_conf_scen_data_yrs <- conf$scenario_data_years
      l <- paste(c(sprintf("%s_.*", consider_goal), goal_specific_layers), collapse = "|")

      rm_lyrs <- setdiff(
        current_conf_scen_data_yrs$layer_name %>%
          grep(pattern = l, value = TRUE) %>%
          unique(),
        names(archive_layers$data) %>%
          grep(pattern = l, value = TRUE))

      keep_conf_scen_data_yrs <- current_conf_scen_data_yrs %>%
        filter(is.na(str_match(layer_name, paste(c(rm_lyrs, "none"), collapse = "|"))))

      ## have to do updates by layer -not all together- since years included in datasets vary
      ## use scenario_data_align when layer already has year information & enough years, else scenario_data_include()
      if("year" %in% names(layer_archive_version) & length(y) >= (length(scenario_yrs)+4)){

        tmp <- read_csv(file.path(new_repo, "layers", fn), col_types = cols()) %>%
          group_by(rgn_id) %>%
          summarise(n = n(), min_yr = min(year), max_yr = max(year)) # View(tmp)

        update_scen_data_yrs <- scenario_data_align(
          scen_data_years = keep_conf_scen_data_yrs,
          lyr_name = consider_lyr_nm,
          data_yrs = y,
          scen_yrs = min(scenario_yrs-4):max(scenario_yrs),
          approach = "timestep"
        )
        ## issue if end up with NA data_years... needs fixing...

      } else {
        update_scen_data_yrs <- scenario_data_include(
          scen_data_years = keep_conf_scen_data_yrs,
          scen_yrs = min(scenario_yrs-4):max(scenario_yrs),
          new_lyrs = consider_lyr_nm) %>%
          mutate(data_year = ifelse(is.na(data_year), dummy_data_yr, data_year))
      } # View(filter(update_scen_data_yrs, layer_name == consider_lyr_nm))

      write_csv(update_scen_data_yrs, file.path(new_repo, "conf", "scenario_data_years.csv"))
    }

    ## overwrite pressure and resilience matrices before moving on to the next goal/subgoal
    write_csv(int_pressure_mat, file.path(new_repo, "conf", "pressures_matrix.csv"))
    write_csv(int_resilience_mat, file.path(new_repo, "conf", "resilience_matrix.csv"))

    ## update 'pressure_categories' and 'resilience_categories' to match updated matrices
    new_prs_rows <-
    updated_prs_categ <- read_csv(file.path("conf", "pressure_categories.csv")) %>%
      filter() %>%
      rbind()
    new_res_rows <-
    updated_res_categ <- read_csv(file.path(new_repo, "conf", "resilience_categories.csv")) %>%
      filter() %>%
      rbind()

    write_csv(updated_prs_categ, file.path(new_repo, "conf", "pressure_categories.csv"))
    write_csv(updated_res_categ, file.path(new_repo, "conf", "resilience_categories.csv"))


    ## TRANSITION UPDATED GOAL FUNCTION ----
    ## copy goal subscript to functions.R main script

    old_funs <- grep(
      list.files(original_funs_dir, full.names = TRUE) %>%
        str_extract(pattern = "v2015/.*R"),
      pattern = sprintf(".*%s.*R", consider_goal),
      value = TRUE, invert = TRUE)
    new_funs <- grep(
      list.files(new_funs_dir, full.names = TRUE) %>%
        str_extract(pattern = "multiyear_v2015/.*R"),
      pattern = sprintf(".*%s.*R|finalizescores", consider_goal),
      value = TRUE)

    if(length(new_funs) != 0){
      finalizescores_fun <- grep(new_funs, pattern = "finalizescores",
                                 value = TRUE) %>% as.data.frame()
      new_funs <- grep(new_funs, pattern = "finalizescores",
                       value = TRUE, invert = TRUE) %>% as.data.frame()
      old_funs <- old_funs %>% as.data.frame()

      funs_to_configure <- rbind(old_funs, new_funs, finalizescores_fun) %>%
        rename(funs_locations = ".")
      configure_functions(new_repo, test_funs_list = funs_to_configure)

    } else {
      stop(sprintf("new function for goal %s not found...", str_to_upper(consider_goal)))
    }

    ## config.R pressures_elements and resilience_elements must be updated with each goal transitioned


    ## check resulting scores
    intermediate_scores <- calculate_scores(new_repo, 2015) # View(intermediate_scores)
    chks <- compare_scores(intermediate_scores, 2015, previous_scores, 2015, dim = "score", goal = "Index")
    chks[[1]]
  }

  ## FINAL STEPS ----
  ## 1. remove from layers folder, layer.csv, and scenario_data_years everything not used by functions.R
  ## 2. update pressure and resilience categories tables
  ## 3. check pressure, resilience, and layers tables after finishing all reconfiguration
  prs_tab0 <- read_csv(file.path(archive_filepath, "conf", "pressures_matrix.csv"))
  chk_prs <- compare_tabs(
    prs_tab0, int_pressure_mat, key_row_var = "goal",
    check_cols = setdiff(names(prs_tab0), c("goal", "element", "element_name"))
  )
  res_tab0 <- read_csv(file.path(archive_filepath, "conf", "resilience_matrix.csv"))
  chk_res <- compare_tabs(
    res_tab0, int_pressure_mat, key_row_var = "goal",
    check_cols = setdiff(names(res_tab0), c("goal", "element", "element_name"))
  )

  ## wrap up the function, return some results
  return(list(updated_layers = updated_layers,
              track_layers_added = track_layers_added,
              pressure_mat = int_pressure_mat,
              resilience_mat = int_resilience_mat))
}