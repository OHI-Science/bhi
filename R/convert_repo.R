convert_repo <- function(new_repo, archive_filepath,
                         original_funs_dir, new_funs_dir,
                         scenario_yrs = 2015:2018, dummy_data_yr = 2014){

  ## SET UP / PREAMBLE ----
  setwd(archive_filepath)
  archive_layers <- ohicore::Layers("layers.csv", "layers") # layers object created from archive
  archive_layerscsv <- read_csv(file.path(archive_filepath, "layers.csv"), col_types = cols())
  setwd(new_repo)

  goal_code_list_archive <- funsR_goals_list(file.path(archive_filepath, "conf", "functions.R")) %>% str_to_lower()
  goal_code_list <- funsR_goals_list(file.path(new_repo, "conf", "functions.R")) %>% str_to_lower()


  ## WORK GOAL BY GOAL ----
  for(consider_goal in goal_code_list){

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
    for(consider_lyr_nm in goal_specific_layers){ # consider_lyr_nm = goal_specific_layers[8]

      cat(sprintf("incorporating and/or reviewing '%s' layer for '%s' goal\n\n", consider_lyr_nm, consider_goal))

      consider_layer <- archive_layers$data[[consider_lyr_nm]] # View(consider_layer)
      if("year" %in% names(consider_layer)){
        y <- consider_layer$year %>% unique() %>% sort() # years we can have in scenario_data_years.csv as data years
        cat("years found in layer:\n", paste(y, collapse = "\n"), sep = "")
      } else {
        message("no year column; adding dummy year col only for purpose of transitioning repo, review during data prep!")
      }

      ## REPLACE WITH ARCHIVED LAYER VERSION ----
      ## check filename (fn) to use, and whether it already exists in repo
      ## view archived layer, if needs a year column add it and save to layers folder, otherwise just copy over

      fn <- str_match(archive_layerscsv$filename, paste0(consider_lyr_nm, ".*.csv")) %>%
        lapply(function(x){x[!is.na(x)]}) %>%
        unlist()
      if(length(fn) != 1){
        message("more than one layer found in archive, using the first one found")
        fn <- fn[1]
      }
      cat(sprintf("filename for '%s' layer found in archive layers folder: %s\n", consider_lyr_nm, fn))

      ## IF NEED TO ADD YEAR COLUMN...
      ## adding dummy year cols only for purpose of transitioning repo!
      ## especially for trend, will be more carefully reviewed when doing actual data prep!
      ## remember to review how years are actually included when doing actual data prep!

      layer_archive_version <- read_csv(file.path(archive_filepath, "layers", fn),
                                        col_types = cols())
      if(!"year" %in% names(layer_archive_version)){
        yr_col_added <- layer_archive_version %>%
          mutate(year = 2014) # temporary for setting up repo!
        lyr_w_yr <- yr_col_added
      } else { lyr_w_yr <- layer_archive_version }

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
        rbind(track_layers_entry)
      write_csv(track_layers_added, file.path(new_repo, "temp", "track_layers_added.csv"))


      ## UPDATE SCENARIO_DATA_YEARS ----
      ## need to recreate rm_lyrs and incl_new_lyrs with each layer reviewed
      ## since otherwise will overwrite all goal-related layer rows of scenario_data_years tab!
      ## keep all the 'rm_lyrs' until adding/reviewing the last layer of the goal

      conf <- ohicore::Conf("conf")
      current_conf_scen_data_yrs <- conf$scenario_data_years
      r <- ifelse(
        str_sub(consider_lyr_nm, 1, str_length(consider_goal)) == consider_goal,
        paste0(consider_goal, "_.*"),
        consider_lyr_nm
      )

      rm_lyrs <- setdiff(
        current_conf_scen_data_yrs$layer_name %>%
          grep(pattern = r, value = TRUE) %>% unique(),
        names(archive_layers$data) %>%
          grep(pattern = r, value = TRUE))

      incl_new_lyrs <- setdiff(
        names(archive_layers$data) %>%
          grep(pattern = r,
               value = TRUE),
        current_conf_scen_data_yrs$layer_name %>%
          grep(pattern = r, value = TRUE) %>% unique())

      if(length(incl_new_lyrs) == 1){
        keep_conf_scen_data_yrs <- current_conf_scen_data_yrs %>%
          filter(is.na(str_match(layer_name, paste(c(rm_lyrs, "none"), collapse = "|"))))
      } else { keep_conf_scen_data_yrs <- current_conf_scen_data_yrs }

      if(length(incl_new_lyrs) != 0){
        ## have to do scenario_data_year updates by layer not all together, since years in datasets vary
        if("year" %in% names(layer_archive_version)){
          ## use scenario_data_align() when layer already has year information
          tmp <- read_csv(file.path(new_repo, "layers", fn), col_types = ) %>%
            group_by(rgn_id) %>%
            summarise(n = n(),
                      min_yr = min(year),
                      max_yr = max(year)) # View(tmp)

          update_scen_data_yrs <- scenario_data_align(
            scen_data_years = keep_conf_scen_data_yrs,
            lyr_name = consider_lyr_nm,
            data_yrs = max(tmp$min_yr):min(tmp$max_yr),
            scen_yrs = min(scenario_yrs-5):max(scenario_yrs), # need more years for trend calculations; accomodating different num yrs used in trend calcs?
            approach = "timestep"
          )
        } else {
          ## otherwise use scenario_data_include() function
          update_scen_data_yrs <- scenario_data_include(
            scen_data_years = keep_conf_scen_data_yrs,
            scen_yrs = min(scenario_yrs-5):max(scenario_yrs),
            new_lyrs = incl_new_lyrs) %>%
            mutate(data_year = ifelse(is.na(data_year), dummy_data_yr, data_year))
        }
        write_csv(update_scen_data_yrs, file.path(new_repo, "conf", "scenario_data_years.csv"))
      }

    }

    ## overwrite pressure and resilience matrices before moving on to the next goal/subgoal
    write_csv(int_pressure_mat, file.path(new_repo, "conf", "pressures_matrix.csv"))
    write_csv(int_resilience_mat, file.path(new_repo, "conf", "resilience_matrix.csv"))

    ## TRANSITION UPDATED GOAL FUNCTION TO FUNCTIONS.R ----
    ## for troubleshooting, pause here and go back to goal functions subscript to continue running model line-by-line...
    ## copy goal subscript to functions.R main script

    old_funs <- grep(list.files(original_funs_dir, full.names = TRUE) %>%
                       str_extract(pattern = "v2015/.*R"),
                     pattern = sprintf(".*%s.*R", consider_goal),
                     value = TRUE, invert = TRUE)

    new_funs <- grep(list.files(multiyear_funs_dir, full.names = TRUE) %>%
                       str_extract(pattern = "multiyear_v2015/.*R"),
                     pattern = sprintf(".*%s.*R", consider_goal),
                     value = TRUE)

    if(length(new_funs) != 0){
      funs_to_configure <- rbind(as.data.frame(old_funs),
                                 as.data.frame(new_funs)) %>%
        rename(funs_locations = old_funs)
      configure_functions(new_repo, test_funs_list = funs_to_configure)
    } else {
      stop(sprintf("new function for goal %s not found...", str_to_upper(consider_goal)))
    }
  }

  ## CHECKS AFTER FINISHING ALL RECONFIGURATION ----
  new_scores <- read_csv(file.path(new_repo, "scores.csv")) %>% mutate(year = 2014)
  previous_scores <- read_csv(file.path(archive_filepath, "scores.csv")) %>% mutate(year = 2014)
  chks <- compare_scores(new_scores, 2015, previous_scores, 2015, dim = "score", goal = "Index")
  chks[[1]]

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

  ## wrap up the function, return results
  return(list(updated_layers = updated_layers,
              track_layers_added = track_layers_added,
              pressure_mat = int_pressure_mat,
              resilience_mat = int_resilience_mat))
}
