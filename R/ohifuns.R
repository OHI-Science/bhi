CalculateAll = function(conf, layers){

  ## Remove global scores
  if (exists('scores', envir=.GlobalEnv)) rm(scores, envir=.GlobalEnv)

  ## Run Setup, all goals
  if ('Setup' %in% ls(conf$functions)){
    cat('Running Setup()...\n')
    conf$functions$Setup()
  }

  ## Access Pre-Index functions: Status and Trend, by goal
  goals_X = conf$goals %>%
    # dplyr::filter(!is.na(preindex_function)) %>% # it it realizes it's NA
    dplyr::filter(preindex_function != "NA") %>% # if it thinks NA is a character string
    dplyr::arrange(order_calculate)

  ## Setup scores variable; many rbinds to follow
  scores = data.frame(
    goal      = character(0),
    dimension = character(0),
    region_id = integer(0),
    score     = numeric())

  ## Calculate Status and Trend, all goals
  for (i in 1:nrow(goals_X)){ # i=14
    g = goals_X$goal[i]
    cat(sprintf('Calculating Status and Trend for each region for %s...\n', g))

    ## assign scores variable to conf$functions environment
    assign('scores', scores, envir=conf$functions)

    ## error if scores already calculated for g
    if (nrow(subset(scores, goal==g & dimension %in% c('status','trend')))!=0) {
      stop(sprintf('Scores were assigned to goal %s by previous goal function.', g))
    }

    ## calculate scores for goal g and create scores_g
    scores_g = eval(parse(text=goals_X$preindex_function[i]), envir=conf$functions)

    ## ungroup if scores_g is a grouped dataframe
    if ( dplyr::is.grouped_df(scores_g) ){
      cat(sprintf('Ungrouping scores variable returned from %s model...\n', g))
      scores_g <- scores_g %>%
        ungroup()
    }

    ## error if 'status' or 'trend' are missing
    if ( !all( c('status', 'trend') %in% unique(scores_g$dimension)) ){
      stop(sprintf('Missing "status" or "trend" dimension in %s goal model\n', g))
    }
    ## error if something other than 'status' or 'trend' as dimension
    if ( !all(unique(scores_g$dimension) %in% c('status', 'trend')) ){
      stop(sprintf('"status" and "trend" should be the only dimensions in %s goal model\n', g))
    }

    if (nrow(scores_g) > 0){
      scores = rbind(scores, scores_g[,c('goal','dimension','region_id','score')])
    }
  }

  ## Calculate Pressures, all goals
  #  layers = Layers(layers.csv = 'layers.csv', layers.dir = 'layers')
  scores_P = CalculatePressuresAll(layers, conf)
  scores = rbind(scores, scores_P)

  ## Calculate Resilience, all goals
  scores_R = CalculateResilienceAll(layers, conf, scores)
  scores = rbind(scores, scores_R)
  scores = data.frame(scores)

  ## Calculate Goal Score and Likely Future, all goals
  goals_G = as.character(unique(subset(scores, dimension=='status', goal, drop=T)))
  for (g in goals_G){ # g = 'FIS'
    cat(sprintf('Calculating Goal Score and Likely Future for each region for %s...\n', g))

    ## spread the scores by dimension
    v = scores %>%
      dplyr::filter(goal == g) %>%
      tidyr::spread(dimension, score)

    ## message if missing dimension, assign NA
    for (col in c('status','trend','pressures','resilience')){
      if (!col %in% names(v)){
        cat(sprintf('  missing %s dimension, assigning NA!\n', col))
        v[col] = NA
      }
    }

    ## Calculations and Scaling
    x = CalculateGoalIndex(
      id         = v$region_id,
      status     = pmin(v$status/100, 1), # weird thing where ECO status is 1 and gets 'out of bounds' warning...
      trend      = v$trend,
      resilience = v$resilience/100,
      pressure   = v$pressures/100,
      DISCOUNT      = conf$config$goal_discount,
      BETA          = conf$config$goal_beta,
      default_trend = conf$config$default_trend)
    x$score = x$score * 100
    x$xF    = x$xF * 100

    ## Gather to scores format: goal, dimension, region_id, score
    scores_G = x %>%
      dplyr::select(region_id = id,
                    future    = xF,
                    score) %>%
      tidyr::gather(dimension, score, -region_id) %>%
      dplyr::mutate(goal = g) %>%
      dplyr::select(goal, dimension, region_id, score)

    ## bind to other scores
    scores = rbind(scores, scores_G)
  }


  ## Post-Index functions: Calculate Status, Trend, Likely Future State and Scores for 'Supragoals'
  # goals_Y = subset(conf$goals, !is.na(postindex_function))
  goals_Y = conf$goals %>%
    # dplyr::filter(!is.na(preindex_function)) %>% # if it realizes it's NA
    dplyr::filter(postindex_function != "NA") # if it thinks NA is a character string

  supragoals = goals_Y$goal
  # supragoals = subset(conf$goals, is.na(parent), goal, drop=T); supragoals

  for (i in 1:nrow(goals_Y)){ # i = 1

    cat(sprintf('Calculating post-Index function for each region for %s...\n', goals_Y$goal[i]))

    ## load environment and run function from functions.r
    assign('scores', scores, envir=conf$functions)
    scores = eval(parse(text=goals_Y$postindex_function[i]), envir=conf$functions)
  }

  ## Calculate Overall Index Scores for each region using goal weights
  cat(sprintf('Calculating Index Score for each region using goal weights to combine goal scores...\n'))

  # calculate weighted-mean Index scores from goal scores and rbind to 'scores' variable
  scores =
    rbind(scores,
          scores %>%

            # filter only supragoal scores, merge with supragoal weightings
            dplyr::filter(dimension=='score',  goal %in% supragoals) %>%
            merge(conf$goals %>%
                    dplyr::select(goal, weight)) %>%
            dplyr::mutate(weight = as.numeric(weight)) %>%

            # calculate the weighted mean of supragoals, add goal and dimension column
            dplyr::group_by(region_id) %>%
            dplyr::summarise(score = weighted.mean(score, weight, na.rm=T)) %>%
            dplyr::mutate(goal      = 'Index',
                          dimension = 'score') %>%
            data.frame())


  ## Calculate Overall Index Likely Future State for each region
  cat(sprintf('Calculating Index Likely Future State for each region...\n'))

  # calculate weighted-mean Likely Future State scores and rbind to 'scores' variable
  scores =
    rbind(scores,
          scores %>%

            # filter only supragoal scores, merge with supragoal weightings
            dplyr::filter(dimension=='future',  goal %in% supragoals) %>%
            merge(conf$goals %>%
                    dplyr::select(goal, weight)) %>%

            # calculate the weighted mean of supragoals, add goal and dimension column
            dplyr::group_by(region_id) %>%
            dplyr::summarise(score = weighted.mean(score, weight, na.rm=T)) %>%
            dplyr::mutate(goal      = 'Index',
                          dimension = 'future') %>%
            data.frame())

  ## Post-process scores, but pre-global calculation: for global assessment only
  if ('PreGlobalScores' %in% ls(conf$functions)){
    cat(sprintf('Calculating Post-process PreGlobalScores() function for each region...\n'))
    scores = conf$functions$PreGlobalScores(layers, conf, scores)
  }

  ## Assessment Areas (sometimes known as 'global', region_id-0) scores by area weighting
  cat(sprintf('Calculating scores for ASSESSMENT AREA (region_id=0) by area weighting...\n'))

  ## Calculate area-weighted Assessment Area scores and rbind to all scores
  scores = rbind(
    scores,
    scores %>%

      # filter only score, status, future dimensions, merge to the area (km2) of each region
      # dplyr::filter(dimension %in% c('score','status','future')) %>%
      merge(ohicore::SelectLayersData(layers, layers=conf$config$layer_region_areas, narrow=T) %>%
              dplyr::select(region_id = id_num,
                            area      = val_num)) %>%

      # calculate weighted mean by area
      dplyr::group_by(goal, dimension) %>%
      dplyr::summarise(score = weighted.mean(score, area, na.rm=T),
                       region_id = 0) %>%
      ungroup())

  ## post-process
  if ('FinalizeScores' %in% ls(conf$functions)){
    cat(sprintf('Calculating FinalizeScores function...\n'))
    scores = conf$functions$FinalizeScores(layers, conf, scores)
  }

  ## check that scores are not duplicated
  stopifnot(sum(duplicated(scores[,c('region_id','goal','dimension')]))==0)

  # return scores
  return(scores)
}

CalculatePressuresAll <- function(layers, conf) {

  ### reporting 1
  cat(sprintf('Calculating Pressures for each region...\n'))

  ### get pressure matrix, goal elements, weights, categories, layers
  p_matrix <- conf$pressures_matrix
  p_matrix <- tidyr::gather(p_matrix, layer, m_intensity,
                            -c(goal, element, element_name)) %>%    ### format the pressure matrix so it is a dataframe
    dplyr::filter(!is.na(m_intensity)) %>%
    dplyr::select(goal, element, layer, m_intensity)

  ### p_elements: make into a data.frame
  p_element <- conf$config$pressures_element
  if (length(p_element) >= 1) { ### only if there are any goals that have elements
    p_element <- plyr::ldply(p_element)
    names(p_element) <- c('goal', 'layer')
  }

  ### gamma weighting for social vs. ecological pressure categories
  p_gamma <- conf$config$pressures_gamma

  ### table describing pressure categories and subcategories
  p_categories <- unique(conf$pressure_categories)

  ### reporting 2
  cat(sprintf('There are %s pressures subcategories: %s \n',
              length(unique(p_categories$subcategory)),
              paste(unique(p_categories$subcategory), collapse = ', ')))

  ### error if the config.R weighting files are not actually included in the the data
  if ( !is.null(p_element) ) {
    obs_data <- ohicore::SelectLayersData(layers, layers = p_element$layer) %>%
      .$layer %>%
      unique()
    exp_data <- unique(p_element$layer)
    dif <- setdiff(exp_data, obs_data)
    if (length(dif) > 0) {
      stop(sprintf('weighting data layers identified in config.r do not exist; please update layers.csv and layers folder to include: %s',
                   paste(dif, collapse = ', ')))
    }
  }

  ### error if pressure categories deviate from "ecological" and "social"
  check <- setdiff(c("ecological", "social"), unique(p_categories$category))
  if (length(check) > 0){
    stop(sprintf('In pressures_categories.csv, the "category" variable does not include %s',
                 paste(check, collapse = ', ')))
  }

  check <- setdiff(unique(p_categories$category), c("ecological", "social"))
  if (length(check) > 0) {
    stop(sprintf('In pressures_categories.csv, the "category" variable includes %s',
                 paste(check, collapse = ', ')))
  }


  ### list of pressure layers from the pressures_matrix
  p_layers <- sort(names(conf$pressures_matrix)[!names(conf$pressures_matrix) %in%
                                                  c('goal', 'element', 'element_name')])


  ### error if layer value range is incorrect
  if (!all(subset(layers$meta, layer %in% p_layers, val_0to1, drop = TRUE))){
    stop(sprintf('These pressures layers must range in value from 0 to 1:\n%s',
                 paste(
                   unlist(
                     layers$meta %>%
                       dplyr::filter(layer %in% p_layers & val_0to1 == F) %>%
                       dplyr::select(layer)),
                   collapse = ', ')))
  }

  ### error if matrix multipliers are not between 0 and 3
  # if(sum(p_matrix$value > 3 | p_matrix$value < 1) > 0) {
  if(sum(!p_matrix$m_intensity %in% c(1:3) ) > 0) {
    message(sprintf('There are values in pressures_matrix.csv that are > 3 or < 1'))
  }


  ### error check: that matrix and categories table include the same pressure layers
  check <- setdiff(p_layers, p_categories$layer)
  if (length(check) >= 1) {
    message(sprintf('These pressure layers are in the pressure_matrix.csv but not in pressure_categories.csv:\n%s',
                    paste(check, collapse = ', ')))
  }

  check <- setdiff(p_categories$layer, p_layers)
  if (length(check) >= 1) {
    message(sprintf('These pressure layers are in the pressure_categories.csv but not in the pressure_matrix.csv:\n%s',
                    paste(check, collapse = ', ')))
  }

  check <- setdiff(p_layers, names(layers))
  if (length(check) >= 1) {
    message(sprintf('These pressure layers are in the pressure_matrix.csv but not in the layers environment:\n%s',
                    paste(check, collapse = ', ')))
  }


  ### setup initial data.frame for column binding results by region
  regions_dataframe <- ohicore::SelectLayersData(layers,
                                                 layers = conf$config$layer_region_labels,
                                                 narrow = TRUE) %>%
    dplyr::select(region_id = id_num)
  regions_vector <- regions_dataframe[['region_id']]


  ### create the weighting scheme
  eco_soc_weight <- data.frame(category = c("ecological", "social"),
                               weight   = c(p_gamma, 1 - p_gamma),
                               stringsAsFactors = FALSE)


  ### ID relevant data year for each layer (if there is no year data, the year is assigned as 20100)
  if(nrow(conf$scenario_data_years) > 0) {

    scenario_data_year <- conf$scenario_data_years %>%
      dplyr::filter(layer_name %in% p_layers)

    scenario_data_year <- scenario_data_year[scenario_data_year$scenario_year == layers$data$scenario_year, ] %>%
      dplyr::select(layer_name, scenario_year, data_year)

    layers_no_years <- setdiff(p_layers, scenario_data_year$layer_name)

    #if there are layers that do not have years, then we add them in through this layers_no_years_df year year 20100
    if(length(layers_no_years) > 0){

      layers_no_years_df <- data.frame(layer_name    = layers_no_years,
                                       scenario_year = 20100,  ### creating a fake variable to match up here
                                       data_year     = 20100,
                                       stringsAsFactors = FALSE)

      scenario_data_year <- rbind(scenario_data_year, layers_no_years_df)
    }

    scenario_data_year <- scenario_data_year %>%
      dplyr::select(layer = layer_name, year = data_year)

  } else{
    scenario_data_year <- data.frame(layer = p_layers,
                                     year  = 20100,
                                     stringsAsFactors = FALSE)
  }



  ### get each pressure data layer and select the appropriate year of data:
  p_rgn_layers_data <- ohicore::SelectLayersData(layers, layers = p_layers)

  if(length(which(names(p_rgn_layers_data) == "year")) == 0){
    p_rgn_layers_data$year <- NA
  }


  p_rgn_layers_data <- p_rgn_layers_data  %>%
    dplyr::filter(id_num %in% regions_vector) %>%
    dplyr::select(region_id = id_num,
                  year,
                  val_num,
                  layer) %>%
    dplyr::filter(!is.na(val_num)) %>%
    dplyr::mutate(year = ifelse(is.na(year), 20100, year))

  p_rgn_layers <- scenario_data_year %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::left_join(p_rgn_layers_data, by=c("year", "layer")) %>%
    select(region_id, val_num, layer)

  ### error check: matrix and region data layers include the same pressure factors
  check <- setdiff(p_layers, p_rgn_layers$layer)
  if (length(check) >= 1) {
    message(sprintf('These pressure layers are in the pressures_matrix.csv, but there are no associated data layers:\n%s',
                    paste(check, collapse = ', ')))
  }

  check <- setdiff(p_rgn_layers$layer, p_layers)
  if (length(check) >= 1) {
    message(sprintf('These pressure layers have data layers, but are not included in the pressures_matrix.csv:\n%s',
                    paste(check, collapse = ', ')))
  }


  ### further preparation of matrix data for analysis
  p_matrix <- p_matrix %>%
    dplyr::left_join(p_categories, by="layer") %>%
    dplyr::group_by(goal, element, category, subcategory) %>%
    dplyr::mutate(max_subcategory = max(m_intensity)) %>%
    data.frame()

  ### merge the region data layers and the pressure matrix
  rgn_matrix <- dplyr::left_join(p_matrix, p_rgn_layers, by="layer")


  ### summarize cumulative pressure for each subcategory
  ### (first find maximum pressure in each pressure subcategory)
  calc_pressure <- rgn_matrix %>%
    dplyr::mutate(pressure_intensity = m_intensity * val_num) %>%
    data.frame()


  ### separate method for ecological pressures
  calc_pressure_eco <- calc_pressure %>%
    dplyr::filter(category == "ecological") %>%
    dplyr::group_by(goal, element, category, subcategory, max_subcategory, region_id) %>%
    dplyr::summarize(cum_pressure = sum(pressure_intensity, na.rm = TRUE) / 3) %>%
    dplyr::mutate(cum_pressure = ifelse(cum_pressure > 1, 1, cum_pressure)) %>%
    dplyr::ungroup() %>%
    data.frame()

  ### separate method for social pressures
  calc_pressure_soc <- calc_pressure %>%
    dplyr::filter(category == "social") %>%
    dplyr::group_by(goal, element, category, subcategory, max_subcategory, region_id) %>%
    dplyr::summarize(cum_pressure = mean(pressure_intensity)) %>%
    dplyr::mutate(cum_pressure = ifelse(cum_pressure > 1, 1, cum_pressure)) %>%
    dplyr::ungroup() %>%
    data.frame()

  ### combine social and ecological
  calc_pressure <- rbind(calc_pressure_eco, calc_pressure_soc)

  ### average of the pressure subcategories (weighted by highest intensity for each region/subcategory)
  calc_pressure <- calc_pressure %>%
    dplyr::group_by(goal, element, category, region_id) %>%
    dplyr::summarize(pressure = weighted.mean(cum_pressure, max_subcategory)) %>%
    dplyr::ungroup() %>%
    data.frame()

  ### combine ecological and social pressures, based on gamma
  calc_pressure <- calc_pressure %>%
    dplyr::left_join(eco_soc_weight, by="category") %>%
    dplyr::group_by(goal, element, region_id) %>%
    dplyr::summarize(pressure = weighted.mean(pressure, weight)) %>%
    dplyr::ungroup() %>%
    data.frame()


  ### Deal with goals with goal elements
  if (length(p_element) >= 1) { ### only if there are any goals that have elements
    p_element_layers <- ohicore::SelectLayersData(layers, layers = p_element$layer) %>%
      dplyr::filter(id_num %in% regions_vector) %>%
      dplyr::select(region_id  = id_num,
                    element    = category,
                    element_wt = val_num,
                    layer) %>%
      dplyr::filter(!is.na(element)) %>%
      dplyr::filter(!is.na(element_wt)) %>%
      dplyr::left_join(p_element, by="layer") %>%
      dplyr::select(region_id, goal, element, element_wt) %>%
      dplyr::mutate(element = as.character(element))

    ### data check:  Make sure elements of each goal are included in the pressure_matrix.R
    check <- setdiff(paste(p_element_layers$goal,
                           p_element_layers$element,
                           sep = "-"),
                     paste(p_matrix$goal[p_matrix$goal %in% p_element$goal],
                           p_matrix$element[p_matrix$goal %in% p_element$goal],
                           sep = "-"))
    if (length(check) >= 1) {
      message(sprintf('These goal-elements are in the weighting data layers, but not included in the pressure_matrix.csv:\n%s',
                      paste(check, collapse = ', ')))
    }

    check <- setdiff(paste(p_matrix$goal[p_matrix$goal %in% p_element$goal],
                           p_matrix$element[p_matrix$goal %in% p_element$goal],
                           sep = "-"),
                     paste(p_element_layers$goal,
                           p_element_layers$element,
                           sep = "-"))
    if (length(check) >= 1) {
      message(sprintf('These goal-elements are in the pressure_matrix.csv, but not included in the weighting data layers:\n%s',
                      paste(check, collapse = ', ')))
    }

    ### Reset calc_pressure as a weighted average of the elements:
    calc_pressure <- calc_pressure %>%
      dplyr::left_join(p_element_layers, by = c('region_id', 'goal', 'element')) %>%
      dplyr::filter(!(is.na(element_wt) & goal %in% p_element$goal))  %>%
      dplyr::mutate(element_wt = ifelse(is.na(element_wt), 1, element_wt)) %>%
      dplyr::group_by(goal, region_id) %>%
      dplyr::summarize(pressure = weighted.mean(pressure, element_wt)) %>% ### retain a 'pressure' column
      dplyr::ungroup() %>%
      data.frame()

  } ### end if(length(p_element) >= 1) for goals with elements

  ### return scores
  scores <- regions_dataframe %>%
    dplyr::left_join(calc_pressure, by = "region_id") %>%
    dplyr::mutate(dimension = "pressures") %>%
    dplyr::select(goal, dimension, region_id, score = pressure) %>%
    dplyr::mutate(score = round(score * 100, 2))

  return(scores)

}

CalculateResilienceAll = function(layers, conf, scores){

  # reporting 1
  cat(sprintf('Calculating Resilience for each region...\n'))

  ## get resilience matrix, goal elements, weights, categories, layers
  r_element = conf$config$resilience_element                              # weighting data for goals with elements
  if (length(r_element) >= 1) { # only if there are any goals that have elements
    r_element = plyr::ldply(r_element)
    names(r_element) <- c('goal', 'layer')
  }

  r_gamma = conf$config$resilience_gamma                                        # gamma weighting for social vs. ecological resilience categories

  r_matrix = conf$resilience_matrix
  r_matrix = within(r_matrix, {element[is.na(element)] = ''})               # resilience matrix
  r_matrix <- tidyr::gather(r_matrix, layer, included,
                            -c(goal, element)) %>%    # format the resilience matrix so it is a dataframe
    dplyr::filter(!is.na(included)) %>%
    dplyr::select(goal, element, layer)

  r_categories = conf$resilience_categories                                           # resilience weights table

  r_layers = setdiff(names(conf$resilience_matrix), c('goal','element','element_name'))   # list of resilience layers from matrix

  # reporting 2
  cat(sprintf('There are %s Resilience subcategories: %s \n',
              length(unique(r_categories$subcategory)),
              paste(unique(r_categories$subcategory), collapse=', ')))


  ## error if the config.R weighting files are not actually included in the the data
  if ( !is.null(r_element) ) {
    obs_data <- dplyr::select(ohicore::SelectLayersData(layers, layers=r_element$layer), layer)
    obs_data <- unique(obs_data$layer)
    exp_data <- unique(r_element$layer)
    dif <- setdiff(exp_data, obs_data)
    if (length(dif) > 0) {
      stop(sprintf('weighting data layers identified in config.r do not exist; please update layers.csv and layers folder to include: %s',
                   paste(dif, collapse=', ')))
    }
  }

  # error if resilience categories deviate from "ecological" and "social"
  check <- setdiff(c("ecological", "social"), unique(r_categories$category))
  if (length(check) > 0){
    stop(sprintf('In resilience_categories.csv, the "category" variable does not include %s', paste(check, collapse=', ')))
  }

  check <- setdiff(unique(r_categories$category), c("ecological", "social"))
  if (length(check) > 0){
    stop(sprintf('In resilience_categories.csv, the "category" variable includes %s', paste(check, collapse=', ')))
  }


  ## error unless layer value range is correct
  if (!all(subset(layers$meta, layer %in% r_layers, val_0to1, drop=T))){
    stop(sprintf('These resilience layers do not range in value from 0 to 1:\n%s',
                 paste(
                   unlist(
                     layers$meta %>%
                       filter(layer %in% r_layers & val_0to1==F) %>%
                       select(layer)),
                   collapse = ', ')))
  }

  ## error check: that matrix and categories table include the same resilience factors
  check <- setdiff(r_layers, r_categories$layer)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers are in the resilience_matrix.csv but not in resilience_categories.csv:\n%s',
                    paste(check, collapse=', ')))
  }

  check <- setdiff(r_categories$layer, r_layers)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers are in the resilience_categories.csv but not in the resilience_matrix.csv:\n%s',
                    paste(check, collapse=', ')))
  }


  ## setup initial data.frame for column binding results by region
  regions_dataframe = ohicore::SelectLayersData(layers, layers=conf$config$layer_region_labels, narrow=T) %>%
    dplyr::select(region_id = id_num)
  regions_vector = regions_dataframe[['region_id']]

  ## create the weighting scheme
  eco_soc_weight <- data.frame(category = c("ecological", "social"),
                               weight = c(r_gamma, 1-r_gamma))
  eco_soc_weight$category <- as.character(eco_soc_weight$category)

  ### ID relevant data year for each layer (if no year data, the year is assigned as 20100)
  if(dim(conf$scenario_data_years)[1]>0){

    scenario_data_year <- conf$scenario_data_years %>%
      dplyr::filter(layer_name %in% r_layers)

    scenario_data_year <- scenario_data_year[scenario_data_year$scenario_year == layers$data$scenario_year, ] %>%
      dplyr::select(layer_name, scenario_year, data_year)

    layers_no_years <- setdiff(r_layers, scenario_data_year$layer_name)

    #if there are layers that do not have years, then we add them in through this layers_no_years_df year year 20100
    if(length(layers_no_years) > 0){

      layers_no_years_df <- data.frame(layer_name=layers_no_years,
                                       scenario_year = 20100,  # creating a fake variable to match up here
                                       data_year = 20100)
      scenario_data_year <- rbind(scenario_data_year, layers_no_years_df)
    }

    scenario_data_year <- scenario_data_year %>%
      dplyr::select(layer = layer_name, year=data_year)

  } else{
    scenario_data_year <- data.frame(layer=r_layers, year=20100)
  }

  scenario_data_year <- scenario_data_year %>%
    mutate(layer = as.character(layer))

  ### get the regional data layer associated with each resilience data layer:
  r_rgn_layers_data <- bind_rows(
    ohicore::SelectLayersData(layers, layers=r_layers),
    scores %>%
      filter(goal == "BD", dimension == "status") %>%
      mutate(
        val_num = score/100, id_name = "region_id", year = 2019,
        layer = "res_biodiversity",
        val_name = "resilience_score", category_name = NA,
        flds = "id_num | year | val_num"
      ) %>%
      select(id_num = region_id, val_num, year, layer, id_name, val_name, category_name, flds)
  )

  if(length(which(names(r_rgn_layers_data)=="year"))==0){
    r_rgn_layers_data$year = NA
  }

  r_rgn_layers_data <- r_rgn_layers_data  %>%
    dplyr::filter(id_num %in% regions_vector) %>%
    dplyr::select(region_id = id_num,
                  year,
                  val_num,
                  layer) %>%
    dplyr::filter(!is.na(val_num)) %>%
    dplyr::mutate(year = ifelse(is.na(year), 20100, year))

  r_rgn_layers <- scenario_data_year %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::left_join(r_rgn_layers_data, by=c("year", "layer")) %>%
    select(region_id, val_num, layer)

  ## error check: matrix and region data layers include the same resilience factors
  check <- setdiff(r_layers, r_rgn_layers$layer)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers are in the resilience_matrix.csv, but there are no associated data layers:\n%s',
                    paste(check, collapse=', ')))
  }

  check <- setdiff(r_rgn_layers$layer, r_layers)
  if (length(check) >= 1) {
    message(sprintf('These resilience layers have data layers, but are not included in the resilience_matrix.csv:\n%s',
                    paste(check, collapse=', ')))
  }



  # merge the region data layers and the resilience matrix
  rgn_matrix <- dplyr::left_join(r_matrix, r_rgn_layers, by="layer")

  # merge rgn_and_matrix data with the information in the resilience_categories.csv
  rgn_matrix_weights <- dplyr::left_join(rgn_matrix, r_categories, by="layer")

  ## average subcategories of resilience layers
  calc_resil <- rgn_matrix_weights %>%
    dplyr::group_by(goal, element, region_id, category, category_type, subcategory) %>%
    dplyr::summarize(max_subcategory = max(weight),
                     val_num = weighted.mean(val_num, weight)) %>%
    data.frame()

  ## average category types of resilience layers (weight by max weight in each subcategory)
  calc_resil <- calc_resil %>%
    dplyr::group_by(goal, element, region_id, category, category_type) %>%
    dplyr::summarize(val_num = weighted.mean(val_num, max_subcategory)) %>%
    data.frame()

  ## average ecological element (ecosystem and regulatory)
  calc_resil <- calc_resil %>%
    dplyr::group_by(goal, element, region_id, category) %>%
    dplyr::summarize(val_num = mean(val_num)) %>%
    data.frame()

  ## combine ecological and social based on resilience gamma weighting
  calc_resil <- calc_resil %>%
    dplyr::left_join(eco_soc_weight, by="category") %>%
    dplyr::group_by(goal, element, region_id) %>%
    dplyr::summarise(val_num = weighted.mean(val_num, weight)) %>%
    data.frame()

  ## For goals with elements, get the relevant data layers used for weights
  if (length(r_element) >= 1) { # only if there are any goals that have elements
    r_element_layers <- ohicore::SelectLayersData(layers, layers=r_element$layer) %>%
      dplyr::filter(id_num %in% regions_vector) %>%
      dplyr::select(region_id = id_num,
                    element = category,
                    element_wt = val_num,
                    layer) %>%
      dplyr::filter(!is.na(element)) %>%
      dplyr::filter(!is.na(element_wt)) %>%
      dplyr::left_join(r_element, by="layer") %>%
      dplyr::select(region_id, goal, element, element_wt) %>%
      dplyr::mutate(element = as.character(element))

    ## data check:  Make sure elements for each goal are included in the resilience_matrix.R
    check <- setdiff(paste(r_element_layers$goal, r_element_layers$element, sep= "-"),
                     paste(r_matrix$goal[r_matrix$goal %in% r_element$goal], r_matrix$element[r_matrix$goal %in% r_element$goal], sep= "-"))
    if (length(check) >= 1) {
      message(sprintf('These goal-elements are in the weighting data layers, but not included in the resilience_matrix.csv:\n%s',
                      paste(check, collapse=', ')))
    }

    check <- setdiff(paste(r_matrix$goal[r_matrix$goal %in% r_element$goal], r_matrix$element[r_matrix$goal %in% r_element$goal], sep= "-"),
                     paste(r_element_layers$goal, r_element_layers$element, sep= "-"))
    if (length(check) >= 1) {
      message(sprintf('These goal-elements are in the resilience_matrix.csv, but not included in the weighting data layers:\n%s',
                      paste(check, collapse=', ')))
    }

    ## A weighted average of the elements:
    calc_resil <- calc_resil %>%
      dplyr::left_join(r_element_layers, by=c('region_id', 'goal', 'element')) %>%
      dplyr::filter(!(is.na(element_wt) & goal %in% r_element$goal))  %>%
      dplyr::mutate(element_wt = ifelse(is.na(element_wt), 1, element_wt)) %>%
      dplyr::group_by(goal, region_id) %>%
      dplyr::summarize(val_num = weighted.mean(val_num, element_wt))

  } # end if(length(r_element) >= 1) for goals with elements

  # return scores
  scores <- regions_dataframe %>%
    dplyr::left_join(calc_resil, by="region_id") %>%
    dplyr::mutate(dimension="resilience") %>%
    dplyr::select(goal, dimension, region_id, score=val_num) %>%
    dplyr::mutate(score = round(score*100, 2))

  return(scores)

}

CalculateGoalIndex <- function(id, status, trend, resilience, pressure,
                               DISCOUNT = 1.0, BETA = 0.67, default_trend = 0.0, xlim = c(0, 1)) {

  # verify parameters
  #if (getOption('debug', FALSE)) {
  if(!(BETA >= 0 && BETA <= 1))
    stop('Beta parameter must be between 0 and 1; check it in config.R')
  if(!DISCOUNT >= 0)
    stop('Discount parameter must be greater than 0 (1.0 = no discounting); check it in config.R')
  #}

  # Simplify symbols based on math writeup
  d <- data.frame(id = id, x = status, t = trend, r = resilience, p = pressure)

  # replace a trend of NA with a default (0)
  if (!is.null(default_trend) && is.numeric(default_trend) && any(is.na(d$t))) {
    d$t[is.na(d$t)] <- default_trend
  }

  # enforce domains
  if(!(min(d$x, na.rm = T) >= 0  && max(d$x, na.rm = T) <= xlim[2]))
    stop('one or more status scores exceed bounds of 0 to ', xlim[2])
  # if(!(min(d$t, na.rm = T) >= -1 && max(d$t, na.rm = T) <= 1))
  if(!(min(d$t, na.rm = T) >= -1 && max(d$t, na.rm = T) <= 1))
    stop('one or more trend scores exceed bounds of -1 to +1')
  if(!(min(d$r, na.rm = T) >= 0  && max(d$r, na.rm = T) <= xlim[2]))
    stop('one or more resilience scores exceed bounds of 0 to ', xlim[2])
  # if(!(min(d$p, na.rm = T) >= 0  && max(d$p, na.rm = T) <= xlim[2]))

  ## manage when resilience or pressure is NA
  d$p <- ifelse(is.na(d$p), 0, d$p)
  d$r <- ifelse(is.na(d$r), 0, d$r)

  # compute "future" status, using all dimensions

  #handle cases with NA for resilience or pressures, converts NA to zero so it drops out of equation
  d$r <- ifelse(is.na(d$r), 0, d$r)
  d$p <- ifelse(is.na(d$p), 0, d$p)

  d$xF <- with(d, (DISCOUNT * (1 + (BETA * t) + ((1-BETA) * (r - p)))) * x)
  # clamp status domain to [0, 1]
  d$xF <- with(d, ohicore::score.clamp(xF, xlim = c(0,1)))
  # compute score using formula for individual goal component indicator
  d$score <- with(d, (x + xF)/2)

  # return results, rescaling if needed
  #if (identical(xlim, c(0,100))){
  #   d[,c('x','r','p','xF','score')] = d[,c('x','r','p','xF','score')]*100
  #}
  return(d)
}
