
## VISUALIZATION ----

#' timeseries plot using highchart spline
#'
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns, e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param basins_or_rgns one of 'subbasins' or 'regions' to indicate which spatial units should be represented
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param dim the dimension the object/plot should represent, typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'
#'
#' @return

ts_highchart <- function(scores_csv, basins_or_rgns = "subbasins", goal_code, dim = "score"){

  ## checks and wrangling
  if(!"year" %in% colnames(scores_csv)){
    stop("no year column in given scores_csv")
  }

  scores <- scores_csv %>%
    filter(dimension == dim, goal == goal_code) %>%
    left_join(rgn_name_lookup, by = "region_id")

  if(basins_or_rgns == "subbasins"){
    scores <- scores %>%
      filter(region_id %in% subbasin_ids_vec)
  } else {
    scores <- scores %>%
      filter(region_id  %in% rgn_ids_vec)
  }
  scores <- scores %>%
    select(year, name = plot_title, score) %>%
    spread(key = name, value = score) # names become colnames

  nyrs <- length(unique(scores$year))

  if(nyrs == 1){
    stop("only one year of scores, cannot create meaningful time series plot")
  }


  ## color options for plot
  optn <- list(
    marker = list(enabled = FALSE),
    fillColor = list(
      linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
      stops = list(list(1, "transparent"))
    )
  )

  ## create the plot
  ts_hc_plot <- highchart() %>%
    hc_legend(layout = "vertical",
              align = "left",
              verticalAlign = "top") %>%
    hc_xAxis(categories = scores$year)

  for(r in colnames(scores)[-1]){ # colnames are regions/basins, without year

    ## scale numbers based on range of data within c(0, 100)
    r_y1 <- max(scores[[r]])/100
    r_y2 = min(scores[[r]])/100

    ts_hc_plot <- ts_hc_plot %>%
      hc_add_series(data = scores[[r]], name = r,
                    type = "area", # "areaspline"
                    showInLegend = FALSE,
                    marker = optn$marker,
                    fillColor = optn$fillColor,
                    color = list(
                      linearGradient = list(x1 = 0, y1 = r_y1, x2 = 0, y2 = r_y2),
                      stops = list(
                        list(0, "#8c031a"),
                        list(.15, "#8c031a"),
                        list(.40, "#cc0033"),
                        list(.60, "#fff78a"),
                        list(.75, "#f6ffb3"),
                        list(.90, "#009999"),
                        list(1.00, "#0278a7")
                      )
                    )
      )
  }
  ts_hc_plot %>%
    hc_add_theme(hc_theme_null())
}


#' create timeseries plot
#'
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param basins_or_rgns one of 'subbasins' or 'regions' to indicate which spatial units should be represented
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param dim the dimension the flowerplot petals should represent (typically OHI 'score')
#' @param make_html if TRUE, will create an hmtl/plottly version rather than ggplot to use e.g. for the website or shiny app
#' @param save can be either a directory in which to save the plot, or if TRUE will save to a default location
#'
#' @return

timeseries_plot <- function(scores_csv, basins_or_rgns = "subbasins", goal_code, dim = "score",
                            make_html = FALSE, save = FALSE){


  scores <- scores_csv
  if("dimension" %in% colnames(scores)){
    scores <- scores %>%
      filter(dimension == dim) %>%
      select(-dimension)
  }
  if("goal" %in% colnames(scores)){
    scores <- scores %>%
      filter(goal == goal_code) %>%
      select(-goal)
  }

  ## apply bhi_theme, in this case the same as for flowerplot
  thm <- apply_bhi_theme(plot_type = "timeseries")

  ## wrangle and join info to create plotting dataframe ----
  if(basins_or_rgns == "subbasins"){
    scores <- filter(scores, region_id > 500)
  } else {
    scores <- filter(scores, region_id < 100)
  }
  scores <- scores %>%
    left_join(rgn_name_lookup, by = "region_id") %>%
    mutate(score_unrounded = score,
           Score = round(score, 2)) %>%
    mutate(text = sprintf("%s:\n%s", str_replace(plot_title, ", ", "\n"), Score)) %>%
    select(region_id, score_unrounded, name = plot_title, text)

  ## make the plot
  plot_obj <- ggplot2::ggplot(data = scores) +
    geom_line(aes(x = year, y = score_unrounded, color = name))



  ## html plotly vs standard ggplot ----
  if(make_html){
    plot_obj <- plotly::ggplotly(plot_obj, tooltip = "text")

  } else {
    plot_obj <- plot_obj +
      ggrepel::geom_text_repel(
        aes(label = Name),
        family = "Helvetica",
        size = 3, angle = 0, direction = "x",
        segment.alpha = 0.1, segment.size = 0.1, box.padding = 0.8,
        show.legend = FALSE)
  }

  ## saving plots ----
  save_loc <- NULL
  if(is.character(save)){
    if(file.exists(save)){
      save_loc <- file.path(save, paste0("barplot_", goal_code, ".png"))
    }
  }
  if(isTRUE(save)){
    if("plotly" %in% class(plot_obj)){
      save_loc <- file.path(dir_assess, "reports", "widgets",
                            paste0("barplot_", goal_code, ".png"))
    } else {
      save_loc <- file.path(dir_assess, "reports", "figures",
                            paste0("barplot_", goal_code, ".png"))
    }
  }
  if(!is.null(save_loc)){
    if("plotly" %in% class(plot_obj)){
      htmlwidgets::saveWidget(as.widget(html_plot_obj), save_loc)
    } else {
      ggplot2::ggsave(filename = save_loc, plot = plot_obj, device = "png",
                      height = 5, width = 3, units = "in", dpi = 400)
    }
  }

  return(invisible(plot_obj))
}


## DATA RETRIEVAL / ARCHIVING ----


#' create connection to BHI database
#'
#' DBI::dbConnect would be simple enough but this way I won't have to remember all args each time...
#'
#' @param user
#' @return returns result of DBI::dbConnect function-- 'an S4 object that inherits from DBIConnection'

bhi_db_con <- function(username = NULL){

  dw <- get("database...")

  if(is.null(username)){username = dw$uid}
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = dw$driver,
    Server = dw$server,
    host = "",
    dbname = dw$database,
    port = 5432,
    user = username,
    password = rstudioapi::askForPassword("Database password: ")
  )
  return(con)
}


### NOTES TO SELF:
### need either DSNs (must be same between the development and deployment environment...)
### DSNs defined correspond to entries in odbc.ini file; defines details for a connection incl. server IP address, the db name, often access credentials
### ...or use use the config package: 'allows connection code in R to reference external file that defines values based on the environment'


## parsing xml for data retrieval...
for(u in urls){
  closeAllConnections()
  Sys.sleep(delay*i)
  i <- i + 1

  xmldoc <- xmlParse(u)
  rootNode <- xmlRoot(xmldoc)
  data0 <- xmlSApply(rootNode, function(x){xmlSApply(x, xmlValue)})

  if(class(data0) == "list"){
    data_lst <- lapply(data0, FUN = to_df_comp) %>% c(data_lst)
  } else { data_lst <- apply(data0, MARGIN = 2, FUN = to_df_comp) %>% c(data_lst) }
}
result <- rbindlist(data_lst)
result <- setNames(result, sort(vars$var))
return(result)





search_terms <- str_to_lower(search_terms)
data_source <- str_to_lower(data_source)


if(data_source %in% c("nest", "baltic nest")){
  ## baltic nest data portal ----

  for(j in 1:nrow(dates_tab)){
    full_url <- sprintf(
      "%s?latBegin=%s&latEnd=%s&lonBegin=%s&lonEnd=%s&dateBegin=%s&dateEnd=%s",
      "http://nest.su.se/dataPortal/getStations",
      latlon_ranges[1], latlon_ranges[2], latlon_ranges[3], latlon_ranges[4],
      dates_tab[j,1], dates_tab[j,2]
    )
    tmp <- readr::read_csv(full_url)
    if(nrow(tmp) == 0){
      message(sprintf("no rows of data found for %s to %s...",
                      dates_tab[j,1], dates_tab[j,2]))
    }
    result <- rbind(result, tmp)
  }
  closeAllConnections()

} else if(data_source == "smhi"){
  ## smhi open data oceanographical observations ----
  "stuff...."

} else if(data_source == "ices"){
  ## ices data center ----

  ## two different web services offered
  ## http://ecosystemdata.ices.dk/webservices/index.aspx
  ## http://dome.ices.dk/Webservices/index.aspx


  ices_ecosystem_ws <- "http://ecosystemdata.ices.dk/WebServices/EcoSystemWebServices.asmx"
  ices_dome_ws <- "http://dome.ices.dk/Webservices/DOMEWebServices.asmx"

  ## some lookup tables for some of the variable options
  params_df <- xmlParse(paste0(ices_ecosystem_ws, "/getListParameters")) %>%
    xmlToList() %>% lapply(FUN = as.data.frame) %>% rbindlist()
  grps_df <- xmlParse(paste0(ices_ecosystem_ws, "/getListParamGroup")) %>%
    xmlToList() %>% lapply(FUN = as.data.frame) %>% rbindlist()
  datasets_df <- xmlParse(paste0(ices_ecosystem_ws, "/getListDatasets")) %>%
    xmlToList() %>% lapply(FUN = as.data.frame) %>% rbindlist()
  spp_df <- xmlParse(paste0(ices_ecosystem_ws, "/getListSpecies")) %>%
    xmlToList() %>% lapply(FUN = as.data.frame) %>% rbindlist()


  if(length(intersect(c("chlorobiphenyls","pcbs","dioxins","organofluorines","pfos"),
                      str_to_lower(search_terms))) != 0){

    ## use ices 'dome' webservice
    ## for contaminants data from DOME webservice: chlorobiphenyls, dioxins, organofluorines
    ## https://vocab.ices.dk codes: OC-CB Chlorobiphenyls, OC-DX Dioxins, O-FL Organofluorines

    url_base <- paste0(
      ices_dome_ws,
      "/selectContaminantsInBiota?",
      "%s&ParamGroup=&CNTRY=%s&Area=&PARAM=&RLABO=&ALABO=&MATRX=&TAXA=&PURPM=&MPROG=")

    ## Parameter Group
    tmp <- tibble(
      code = c("OC-CB", "OC-DX", "O-FL"),
      key = c("OC%%2DCB", "OC%%2DDX", "O%%2DFL"), # double % because used with sprintf
      grp = c("chlorobiphenyls", "dioxins", "organofluorines"),
      abbrev = c("pcbs", "dioxins", "pfos"))
    url_base <- str_replace(
      url_base, pattern = "ParamGroup=",
      replacement = paste0("ParamGroup=", tmp[tmp$grp %in% search_terms|tmp$abbrev %in% search_terms,"key"]))

    ## Years
    ## need start and end years as inputs
    yrs <- dates_tab %>%
      mutate(start_yr = year(start), end_yr = year(end)) %>%
      filter(start_yr != end_yr) %>%
      mutate(yrs = sprintf("yearBegining=%s&yearEnd=%s", start_yr, end_yr))
    yrs <- yrs$yrs

    ## Output vars
    ## table of all dome web service outputs, from dome.ices.dk/Webservices/index.aspx
    vars <- select(read_csv(file.path(dir_prep, "ref", "lookup_tabs", "dome_ices_vars.csv")), var)

    ## Function to transform list from xml
    to_df <- function(x){
      entry <- unlist(x) %>%
        data.frame(row.names = str_remove(names(.), "DOMErecord.")) %>%
        tibble::rownames_to_column() %>%
        merge(vars, by.x = "rowname", by.y = "var", all = TRUE) %>% # will throw error if merging fails...
        t() %>%
        data.frame(row.names = NULL)
      entry <- entry[2,]
      return(entry)
    }
    to_df_comp <- compiler::cmpfun(to_df)

  } else {

    ## for other data, use ices 'ecosystem' webservice

    url_base <- paste0(
      ices_ecosystem_ws,
      "/getICESDataPortalData?",
      sprintf("minLatitude=%s&maxLatitude=%s&minLongitude=%s&maxLongitude=%s",
              latlon_ranges[1], latlon_ranges[2], latlon_ranges[3], latlon_ranges[4]),
      "&area=&dataset=&datatype=&parametergroup=&parameter=&taxa=&matrix=")

    ws_datasets <- c(
      "oceanographic",
      "contaminants and biological effects",
      "eggs and larvae",
      "historical datasets",
      "vulnerable marine ecosystems",
      "fish trawl survey",
      "biological community",
      "fish predation (stomach contents)")
    if(search_terms %in% ws_datasets){
      key <- datasets_df[str_to_lower(datasets_df$DatasetName) == search_terms, "key"] %>%
        as.character()
      url_base <- str_replace(url_base, pattern = "dataset=", replacement = paste0("dataset=", key))
    }
    if(any(str_detect(grps_df$paramGroupName, search_terms))){
      key <- grps_df[str_detect(grps_df$paramGroupName, search_terms), "key"] %>%
        as.character()
      if(length(key) == 1){
        url_base <- str_replace(url_base, pattern = "parametergroup=", replacement = paste0("parametergroup=", key))
      } else {stop("be more specific with search terms, to locate specific ICES parameter group...")}
    }

    ## takes single year input
    yrs <- seq(year(date_begin), year(date_end))

    vars <- data.frame(var = c(
      "Year","Month","day","Datetime","DayNight",
      "Cruise","Station","Longitude","Latitude","Depth",
      "DataSet","Datatype","Parameter",
      "Value","Precision","Unit","Original Value","Original Unit",
      "Species","Matrix","Depth Class","Age Class","Length Class","sex",
      "DEPHL","DEPHU","QFLAG","BASIS","NOINP",
      "SampleID","MeasurementID",
      "ICES Position Note","ICES DateTime Note")
    )

    to_df <- function(x){
      entry <- unlist(x) %>%
        data.frame(row.names = str_remove(names(.), "DOMErecord.")) %>%
        tibble::rownames_to_column() %>%
        merge(vars, by.x = "rowname", by.y = "var", all = TRUE) %>% # will throw error if merging fails...
        t() %>%
        data.frame(row.names = NULL)
      entry <- entry[2,]
      return(entry)
    }
    to_df_comp <- compiler::cmpfun(to_df)
  }

  data_lst <- list()
  if(str_detect(url_base, pattern = "CNTRY")){
    countrycodes <- c("06","07","RU","67","LT","LA","ES","34","77","26")
    urls <- mapply(expand.grid(yrs, countrycodes)[1],
                   expand.grid(yrs, countrycodes)[2],
                   FUN = function(x, y){sprintf(url_base, x, y)})
  } else {urls <- lapply(yrs, FUN = function(x){sprintf(url_base, x)}) %>% unlist()}
}


cl <- makeCluster(cores)
registerDoParallel(cl)
data_lst <- foreach(i = urls, .packages= c("XML", "magrittr", "stringr"), .combine = c) %dopar% {
  Sys.sleep(delay)

  xmldoc <- xmlParse(i)
  rootNode <- xmlRoot(xmldoc)
  data0 <- xmlSApply(rootNode, function(x){xmlSApply(x, xmlValue)})

  if(class(data0) == "list"){
    data_lst <- lapply(data0, FUN = to_df_comp) %>% c(data_lst)
  } else { data_lst <- apply(data0, MARGIN = 2, FUN = to_df_comp) %>% c(data_lst) }
}
stopCluster(cl)
closeAllConnections()
result <- rbindlist(data_lst)
result <- setNames(result, sort(vars$var))
return(result)


#' searching helcom datasets via metadata catalog
#'
#' @param datasetname
#' @param search_terms a single string (not a character vector) with search terms separated by spaces
#'
#' @return

helcom_mapservice_data <- function(datasetname, search_terms){

  metadata_base <- "http://metadata.helcom.fi/geonetwork/srv/eng/"
  results <- paste0(metadata_base,
                    "catalog.search#/search?facet.q=type%2Fdataset",
                    "&resultType=details&from=1&to=20&sortBy=relevance&fast=index&_content_type=json&any=",
                    gsub(" ", "%20", search_terms))
  datasets_html <- html_nodes(read_html(results), "list-group-item")
  datasets_text <- html_text(datasets_html)


  # splash("localhost") %>%
  #   render_html("http://www.techstars.com/companies/", wait=5) -> pg
  #
  # html_nodes(pg, "table")
  # ## {xml_nodeset (89)}
  # ##  [1]




  identifier <- stringr::str_extract()
  datasetname <- stringr::str_extract()
  sprintf("downloading dataset '%s' with identifier number '%s'", datasetname, identifier)

  name_url <- sprintf("resources.get?uuid=%s&fname=%s.zip&access=public", identifier, datasetname)
  data_url <- paste0(metadata_base, name_url)
  metadata_url <- paste0(metadata_base, "xml.metadata.get?uuid=", identifier)



}


## getting semi customized groupings of harmonized BHI data
get_parcel_data <- function(include_attributes = "all", exclude_raster = FALSE, for_extent = NULL, software = "R"){

  ## want in format where can download, and open in R as a project
  ## ... or load all easily and immediately into a GIS (or ENVI?)

  ## if 'for_extent' polygons are not the same as the baltic regions, intersect them to create a new polygon layer
  ## intersected polygons are the ones we want to link parcel data to

  ## include two additional spatial layers: ocean/land binary vector and raster

  ## get spatial data from the database
  ## process to get to same spatial extent and resolution
  ## rasterize vectors, but also include original vector layers
  ## create attribute table linked with 'for_extent' polygons, incorporating all tabular data


  attr <- sf
  vect <- sf
  rast <- raster::stack()

  return(list(raster_data = rast, vector_data = vect, attribute_data = attr))
}


## BHI REPOS CONFIGURATION ----

## editing scenario data years config file...
cat("remove from scenario_data_years.csv: \n", paste(
  setdiff(
    current_conf_scen_data_yrs$layer_name %>%
      grep(pattern = r, value = TRUE) %>% unique(),
    names(archive_layers$data) %>%
      grep(pattern = r, value = TRUE)),
  collapse = "\n"), "\n\n", sep = "")
cat("add to scenario_data_years.csv: \n", paste(
  setdiff(
    names(archive_layers$data) %>%
      grep(pattern = r,
           value = TRUE),
    current_conf_scen_data_yrs$layer_name %>%
      grep(pattern = r,
           value = TRUE) %>% unique()),
  collapse = "\n"), "\n\n", sep = "")



check_linked_resources <- function(directory, ){

  md_docs <- list.files(directory, pattern = ".*rmd$|.*Rmd$|.*md$", recursive = TRUE, full.names = TRUE)
  pdf_docs <- list.files(directory, pattern = ".*pdf$", recursive = TRUE, full.names = TRUE)
  r_docs <- list.files(directory, pattern = ".*R$|.*r$", recursive = TRUE, full.names = TRUE)

  df_out <- data.frame()
}


## knitting bhi-prep repo documents
knit_prep_docs <- function(prep_dir, assess_year, goal_or_dim = "all"){

  yr <- paste0("v", assess_year)

  conf_doc <- file.path(prep_dir, "conf", yr, "...???")
  data_doc <- paste0(str_to_lower(g), "_data.rmd")
  prep_doc <- file.path(prep_dir, "prep", yr)

  conf_docs_loc <- file.path(prep_dir, "conf", yr)
  data_docs_loc <- file.path(prep_dir, "data", yr)
  prep_docs_loc <- file.path(prep_dir, "prep", yr)


  for(g in goal_or_dim){
    "stuff..."
  }
}


check_linked_resources <- function(directory, ...){

  md_docs <- list.files(directory, pattern = ".*rmd$|.*Rmd$|.*md$", recursive = TRUE, full.names = TRUE)
  pdf_docs <- list.files(directory, pattern = ".*pdf$", recursive = TRUE, full.names = TRUE)
  r_docs <- list.files(directory, pattern = ".*R$|.*r$", recursive = TRUE, full.names = TRUE)

  for(m in md_docs){

  }
  for(p in pdf_docs){

  }
  for(r in r_docs){

  }


  df_out <- data.frame()
}


exploration_script_skeleton <- function(data_exploration_description){
  basic_script <- c("read in data assumed to be of form created by get_data_parcel function",
                    "wrangle some things",
                    "ggplot some things (different types of plots) including with ggplotly",
                    "explore basic possibilities with raster package",
                    "in section near end describe some additional packages that might be of interest")

  testing_rmds <- list.files(file.path(here::here(), "baltic", "testing"),
                             full.names = TRUE,
                             recursive = TRUE) %>%
    grep(pattern = ".*[a-z0-z_].Rmd$", value = TRUE)

  scripts_keys_df <- tibble::tibble(script = basename(testing_rmds),
                                    filepath = testing_rmds,
                                    keywords = "")
  for(nm in testing_rmds){
    k <-
      scripts_keys_df[filepath == nm, keywords] <- as.list(k)
  }

  keywords <- "words from description found in the scripts_keys_df"


  if(length(keywords) < 3){
    print("no analysis in our files closely relates, so suggested script is quite sparse...")
    script_text <- basic_script
  } else {
    script_text <- somthing_more_specific_based_on_matchingness_of_describe_test_rmds
    cat(sprintf("selected basic script template based on words from description: %s",
                paste(keywords, collapse = ", ")))
  }
  print("output is a ?; to use this output, write the object to an RMarkdown document")
  return()
}


rfun_to_gis <- function(fun_loc, fun_name, software = "QGIS", write_loc = NULL){

  tmp <- scan(file = fun_loc, what = "character", sep = "\n", quote = "\"") %>%
    grep(pattern = "\\s*#{1,}.*", value = TRUE, invert = TRUE) %>%
    grep(pattern = "\\s*#{1,}.*", value = TRUE, invert = TRUE) %>%

  sep_indices <- # need to parse functions within same script and get just lines of 'fun_name' function
  req_libraries <- # if called by :: notation or within

  ## many functions cannot be transported and/or made GIS compatible...
  chk <- c("stuff here eg:",
           "filepath dependancy not given as arg, so cannot find",
           "types of data the GIS cannot work with via the R bridge",
           "setting or identifying working directories",
           "returning classes that are not workable within GIS",
           "no implicitly/explicity spatial data- needs at least potential connection to sf geometry or similar...")
  if(length(chk) > 0){
    stop(cat("the following elements make this function non-transportable: ", chk, sep = "\n"))
  }

  if(software == "ArcGIS"){
    script_text <- "makescriptthatlookslikethis using scanned links in object 'tmp'"
  } else {
    if(software != "QGIS"){
      cat("can't create script for that software-- ",
          "creating a script that can be utilized within QGIS")
    }
    script_text <- "makescriptthatlookslikethis using scanned links in object 'tmp'"
  }

  cat(sprintf("helpful resource on integrating the generated script within %s workflows: %s",
              software,
              ifelse(software == "ArcGIS",
                     "\nhttp://amsantac.co/blog/en/2016/04/30/arcgis-r.html",
                     "\nhttps://docs.qgis.org/2.14/en/docs/training_manual/processing/r_intro.html#use-r-scripts-in-processing")))

    if(!is.null(write_loc)){
      output_fun_name <- sprintf("%s_4gis.R", fun_name)

      if(!file.exists(write_loc)){
        print("given write location does not exist; saving output to 'temp' within assessment folder")
        write_loc <- file.path(dir_baltic, "temp") # ...assumes dir_baltic was created from sourcing 'common.R'
      }

      sink(file = file.path(write_loc, output_fun_name))
      cat(script_text, sep = "\n")
      closeAllConnections() # stop the sinking!

      } else { cat(script_text, sep = "\n") }

  return(NULL)
}

## some kind of parameter sensitivity analysis or optimization...??
## don't remember what this was supposed to be...
model_coeffs <- function(functionsR, goals_subgoals){

  functionsR_txt <- "scan in functions.R models"
  coeffs <- list()

  for(g in 1:length(goals_subgoals)){
    tmp <- goal_function(functionsR_txt, goals_subgoals[g])
    coeffs[g] <- "somehow get coefficients used... or maybe this should be modeled, or predefined?"
  }

  model_coeffs_array <- array()
  model_coeffs_mat <- matrix(NA, nrow = length(goals_subgoals), ncol = length(tmp))
  return(model_coeffs_mat)
}


transition_matrix <- function(functionsR, goals_subgoals, next_yr_scores){

  M <- model_coeffs(functionsR, goals_subgoals)
  Minv <- solve(M)
  l <- vector()
  for(g in goals_subgoals){
    l <- c(l, goal_layer(functionsR_txt, g))
  }
  t <- "something"

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

  if(file_type %in% c("raster", "tiff", "geotiff")){
  }
  if(file_type %in% c("csv", "shapefile")){
  }
  if(file_type %in% c("image", "picture", "png", "jpg")){
  } else {
    print("sadly, this function isn't able to tell you anything about that kind of file")
    meaningful_stuff <- NULL
    other_info_lumped <- NULL
  }
  return(list(meaningful_stuff, other_info_lumped))
}
