

# write_to_db <- function(datatable, name_in_db, harmonize_vars, connection){}



#' call goal data from database
#' 
#' this calls the data from the database and saves as a cvs in a specified location
#' this function should be used before preparing a data layer, if the raw data were updated
#'
#' @param goal_code
#' @param assess_year
#' @param datasets 
#' @param save_loc
#' 
#' @return

extract_data <- function(goal_code, assess_year, datasets, save_loc = NA){
  
  if(is.na(save_loc)){
    dir_goal <- file.path(dir_prep, "prep", "", assess_year) 
  } else { dir_goal <- save_loc }
  
  ## access database
  ## make sure data are in database, and put them there if not
  ## import datasets
  
  for(d in datasets){
    readr::read_csv(file.path(dir_goal, "raw", "name dataset..."))
  }
  
}




# loc = "C:/Users/jgrif/Documents/StockholmUnivPostDoc/BalticHealthIndex/BHI_r/shapefiles/BHI_shapefile_projected"
plot_bhi_w_nuts_rgns <- function(shp_loc){
  
  ## read shapefiles from location specified by shapefile location param 'shp_loc'
  bhi_shp <- sf::st_read(shp_loc, "BHI_shapefile_projected.shp") %>% 
    sf::st_transform("+proj=longlat +init=epsg:4326")
  plot(st_geometry(bhi_shp))
  
  bhi_shp_buffer <- sf::st_read(shp_loc, "BHI_shapefile_25km_buffer_projected.shp") %>% 
    sf::st_transform("+proj=longlat +init=epsg:4326")
  plot(st_geometry(bhi_shp_buffer))
  
  nuts2 <- sf::st_read(shp_loc, "NUTS2006_Level_2_reprojected.shp") %>% 
    sf::st_transform("+proj=longlat +init=epsg:4326")
  plot(st_geometry(nuts2))
  
  nuts3 <- sf::st_read(shp_loc, "NUTS2006_Level_3_reprojected.shp") %>% 
    sf::st_transform("+proj=longlat +init=epsg:4326")
  plot(st_geometry(nuts3))
  
  
  
  
}


##---------------------------------------##
## PLOT BHI REGIONS and NUTS3 REGIONS ##
##---------------------------------------##

## get centroids of polygons for plotting labels
centroids_bhi <- getSpPPolygonsLabptSlots(BHIshp2)%>% data.frame()%>%
  dplyr::rename(lon=X1, lat=X2)%>%
  mutate(BHI_ID = BHIshp2@data$BHI_ID)

centroids_nuts3 =getSpPPolygonsLabptSlots(NUTS3_2)%>% data.frame()%>%
  dplyr::rename(lon=X1, lat=X2)%>%
  mutate(NUTS3_ID = NUTS3_2@data$NUTS_ID)

## plot NUTS3 and shapefile with buffer
png(file.path(dir_prep,"BHI_regions_NUTS3_withbuffer_plot.png"),
    width = 210, height=297, units="mm",res=300 )
plot(BHIshp_buffer2, col="red")
plot(BHIshp2, add=TRUE)
plot(NUTS3_2, add=TRUE)
text(centroids_nuts3$lon,centroids_nuts3$lat, labels=centroids_nuts3$NUTS3_ID, col="black",cex=.3)
text(centroids_bhi$lon, centroids_bhi$lat, labels=centroids_bhi$BHI_ID, col="red",cex=.5)
dev.off()

## plot NUTS3 and shapefile without buffer
png(file.path(dir_prep,"BHI_regions_NUTS3_plot.png"),
    width = 210, height=297, units="mm",res=300 )
plot(BHIshp2,col="red")
plot(NUTS3_2, add=TRUE)
text(centroids_nuts3$lon,centroids_nuts3$lat, labels=centroids_nuts3$NUTS3_ID, col="black",cex=.3)
text(centroids_bhi$lon, centroids_bhi$lat, labels=centroids_bhi$BHI_ID, col="black",cex=.5)
dev.off()


##---------------------------------------##
## PLOT BHI REGIONS and NUTS2 REGIONS ##
##---------------------------------------##

## get centroids of polygons for plotting labels
centroids_nuts2 =getSpPPolygonsLabptSlots(NUTS2_2)%>% data.frame()%>%
  dplyr::rename(lon=X1, lat=X2)%>%
  mutate(NUTS2_ID = NUTS2_2@data$NUTS_ID)

## plot NUTS2 and shapefile with buffer
png(file.path(dir_prep,"BHI_regions_NUTS2_withbuffer_plot.png"),
    width = 210, height=297, units="mm",res=300 )
plot(BHIshp_buffer2, col="red")
plot(BHIshp2, add=TRUE)
plot(NUTS2_2, add=TRUE)
text(centroids_nuts2$lon,centroids_nuts2$lat, labels=centroids_nuts2$NUTS2_ID, col="black",cex=.3)
text(centroids_bhi$lon, centroids_bhi$lat, labels=centroids_bhi$BHI_ID, col="red",cex=.5)
dev.off()

## plot NUTS2 and shapefile without buffer
png(file.path(dir_prep,"BHI_regions_NUTS2_plot.png"),
    width = 210, height=297, units="mm",res=300 )
plot(BHIshp2,col="red")
plot(NUTS2_2, add=TRUE)
text(centroids_nuts2$lon,centroids_nuts2$lat, labels=centroids_nuts2$NUTS2_ID, col="black",cex=.3)
text(centroids_bhi$lon, centroids_bhi$lat, labels=centroids_bhi$BHI_ID, col="black",cex=.5)
dev.off()


####----


check_linked_resources <- function(directory, ){

  md_docs <- list.files(directory, pattern = ".*rmd$|.*Rmd$|.*md$", recursive = TRUE, full.names = TRUE)
  pdf_docs <- list.files(directory, pattern = ".*pdf$", recursive = TRUE, full.names = TRUE)
  r_docs <- list.files(directory, pattern = ".*R$|.*r$", recursive = TRUE, full.names = TRUE)




  df_out <- data.frame()
}


#' Title
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

## --------


knit_prep_docs <- function(prep_dir, assess_year, goal_or_dim = "all"){

  yr <- paste0("v", assess_year)

  conf_doc <- file.path(prep_dir, "conf", yr, )
  data_doc <- paste0(str_to_lower(g), "_data.rmd")
  prep_doc <- file.path(prep_dir, "prep", yr)

  conf_docs_loc <- file.path(prep_dir, "conf", yr)
  data_docs_loc <- file.path(prep_dir, "data", yr)
  prep_docs_loc <- file.path(prep_dir, "prep", yr)


  for(g in goal_or_dim){


  }



}



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



check_linked_resources <- function(directory, ){

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
