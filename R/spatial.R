## Libraries
source(file.path(here::here(), "R", "common.R"))
library(rgdal)
library(raster)
library(fasterize) # install.packages("fasterize)
library(sf)
library(RQGIS)
library(sp)
library(tibble)
library(dplyr)

## Functions

#' create region lookup table
#'
#' @param dir_bhi file path to root bhi directory
#' @param layers_object ohicore layers object, best if these are specified
#' @param conf_object ohicore
#'
#' @return no immediate output; writes

create_rgn_lookup <- function(dir_bhi, layers_object = NULL, conf_object = NULL){
  ## from create_rgns_complete.R by @jules32 Sept 14 2016
  ## find original script in prep folder of bhi-1.0-archive github repo

  ## creates lookup/regions_lookup_complete.csv used by FinalizeScores in functions.R
  if(file.exists(file.path(dir_bhi, "spatial", "regions_lookup_complete_wide.csv"))){
    print("overwriting regions_lookup_complete_wide.csv with new version")
  }
  if(file.exists(file.path(dir_bhi, "spatial", "regions_lookup_complete.csv"))){
    print("overwriting regions_lookup_complete.csv with new version")
  }

  ## lookup table for EEZ ids
  if(file.exists(file.path(dir_bhi, "spatial", "eez_lookup.csv"))){
    eez_lookup <- read_csv(file.path(dir_bhi, "spatial", "eez_lookup.csv"))
  } else {
    eez_lookup <- c("SWE" = 1, "DNK" = 2, "DEU" = 3, "POL" = 4,
                    "RUS" = 5, "LTU" = 6, "LVA" = 7, "EST" = 8, "FIN" = 9)
    write_csv(data.frame(eez = eez_lookup, rgn_key = names(eez_lookup)),
              file.path(dir_bhi, "spatial", "eez_lookup.csv"))
    print("eez_lookup.csv created by spatial.R 'create_rgn_lookup' function, saved to 'spatial' folder")
  }

  if(is.null(layers_object)|is.null(conf_object)){
    rgn_labels <- read_csv(file.path(dir_bhi, "spatial", "rgn_labels.csv")) %>%
      select(region_id = rgn_id, region_name = label)
  } else {
    rgn_labels <- ohicore::SelectLayersData(
      layers_object,
      layers = conf_object$config$layer_region_labels) %>% # rgn_labels.csv per conf/config.R specs
      dplyr::select(region_id = id_num, # id_num and val_char are standard colnames once ohicore configures via 'conf' and 'layers'
                    region_name = val_chr)
  }
  ## join region labels with labels for EEZs and HELCOM Subbasins
  rgns <- left_join(rgn_labels,
    read.csv(file.path(dir_bhi, "spatial", "bhi_basin_country_lookup.csv")) %>%
      dplyr::rename(region_id = BHI_ID,
                    eez_name = rgn_nam,
                    subbasin_name = Subbasin,
                    area_km2_rgn = Area_km2) %>%
      ## for HELCOM sub-basin areas and for EEZ areas, create numeric ids and calculate areas
      dplyr::mutate(subbasin_id = as.integer(stringr::str_replace_all(HELCOM_ID, "SEA-0", "5"))) %>%
      dplyr::rowwise() %>% # have to do eez_lookup mutate rowwise!
      dplyr::mutate(eez_id = as.integer(
        stringr::str_c("30", eez_lookup$eez[which(eez_lookup$rgn_key == rgn_key)])
        )) %>%
      ungroup(),
    by = "region_id") %>% # join by region_id and select
    dplyr::select(region_id, region_name, area_km2_rgn, eez_id, eez_name, subbasin_id, subbasin_name) # View(rgns)

  ## create csv lookup of all regions with headers to match layers/rgn_labels.csv
  ## region_id 1-42 correspond to BHI regions, region_id 301-309 to EEZs, region_id 501-517 to Subbasins, 0 to global
  rgns_complete <- bind_rows(
    tibble::tibble(region_id = 0, label = "Baltic", type = "GLOBAL"),
    rgns %>%
      dplyr::mutate(label = as.character(region_name), type = "bhi") %>%
      dplyr::select(region_id = region_id, label, type),
    rgns %>%
      dplyr::distinct(eez_id, eez_name) %>% # EEZ ids
      dplyr::mutate(type = "eez", label = as.character(eez_name)) %>%
      dplyr::select(region_id = eez_id, label, type),
    rgns %>%
      dplyr::distinct(subbasin_id, subbasin_name) %>% # SUBBASIN ids
      dplyr::mutate(type = "subbasin", label = as.character(subbasin_name)) %>%
      dplyr::select(region_id = subbasin_id, label, type)) # View(rgns_complete)

  ## save lookups
  write.csv(rgns,
            file.path(dir_bhi, "spatial",
                      "regions_lookup_complete_wide.csv"),
            row.names = FALSE)
  write.csv(rgns_complete,
            file.path(dir_bhi, "spatial",
                      "regions_lookup_complete.csv"),
            row.names = FALSE)
}


#' create BHI regions shape object
#'
#' @return defines a spatial shape objects named 'regions_sp' and 'baltic_mpas' in the global environment

regions_shape <- function(rgn_shp_foldername = "bhi_shapefile", mpas_shp_foldername = "mpas_shapefile"){

  cat("defining in global environment: spatial shape objects named 'regions_sp' and 'baltic_mpas'\n")

  rgns_sp_path <- file.path(dir_B, "Spatial", rgn_shp_foldername)
  mpas_sp_path <- file.path(dir_B, "Spatial", mpas_shp_foldername)

  if(!file.exists(rgns_sp_path) | !file.exists(mpas_sp_path)){
    sprintf("folders %s and/or %s not found", rgns_sp_path, mpas_sp_path)
  }

  rgns_sp <-list.files(rgns_sp_path,
                       full.names = TRUE,
                       pattern = "[A-Za-z0-9_]+.shp$") # assumes best practice of one shapefile per folder
  if(!file.exists(rgns_sp)){
    sprintf("the BHI regions shapefile '%s' does not exist at location %s", basename(rgns_sp), rgns_sp_path)
  } else {
    regions_sp <<- sf::st_read(dsn = rgns_sp_path, layer = rgn_shp_foldername)
  }

  mpas_sp <- list.files(mpas_sp_path,
                        full.names = TRUE,
                        pattern = "[A-Za-z0-9_]+.shp$")
  if(!file.exists(mpas_sp)){
    sprintf("Baltic MPAs shapefile '%s' does not exist at location %s", basename(mpas_sp), mpas_sp_path)
  } else {
    baltic_mpas <<- sf::st_read(dsn = mpas_sp_path, layer = mpas_shp_foldername)
  }
}


#' load ocean mask, zones, and mpa rasters
#'
#' @return loads and defines three raster objects in the global environment

bhi_rasters <- function(zones = TRUE, ocean = TRUE, mpas = TRUE){
  cat("loads 3 rasters unless otherwise specified: zones, ocean, and mpas\n",
      "zones = raster cells with BHI region ID values, see rgn_labels.csv to link IDs with names\n",
      "ocean = raster with ocean cells identified as 1, otherwise 0\n",
      "mpas  = raster with Baltic marine protected areas identified as 1, elsewhere 0\n\n")

  tiffs <- c("baltic_zones.tif", "baltic_ocean.tif", "baltic_mpas.tif")
  for(i in tiffs){
    path <- file.path(dir_B, "Spatial", i)
    if(!file.exists(path)){
      sprintf("the file '%s' was not found at location '%s'",
              i, file.path(dir_B, "Spatial"))
      sprintf("check that BHI server '%s' is mounted, and that files are correctly named...", dir_B)
    }
  }
  ## bhi regions rasterized to match ocean
  zones <<- raster::raster(file.path(dir_B, "Spatial", "baltic_zones.tif"))
  ## an ocean raster for masking spatial raster data
  ocean <<- raster::raster(file.path(dir_B, "Spatial", "baltic_ocean.tif"))
  ## baltic MPAs from specified year rasterized to match ocean and zone
  mpas <<- raster::raster(file.path(dir_B, "Spatial", "baltic_mpas.tif"))
}
