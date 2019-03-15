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

## Directories
dir_bhi <- here::here()
dir_spatial <- file.path(dir_bhi, "spatial") # spatial folder of bhi repo

## Additional...
source(file.path(dir_bhi, "R", "common.R"))
projstringCRS <- raster::crs("+proj=longlat +datum=WGS84 +no_defs") # BHI spatial data-- use lat/long coords on WGS84

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
#' @return defines a spatial shape object named 'regions_sp' in the global environment

regions_shape <- function(){

  cat("defining in the global environment a spatial shape object named 'regions_sp'\n")
  regions_sp_path <- file.path(dir_B, "Spatial", "bhi_shapefile")

  if(!file.exists(file.path(regions_sp_path, "bhi_shapefile.shp"))){
    sprintf("BHI regions shapefile does not exist at location %s", regions_sp_path)
  } else {
    regions_sp <<- sf::st_read(dsn = regions_sp_path, layer = "bhi_shapefile")
  }
}


#' load ocean mask and zones rasters
#'
#' @return loads and defines two raster objects into the global environment

bhi_rasters <- function(){
  cat("loads 2 rasters: zones and ocean\n",
      "zones = raster cells with BHI region ID values, see rgn_labels.csv to link IDs with names\n",
      "ocean = raster with ocean cells identified as 1, otherwise 0\n\n")

  zones <<- raster::raster(file.path(dir_B, "Spatial", "rgns_zones.tif"))

  ## an ocean raster for masking spatial raster data
  ocean <<- raster::raster(file.path(dir_B, "Spatial", "rgns_ocean.tif"))

}
