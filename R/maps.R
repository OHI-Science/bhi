## Libraries
library(ggplot2)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(ggspatial)
library(sf)


## Functions

#' make sf obj with subbasin-aggregated goal scores
#'
#' @param subbasins_shp a shapefile read into R as a sf (simple features) object;
#' must have an attribute column with subbasin full names
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param dim the dimension the object/plot should represent,
#' typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'
#' @param year the scenario year to filter the data to, by default the current assessment yearr
#' @param simplify_level number of times rmapshaper::ms_simplify function should be used on the shapefile,
#' to simplify polygons for display
#'
#' @return sf obj with subbasin-aggregated goal scores

make_subbasin_sf <- function(subbasins_shp, scores_csv,
                             goal_code = "all", dim = "score", year = assess_year,
                             simplify_level = 1){

  ## raster::select conflict w dplyr...
  if("raster" %in% (.packages())){
    detach("package:raster",  unload = TRUE)
    library(tidyverse)
  }
  if("year" %in% colnames(scores_csv)){
    scores_csv <- scores_csv %>%
      dplyr::filter(year == year) %>%
      select(-year)
  } else {
    message("no year column in given scores_csv, assuming it has been properly filtered by year")
  }

  ## aggregate data by subbasin
  # subbasin_data <- scores_csv %>%
  #   dplyr::filter(region_id < 100 & region_id != 0) %>%
  #   dplyr::collect() %>%
  #   dplyr::left_join(tbl(bhi_db_con, "regions") %>%
  #                      select(region_id, subbasin, area_km2) %>%
  #                      collect(),
  #             by = "region_id") %>%
  #   dplyr::filter(!is.na(subbasin)) %>%
  #   dplyr::group_by(goal, dimension, subbasin) %>%
  #   dplyr::summarise(score = weighted.mean(score, area_km2, na.rm = TRUE)) %>%
  #   dplyr::ungroup()
  subbasin_data <- scores_csv %>%
    dplyr::filter(region_id >= 500) %>%
    dplyr::collect() %>%
    dplyr::left_join(tbl(bhi_db_con, "basins") %>%
                       select(region_id = subbasin_id, subbasin, area_km2) %>%
                       collect(),
                     by = "region_id") %>%
    dplyr::filter(!is.na(subbasin))

  ## filter and spread data by goal
  mapping_data <- subbasin_data %>%
    dplyr::filter(dimension == dim) %>%
    dplyr::select(score, goal, Name = subbasin) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::filter(!is.na(Name)) %>%
    dplyr::mutate(dimension = dim, simplified = simplify_level)

  if(goal_code != "all" & goal_code %in% colnames(mapping_data)){
    mapping_data <- mapping_data %>%
      select(Name, goal_code, dimension, simplified)
  }

  ## simplify the polygons for plotting
  if(simplify_level >= 1){
    subbasins_shp <- rmapshaper::ms_simplify(input = subbasins_shp) %>%
      sf::st_as_sf()
  }
  if(simplify_level == 2){
    subbasins_shp <- rmapshaper::ms_simplify(input = subbasins_shp) %>%
      sf::st_as_sf()
  }

  ## join with spatial information from subbasin shapfile
  mapping_data_sp <- subbasins_shp %>%
    dplyr::mutate(Name = as.character(Name)) %>%
    dplyr::mutate(Name = ifelse(
      Name == "Åland Sea",
      "Aland Sea", Name)) %>%
    dplyr::left_join(mapping_data, by = "Name") %>%
    sf::st_transform(crs = 4326)

  return(mapping_data_sp)
}

#' make bhi-regiomns sf obj joined with goal scores
#'
#' @param bhi_rgns_shp a shapefile of the BHI regions, as a sf (simple features) object
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param dim the dimension the object/plot should represent,
#' typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'
#' @param year the scenario year to filter the data to, by default the current assessment yearr
#' @param simplify_level number of times rmapshaper::ms_simplify function should be used on the shapefile,
#' to simplify polygons for display
#'
#' @return bhi-regions sf obj joined with goal scores

make_rgn_sf <- function(bhi_rgns_shp, scores_csv,
                        goal_code = "all", dim = "score", year = assess_year,
                        simplify_level = 1){

  # bhi_rgns_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile", "BHI_shapefile") %>%
  #   dplyr::mutate(Subbasin = as.character(Subbasin)) %>%
  #   dplyr::mutate(Subbasin = ifelse(Subbasin == "Bothian Sea", "Bothnian Sea", Subbasin)) # NEED TO FIX THIS TYPO!!!!!!!!

  rgn_lookup <- tbl(bhi_db_con, "regions") %>%
    select(region_id, Name = region_name) %>%
    collect()

  if("year" %in% colnames(scores_csv)){
    scores_csv <- scores_csv %>%
      dplyr::filter(year == year) %>%
      select(-year)
  } else {
    message("no year column in given scores_csv, assuming it has been properly filtered by year")
  }

  ## wrangle/reshape and join with spatial info to make sf for plotting
  mapping_data <- scores_csv %>%
    dplyr::filter(dimension == dim, region_id %in% rgn_lookup$region_id) %>%
    collect() %>%
    dplyr::left_join(rgn_lookup, by = "region_id") %>%
    dplyr::select(-dimension) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(dimension = dim, simplified = simplify_level)

  if(goal_code != "all" & goal_code %in% names(mapping_data)){
    mapping_data <- mapping_data %>%
      select(Name, goal_code, dimension, simplified)
  }

  ## simplify the polygons for plotting
  if(simplify_level >= 1){
    bhi_rgns_shp <- rmapshaper::ms_simplify(input = bhi_rgns_shp) %>%
      sf::st_as_sf()
  }
  if(simplify_level == 2){
    bhi_rgns_shp <- rmapshaper::ms_simplify(input = bhi_rgns_shp) %>%
      sf::st_as_sf()
  }

  ## join with spatial information from subbasin shapfile
  mapping_data_sp <- bhi_rgns_shp %>%
    dplyr::mutate(Name = sprintf("%s, %s", Subbasin, rgn_nam)) %>%
    dplyr::left_join(mapping_data, by = "Name")

  return(mapping_data_sp)
}


#' create standardized map
#'
#' a general function used to create maps by subbasin or by BHI region
#' calls apply_bhi_theme function defined in common.R for standardized plotting elements e.g. colors and palettes
#'
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param basins_or_rgns one of 'subbasins' or 'regions' to indicate which spatial units should be represented
#' @param shp spatial features object with geometry of either subbasins or BHI regions depending on specified spatial unit
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param dim the dimension the object/plot should represent,
#' typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'
#' @param year the scenario year to filter the data to, by default the current assessment yearr
#'
#' @return returns map created, a ggplot object

map_general <- function(goal_code, basins_or_rgns = "subbasins", shp, scores_csv, dim = "score", year = assess_year){

  ## checks, wrangling ----
  bhi_goals <- collect(tbl(bhi_db_con, "scores2015"))$goal %>% unique()
  if(length(goal_code) > 1|!goal_code %in% bhi_goals){
    stop(sprintf("goal must be one of: %s", paste(bhi_goals, collapse = ", ")))
  }
  if(is.null(shp)|is.null(scores_csv)){
    stop("missing spatial information: must provide sf with goal scores,
         or raw shapefile and scores.csv")
  }
  if(basins_or_rgns == "subbasins"){
    mapping_data_sp <- make_subbasin_sf(shp, scores_csv, goal_code = "all", dim, year, 1)
  } else {
    message("creating map for BHI regions rather than subbasins")
    mapping_data_sp <- make_rgn_sf(shp, scores_csv, goal_code = "all", dim, year, 1)
  }

  ## set up and make map ----
  ## background map of baltic countries
  if(!exists("baltic")){
    baltic <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      dplyr::select(name, iso_a2) %>%
      st_crop(xmin = -5, xmax = 35, ymin = 48, ymax = 70)
  }

  ## create the map using ggplot geom_sf ----
  plot_map <- ggplot(data = baltic) +
    ## baltic countries
    geom_sf(
      fill = "white",
      alpha = 0.8,
      color = thm$cols$map_polygon_border2,
      size = 0.15
    ) +
    ## overlay goal scores by subbasin with custom continuous color palette
    geom_sf(
      data = mapping_data_sp,
      aes_string(fill = goal_code),
      color = thm$cols$map_polygon_border1,
      size = 0.1,
      alpha = 0.95
    ) +
    ## color palette
    scale_fill_gradientn(
      colours = thm$palettes$divergent_red_blue,
      breaks = c(15, 40, 60, 75, 90, 99),
      limits = c(0, 100),
      na.value = thm$cols$map_background1
    ) +
    ## some formatting and map elements ----
  coord_sf(xlim = c(4.5, 32.5), ylim = c(53.5, 66), expand = FALSE) +
    ## legend formatting
    guides(
      fill = guide_colorbar(
        barheight = unit(2.4, "in"),
        frame.colour = "black",
        ticks.colour = "black",
        ticks.linewidth = 0.5
      )
    ) +
    ## scalebar
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("grey40", "white"),
      height = unit(0.1, "cm"),
      width_hint = 0.3,
      text_cex = 0.5
    ) +
    ## north arrow
    ggspatial::annotation_north_arrow(
      location = "br",
      which_north = "true",
      pad_y = unit(0.25, "in"),
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      style = north_arrow_fancy_orienteering(
        fill = c("white", "grey40"),
        text_col = "grey40"
      )
    ) +
    ## subbasin labels can uses either ggrepel or regular geom_text
    ggrepel::geom_text_repel(
    # geom_text(
      data = cbind(mapping_data_sp, st_coordinates(st_centroid(mapping_data_sp))),
      aes(X, Y, label = Name),
      size = 3.4,
      color = "#8b98a6"
    ) +
    ## labels and assorted plot elements
    labs(x = element_blank(), y = element_blank()) + # no x or y axes labels
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "#d0d7e7"),
      # panel.grid.major = element_line(color = gray(0.5), size = 0.5),
      legend.position = "none", # c(0.08, 0.75)
      legend.title = element_blank(),
      # legend.background = element_rect(fill = "aliceblue"),
      legend.spacing.x = unit(0.24, "cm"),
      legend.key.width = unit(0.3, "cm")
    )

  ## return result ----
  return(invisible(plot_map))
}
