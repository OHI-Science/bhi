## Libraries
source(file.path(here::here(), "R", "common.R"))
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(ggspatial)
library(sf)


## Functions

#' create standardized map, general function
#'
#' a general function used to create maps by subbasin or by  BHI region
#' calls apply_bhi_theme function defined in common.R for standardized plotting elements e.g. colors and palettes
#'
#' @param mapping_data_sp an sf object associating goal scores with spatial polygons/geometries
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param legend boolean indicating whether or not to include plot legend
#' @param labels boolean indicating whether to include labels-- either subbasin or BHI region names
#' @param scalebar boolean indicating whether or not to include a scalebar
#' @param northarrow boolean indicating whether or not to include a northarrow
#'
#' @return returns map created, a ggplot object

map_general <- function(mapping_data_sp, goal_code,
                        legend = TRUE, labels = FALSE,
                        scalebar = FALSE, northarrow = FALSE){

  ## background map of baltic countries
  if(!exists("baltic")){
    baltic <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      st_crop(xmin = -5, xmax = 35, ymin = 48, ymax = 70)
  }

  ## apply theme to get standardized elements, colors, palettes
  thm <- apply_bhi_theme()

  ## create the map using ggplot geom_sf
  plot_map <- ggplot(data = baltic) +
    ## baltic countries
    geom_sf(fill = thm$cols$map_background2,
            alpha = 0.6,
            color = thm$cols$map_polygon_border2,
            size = 0.1) +
    ## overlay goal scores by subbasin with custom continuous color palette
    geom_sf(data = mapping_data_sp,
            aes_string(fill = goal_code),
            color = thm$cols$map_polygon_border1,
            size = 0.1) +
    ## some formatting and map elements
    scale_fill_gradientn(colours = thm$palettes$divergent_red_blue,
                         breaks = c(15, 40, 60, 75, 90, 99), limits = c(0, 100),
                         na.value = thm$cols$map_background1) +
    coord_sf(xlim = c(5, 32), ylim = c(53, 67), expand = FALSE) +
    guides(fill = guide_colorbar(barheight = unit(2.4, "in"), # legend formatting
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 ticks.linewidth = 0.5))

  ## additional optional map elements
  if(scalebar){
    plot_map <- plot_map +
      ggspatial::annotation_scale(location = "br", width_hint = 0.3) # scalebar
  }
  if(northarrow){
    plot_map <- plot_map +
      ggspatial::annotation_north_arrow( # north arrow
        location = "br", which_north = "true", pad_y = unit(0.25, "in"),
        style = north_arrow_fancy_orienteering)
  }
  if(basin_labels){
    plot_map <- plot_map +
      ## subbasin labels either with ggrepel or regular geom_text
      ggrepel::geom_text_repel(data = mapping_data_sp,
                               aes(label = Name, geometry = geometry),
                               stat = "sf_coordinates",
                               size = 3.25)
  }
  if(legend){legend_pos <- c(0.08, 0.75)} else {legend_pos <- "none"}

  plot_map <- plot_map +
    labs(x = element_blank(), y = element_blank()) + # no x or y axes labels
    theme(panel.background = element_rect(fill = "#ddedfd"),
          panel.grid.major = element_line(color = gray(0.5), size = 0.5),
          legend.position = legend_pos, # for no legend use "none"
          legend.title = element_blank(),
          legend.background = element_rect(fill = "aliceblue"),
          legend.spacing.x = unit(0.24, "cm"))

  return(invisible(plot_map))
}


#' make sf obj with subbasin-aggregated goal scores
#'
#' @param subbasins_shp a shapefile read into R as a sf (simple features) object;
#' must have an attribute column with subbasin full names
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param basin_lookup
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param dim
#' @param simplified
#'
#' @return

make_subbasin_sf <- function(subbasins_shp, scores_csv, basin_lookup,
                             goal_code = "all", dim = "score", year = assess_year,
                             simplify_level = 1){

  if("year" %in% names(scores_csv)){
    scores_csv <- scores_csv %>%
      dplyr::filter(year == year) %>%
      select(-year)
  } else {
    message("no year column in given scores_csv, assuming it has been properly filtered by year")
  }

  ## aggregate data by subbasin
  subbasin_data <- scores_csv %>%
    # filter(region_id >= 500) %>% # need subbasin name code look ups table...
    dplyr::left_join(select(basin_lookup, BHI_ID, Subbasin, Area_km2),
              by = c("region_id" = "BHI_ID")) %>%
    dplyr::filter(!is.na(Subbasin)) %>%
    dplyr::group_by(goal, dimension, Subbasin) %>%
    dplyr::summarise(basin_mean = weighted.mean(score, Area_km2, na.rm = TRUE)) %>%
    dplyr::ungroup()

  ## filter and spread data by goal
  mapping_data <- subbasin_data %>%
    dplyr::filter(dimension == dim) %>%
    dplyr::select(basin_mean, goal, Subbasin) %>%
    tidyr::spread(key = goal, value = basin_mean) %>%
    dplyr::filter(!is.na(Subbasin)) %>%
    dplyr::mutate(dimension = dim, simplified = simplify_level)

  if(goal_code != "all" & goal_code %in% names(mapping_data)){
    mapping_data <- mapping_data %>%
      select(Subbasin, goal_code, dimension, simplified) %>%
      rename(scores = goal_code)
  }

  ## join with spatial information from subbasin shapfile
  mapping_data_sp <- subbasins_shp %>%
    dplyr::left_join(mapping_data, by = c("Name" = "Subbasin")) %>%
    sf::st_transform(crs = 4326)

  ## simplify the polygons for plotting
  if(simplify_level >= 1){
    mapping_data_sp <- rmapshaper::ms_simplify(input = mapping_data_sp) %>%
      sf::st_as_sf()
  }
  if(simplify_level == 2){
    mapping_data_sp <- rmapshaper::ms_simplify(input = mapping_data_sp) %>%
      sf::st_as_sf()
  }

  return(mapping_data_sp)
}


#' create subbasin level maps
#'
#' @param subbasins_shp
#' @param scores_csv
#' @param basin_lookup
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param dim
#' @param simplify_level
#' @param save_map
#' @param legend
#' @param basin_labels
#' @param scalebar
#' @param northarrow
#'
#' @return

subbasin_goal_map <- function(subbasins_shp, scores_csv, basin_lookup, goal_code, dim = "score", year = assess_year,
                              simplify_level = 1, save_map = FALSE,
                              legend = TRUE, basin_labels = FALSE, scalebar = FALSE, northarrow = FALSE){


  ## create or recreate subbasin goals sf object if needed
  if(!exists("mapping_data_sp")){
    mapping_data_sp <- make_subbasin_sf(subbasins_shp, scores_csv, basin_lookup,
                                        goal_code, dim, year, simplify_level)
  }

  chk1 <- !goal_code %in% names(mapping_data_sp)
  chk2 <- mapping_data_sp$dimension[1] != dim
  chk3 <- mapping_data_sp$simplified[1] != simplify_level
  if(chk1|chk2|chk3){
    mapping_data_sp <- make_subbasin_sf(subbasins_shp, scores_csv, basin_lookup,
                                        "all", dim, year, simplify_level)
  }

  ## map_general function above, used in both subbasin and bhi-region level maps
  plot_map <- map_general(mapping_data_sp, goal_code, legend,
                          labels = basin_labels, scalebar, northarrow)

  ## save the map
  save_loc <- NULL
  if(is.character(save_map)){
    if(file.exists(save_map)){
      save_loc <- file.path(save_map, sprintf("scores_%s_map.png", goal_code))
    }
  }
  if(isTRUE(save_map)){
    save_loc <- file.path(dir_assess, "reports", "basin_maps",
                          sprintf("scores_%s_map.png", goal_code))
  }
  if(!is.null(save_loc)){
    ggplot2::ggsave(save_loc, plot_map, device = "png",
                    width = 7, height = 6.5, dpi = "retina", units ="in")
  }

  return(invisible(plot_map))
}


make_rgn_sf <- function(bhi_rgns_shp, scores_csv, rgn_lookup,
                        goal_code = "all", dim = "score", year = assess_year,
                        simplify_level = 1){

  ## bhi_rgns_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile", "BHI_shapefile")
  ## rgn_lookup <- readr::read_csv(file.path(here::here(), "spatial", "regions_lookup_complete_wide.csv"))

  if("year" %in% names(scores_csv)){
    scores_csv <- scores_csv %>%
      dplyr::filter(year == year) %>%
      select(-year)
  } else {
    message("no year column in given scores_csv, assuming it has been properly filtered by year")
  }

  ## wrangle/reshape and join with spatial info to make sf for plotting
  mapping_data <- scores_csv %>%
    dplyr::filter(dimension == dim, region_id %in% rgn_lookup$region_id) %>%
    dplyr::left_join(select(rgn_lookup, region_id, region_name, area_km2_rgn),
                     by = "region_id") %>%
    dplyr::mutate(label_txt = region_name) %>%
    dplyr::select(region_id, goal, score, label_txt) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(dimension = dim, simplified = simplify_level)

  if(goal_code != "all" & goal_code %in% names(mapping_data)){
    mapping_data <- mapping_data %>%
      select(label_txt, region_id, goal_code, dimension, simplified) %>%
      rename(scores = goal_code)
  }

  ## join with spatial information from subbasin shapfile
  mapping_data_sp <- bhi_rgns_shp %>%
    dplyr::left_join(mapping_data, by = c("BHI_ID" = "region_id"))

  ## simplify the polygons for plotting
  if(simplify_level >= 1){
    mapping_data_sp <- rmapshaper::ms_simplify(input = mapping_data_sp) %>%
      sf::st_as_sf()
  }
  if(simplify_level == 2){
    mapping_data_sp <- rmapshaper::ms_simplify(input = mapping_data_sp) %>%
      sf::st_as_sf()
  }

  return(mapping_data_sp)
}


#' create BHI region level maps
#'
#' @param bhi_rgns_shp
#' @param scores_csv
#' @param rgn_lookup
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param dim
#' @param simplify_level
#' @param save_map
#' @param legend
#' @param rgn_labels
#' @param scalebar
#' @param northarrow
#'
#' @return

rgn_goal_map <- function(bhi_rgns_shp, scores_csv, rgn_lookup, goal_code, dim = "score", year = assess_year,
                         simplify_level = 1, save_map = FALSE,
                         legend = TRUE, rgn_labels = FALSE, scalebar = FALSE, northarrow = FALSE){


  ## create or recreate bhi-region-with-goals sf object as needed
  if(!exists("mapping_data_sp")){
    mapping_data_sp <- make_rgn_sf(bhi_rgns_shp, scores_csv, rgn_lookup,
                                   goal_code, dim, year, simplify_level)
  }
  chk1 <- !goal_code %in% names(mapping_data_sp)
  chk2 <- mapping_data_sp$dimension[1] != dim
  chk3 <- mapping_data_sp$simplified[1] != simplify_level
  if(chk1|chk2|chk3){
    mapping_data_sp <- make_rgn_sf(bhi_rgns_shp, scores_csv, rgn_lookup,
                                   "all", dim, year, simplify_level)
  }


  ## background map of baltic countries
  if(!exists("baltic")){
    baltic <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      st_crop(xmin = -5, xmax = 35, ymin = 48, ymax = 70)
  }

  ## map_general function above, used in both subbasin and bhi-region level maps
  plot_map <- map_general(mapping_data_sp, goal_code, legend,
                          labels = rgn_labels, scalebar, northarrow)


  ## save the map
  save_loc <- NULL
  if(is.character(save_map)){
    if(file.exists(save_map)){
      save_loc <- file.path(save_map, sprintf("scores_%s_map.png", goal_code))
    }
  }
  if(isTRUE(save_map)){
    save_loc <- file.path(dir_assess, "reports", "bhi_maps",
                          sprintf("scores_%s_map.png", goal_code))
  }
  if(!is.null(save_loc)){
    ggplot2::ggsave(save_loc, plot_map, device = "png",
                    width = 7, height = 6.5, dpi = "retina", units ="in")
  }

  return(invisible(plot_map))
}



#' create leaflet map
#'
#' @param scores_csv
#' @param goal_code
#' @param dim
#' @param year
#' @param basin_or_rgns
#' @param shp
#' @param lookup_tab
#' @param simplify_level
#' @param legend
#' @param labels
#' @param scalebar
#' @param northarrow
#'
#' @return

leaflet_map <- function(scores_csv, goal_code, dim = "score", year = assess_year, basin_or_rgns = "subbasins",
                        shp, lookup_tab, simplify_level = 1, include_legend = TRUE, legend_title = NA){

  ## wrangle for plotting using functions from above
  if(basin_or_rgns == "subbasins"){
    plotting_sf <- make_subbasin_sf(
      shp, scores_csv, lookup_tab,
      goal_code, dim, year,
      simplify_level)
  } else {
    message("creating leaflet map for BHI regions rather than subbasins")
    plotting_sf <- make_rgn_sf(
      shp, scores_csv, lookup_tab,
      goal_code, dim, year,
      simplify_level)
  }

  ## apply theme to get standardized elements, colors, palettes
  ## https://stackoverflow.com/questions/49126405/how-to-set-up-asymmetrical-color-gradient-for-a-numerical-variable-in-leaflet-in
  thm <- apply_bhi_theme()
  pal <- colorNumeric(palette = thm$palettes$divergent_red_blue,
                      domain = c(0, 100),
                      na.color = thm$cols$map_background1)

  ## create leaflet map
  leaflet_map <- leaflet::leaflet(data = plotting_sf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% # "Stamen.TonerLite"
    setView(19, 60.1, zoom = 3)

  if(include_legend){
    leaflet_map <- leaflet_map %>%
      addLegend("bottomright", pal = pal, values = c(0:100),
              title = legend_title, opacity = 0.8, layerId = "colorLegend")
  }

  leaflet_map <- leaflet_map %>%
    addPolygons(
      layerId = ~BHI_ID,
      stroke = TRUE, opacity = 0.5, weight = 2, fillOpacity = 0.6, smoothFactor = 0.5,
      color = thm$cols$map_polygon_border1, fillColor = ~pal(scores))

  return(leaflet_map)
}
