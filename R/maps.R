## Libraries
source(file.path(here::here(), "R", "common.R"))
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
#' @param mapping_data_sp  sf object associating scores with spatial polygons, i.e. having goal score and geometries information
#' @param shp spatial features object with geometry of either subbasins or BHI regions depending on specified spatial unit
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param simplify_level number of times rmapshaper::ms_simplify function should be used on the shapefile,
#'  to simplify polygons for display
#' @param dim the dimension the object/plot should represent,
#' typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'
#' @param year the scenario year to filter the data to, by default the current assessment yearr
#' @param legend boolean indicating whether or not to include plot legend
#' @param labels boolean indicating whether to include labels-- either subbasin or BHI region names
#' @param scalebar boolean indicating whether or not to include a scalebar
#' @param northarrow boolean indicating whether or not to include a northarrow
#' @param save_map either a directory in which to save the map, or if TRUE will save to a default location
#' @param calc_sf boolean indicating whether to (re)calculate mapping sf object
#'
#' @return returns map created, a ggplot object

map_general <- function(goal_code, basins_or_rgns = "subbasins", mapping_data_sp = NULL,
                        shp = NULL, scores_csv = NULL, simplify_level = 1,
                        dim = "score", year = assess_year,
                        legend = TRUE, labels = FALSE, scalebar = FALSE, northarrow = FALSE,
                        save_map = FALSE, calc_sf = FALSE){

  ## data checks, wrangling ----
  bhi_goals <- collect(tbl(bhi_db_con, "scores2015"))$goal %>% unique()
  if(length(goal_code) > 1|!goal_code %in% bhi_goals){
    stop(sprintf("goal must be one of: %s", paste(bhi_goals, collapse = ", ")))
  }

  if(is.null(mapping_data_sp)){calc_sf <- TRUE}

  if(!calc_sf){
    chk1 <- !goal_code %in% colnames(mapping_data_sp)
    chk2 <- basins_or_rgns == "subbasins" & any(str_detect(mapping_data_sp$Name, ","))
    chk3 <- mapping_data_sp$dimension[1] != dim
    chk4 <- mapping_data_sp$simplified[1] != simplify_level
    if(chk1|chk2|chk3|chk4){calc_sf <- TRUE}
  }

  if(calc_sf){
    if(is.null(shp)|is.null(scores_csv)){
      stop("missing spatial information: must provide sf with goal scores,
           or raw shapefile and scores.csv")
    }
    if(basins_or_rgns == "subbasins"){
      mapping_data_sp <- make_subbasin_sf(
        shp, scores_csv, goal_code = "all", dim, year,
        simplify_level)
    } else {
      message("creating leaflet map for BHI regions rather than subbasins")
      mapping_data_sp <- make_rgn_sf(
        shp, scores_csv, goal_code = "all", dim, year,
        simplify_level)
    }
  }

  ## set up and make map ----
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

  ## additional optional map elements ----
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
  if(labels){
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


  ## save the map ----
  save_loc <- NULL
  if(basins_or_rgns == "subbasins"){
    save_dir <- file.path(dir_assess, "reports", "basin_maps")
  } else {save_dir <- file.path(dir_assess, "reports", "bhi_maps")}

  if(is.character(save_map)){
    if(file.exists(save_map)){
      save_loc <- file.path(save_map, sprintf("scores_%s_map.png", goal_code))
    }
  }
  if(isTRUE(save_map)){
    save_loc <- file.path(save_dir, sprintf("scores_%s_map.png", goal_code))
  }
  if(!is.null(save_loc)){
    ggplot2::ggsave(save_loc, plot_map, device = "png",
                    width = 7, height = 6.5, dpi = "retina", units ="in")
  }

  ## return result, invisible
  return(invisible(plot_map))
}


#' create leaflet map
#'
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param basins_or_rgns one of 'subbasins' or 'regions' to indicate which spatial units should be represented
#' @param mapping_data_sp  sf object associating scores with spatial polygons,
#' i.e. having goal score and geometries information
#' @param shp simple features object with spatial units to be mapped;
#' must be provided along with scores_csv if ´mapping_data_sp´ is not
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param simplify_level number of times rmapshaper::ms_simplify function should be used on the shapefile,
#' to simplify polygons for display
#' @param dim the dimension the object/plot should represent,
#' typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'
#' @param year the scenario year to filter the data to, by default the current assessment year
#' @param overlay_mpas boolean indicating whether to overlay MPAs on the goal scores map
#' @param include_legend boolean indicating whether or not to include plot legend
#' @param legend_title text to be used as the legend title
#' @param calc_sf boolean indicating whether to (re)calculate mapping sf object
#'
#' @return leaflet map with BHI goal scores by BHI region or Subbasins

leaflet_map <- function(goal_code, basins_or_rgns = "subbasins", mapping_data_sp = NULL,
                        shp = NULL, scores_csv = NULL, simplify_level = 1,
                        dim = "score", year = assess_year,
                        # overlay_mpas = FALSE,
                        include_legend = TRUE, legend_title = NA,
                        calc_sf = FALSE){


  ## check and wrangle for plotting ----
  bhi_goals <- collect(tbl(bhi_db_con, "scores2015"))$goal %>% unique()
  if(length(goal_code) > 1|!goal_code %in% bhi_goals){
    stop(sprintf("goal must be one of: %s", paste(bhi_goals, collapse = ", ")))
  }

  if(is.null(mapping_data_sp)){
    if(exists("leaflet_fun_result", where = .GlobalEnv)){
      if(leaflet_fun_result$info$basins_or_rgns == basins_or_rgns &
         leaflet_fun_result$info$dimension ==  dim){
        leaflet_plotting_sf0 <- leaflet_fun_result$data_sf
      } else {calc_sf <- TRUE}
    } else {calc_sf <- TRUE}
  } else {
    leaflet_plotting_sf0 <- mapping_data_sp
    calc_sf <- FALSE
  }

  if(!calc_sf){
    chk1 <- !goal_code %in% colnames(leaflet_plotting_sf0)
    chk2 <- basins_or_rgns == "subbasins" & any(str_detect(leaflet_plotting_sf0$Name, ","))|
      basins_or_rgns != "subbasins" & !any(str_detect(leaflet_plotting_sf0$Name, ","))
    chk3 <- leaflet_plotting_sf0$dimension[1] != dim
    chk4 <- leaflet_plotting_sf0$simplified[1] != simplify_level
    if(chk1|chk2|chk3|chk4){calc_sf <- TRUE}
  }

  if(calc_sf){
    if(is.null(shp)|is.null(scores_csv)){
      stop("missing spatial information: must provide sf with goal scores,
           or raw shapefile and scores.csv")
    }

    if(basins_or_rgns == "subbasins"){
      leaflet_plotting_sf0 <- make_subbasin_sf(
        shp, scores_csv, goal_code = "all", dim, year,
        simplify_level)
    } else {
      message("creating leaflet map for BHI regions rather than subbasins")
      leaflet_plotting_sf0 <- make_rgn_sf(
        shp, scores_csv, goal_code = "all", dim, year,
        simplify_level)
    }
  }
  leaflet_plotting_sf <- leaflet_plotting_sf0 %>%
    rename(score = goal_code)


  ## apply theme to get standardized elements, colors, palettes ----
  thm <- apply_bhi_theme()

  ## create asymmetric color ranges for legend, with same ranges as in 'map_general' function above
  rc1 <- colorRampPalette(
    colors = thm$palettes$divergent_red_blue[1:2],
    space = "Lab")(15)
  rc2 <- colorRampPalette(
    colors = thm$palettes$divergent_red_blue[2:3],
    space = "Lab")(25)
  rc3 <- colorRampPalette(
    colors = thm$palettes$divergent_red_blue[3:4],
    space = "Lab")(20)
  rc4 <- colorRampPalette(
    colors = thm$palettes$divergent_red_blue[4:5],
    space = "Lab")(15)
  rc5 <- colorRampPalette(
    colors = thm$palettes$divergent_red_blue[5:6],
    space = "Lab")(25)

  pal <- colorNumeric(
    palette = c(rc1, rc2, rc3, rc4, rc5),
    domain = c(0, 100),
    na.color = thm$cols$map_background1
  )


  ## create leaflet map ----
  leaflet_map <- leaflet::leaflet(data = leaflet_plotting_sf) %>%
    addProviderTiles(providers$CartoDB.Positron) %>% # "Stamen.TonerLite"
    setView(18, 59, zoom = 5)

  if(include_legend){
    leaflet_map <- leaflet_map %>%
      addLegend("bottomright", pal = pal, values = c(0:100),
              title = legend_title, opacity = 0.8, layerId = "colorLegend")
  }

  leaflet_map <- leaflet_map %>%
    addPolygons(
      layerId = ~Name,
      stroke = TRUE, opacity = 0.5, weight = 2, fillOpacity = 0.6, smoothFactor = 0.5,
      color = thm$cols$map_polygon_border1, fillColor = ~pal(score))

  # if(overlay_mpas){
  #   leaflet_map <- leaflet_map %>%
  #     addPolygons(data = ,
  #                 layerId =  ~Name,
  #                 stroke = TRUE, weight = 1, fillOpacity = 0, smoothFactor = 0.5,
  #                 fillColor = NA)
  # }

  leaflet_fun_result <<- list(
    map = leaflet_map,
    data_sf = leaflet_plotting_sf0,
    info = list(goal = goal_code,
                dimension = dim,
                basins_or_rgns = basins_or_rgns,
                created = Sys.time())
  )

  return(leaflet_fun_result)
}
