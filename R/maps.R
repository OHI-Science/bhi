## Libraries
source(file.path(here::here(), "R", "common.R"))
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)


## Functions

#' create standardized map, general function
#'
#' @param mapping_data_sp
#' @param goal_code
#' @param legend
#' @param labels
#' @param scalebar
#' @param northarrow
#'
#' @return

map_general <- function(mapping_data_sp, goal_code,
                        legend = TRUE, labels = FALSE,
                        scalebar = FALSE, northarrow = FALSE){


  ## background map of baltic countries
  if(!exists("baltic")){
    baltic <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      st_crop(xmin = -5, xmax = 35, ymin = 48, ymax = 70)
  }

  ## create the map using ggplot geom_sf
  plot_map <- ggplot(data = baltic) +
    ## baltic countries
    geom_sf(fill = "#f0e7d6", alpha = 0.6, color = "#b2996c", size = 0.1) + # f0e7d6
    ## overlay goal scores by subbasin with custom continuous color palette
    geom_sf(data = mapping_data_sp, aes_string(fill = goal_code), color = "#acb9b6", size = 0.1) +
    ## some formatting and map elements
    scale_fill_gradientn(colours = c("#8c031a", "#cc0033", "#fff78a", "#f6ffb3", "#009999", "#0278a7"),
                         breaks = c(15, 40, 60, 75, 90, 99), limits = c(0, 100),
                         na.value = "#fcfcfd") +
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
#' @param subbasins_shp
#' @param scores_csv
#' @param basin_lookup
#' @param goal_code
#' @param dim
#' @param simplified
#'
#' @return

make_subbasin_sf <- function(subbasins_shp, scores_csv, basin_lookup,
                             goal_code = "all", dim = "score", simplify_level = 1){


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
      select(Subbasin, goal_code, dimension, simplified)
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
#' @param goal_code
#' @param dim
#' @param simplify_level
#' @param save_map
#' @param legend
#' @param basin_labels
#' @param scalebar
#' @param northarrow
#'
#' @return

subbasin_goal_map <- function(subbasins_shp, scores_csv, basin_lookup, goal_code, dim = "score",
                              simplify_level = 1, save_map = FALSE,
                              legend = TRUE, basin_labels = FALSE, scalebar = FALSE, northarrow = FALSE){


  ## create or recreate subbasin goals sf object if needed
  if(!exists("mapping_data_sp")){
    mapping_data_sp <- make_subbasin_sf(subbasins_shp, scores_csv, basin_lookup,
                                        goal_code, dim, simplify_level)
  }

  chk1 <- !goal_code %in% names(mapping_data_sp)
  chk2 <- mapping_data_sp$dimension[1] != dim
  chk3 <- mapping_data_sp$simplified[1] != simplify_level
  if(chk1|chk2|chk3){
    mapping_data_sp <- make_subbasin_sf(subbasins_shp, scores_csv, "all", dim, simplify_level)
  }

  ## map_general function above, used in both subbasin and bhi-region level maps
  plot_map <- map_general(mapping_data_sp, goal_code, legend,
                          labels = basin_labels, scalebar, northarrow)

  ## save the map
  save_loc <- NULL
  if(is.character(save_map)){
    if(file.exists(save_map)){
      save_loc <- sprintf(file.path(save_map, "scores_%s_map.png"), goal_code)
    }
  }
  if(isTRUE(save_map)){
    save_loc <- sprintf(file.path(dir_assess, "reports", "basin_maps", "scores_%s_map.png"), goal_code)
  }
  if(!is.null(save_loc)){
    ggplot2::ggsave(save_loc, plot_map, device = "png", width = 7, height = 6.5, dpi = "retina", units ="in")
  }

  return(invisible(plot_map))
}


#' create BHI region level maps
#'
#' @param bhi_rgns_shp
#' @param scores_csv
#' @param rgn_lookup
#' @param goal_code
#' @param dim
#' @param simplify_level
#' @param save_map
#' @param legend
#' @param rgn_labels
#' @param scalebar
#' @param northarrow
#'
#' @return

bhi_rgn_goal_map <- function(bhi_rgns_shp, scores_csv, rgn_lookup, goal_code, dim = "score",
                             simplify_level = 1, save_map = FALSE,
                             legend = TRUE, rgn_labels = FALSE, scalebar = FALSE, northarrow = FALSE){


  ## wrangle/reshape and join with spatial info to make sf for plotting
  mapping_data <- scores_csv %>%
    dplyr::filter(dimension == dim, region_id %in% rgn_lookup$BHI_ID) %>%
    dplyr::left_join(select(rgn_lookup, BHI_ID, rgn_nam, Subbasin),
                     by = c("region_id" = "BHI_ID")) %>%
    dplyr::mutate(label_txt = sprintf("%s, %s", Subbasin, rgn_nam)) %>%
    dplyr::select(region_id, goal, score, label_txt) %>%
    tidyr::spread(key = goal, value = score) %>%
    dplyr::mutate(dimension = dim, simplified = simplify_level)

  if(goal_code != "all" & goal_code %in% names(mapping_data)){
    mapping_data <- mapping_data %>%
      select(label_txt, region_id, goal_code, dimension, simplified)
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
      save_loc <- sprintf(file.path(save_map, "scores_%s_map.png"), goal_code)
    }
  }
  if(isTRUE(save_map)){
    save_loc <- sprintf(file.path(dir_assess, "reports", "bhi_maps", "scores_%s_map.png"), goal_code)
  }
  if(!is.null(save_loc)){
    ggplot2::ggsave(save_loc, plot_map, device = "png", width = 7, height = 6.5, dpi = "retina", units ="in")
  }

  return(invisible(plot_map))
}

