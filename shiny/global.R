## Libraries ----
source(file.path(here::here(), "R", "visualization.R"))
source(file.path(here::here(), "R", "flowerplot.R"))
source(file.path(here::here(), "R", "maps.R"))
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(pool)
library(highcharter)

## Apply Theme
thm <- apply_bhi_theme()


## Functions ----

source(file.path(here::here(), "shiny", "modules", "map_card.R"))
source(file.path(here::here(), "shiny", "modules", "map_barplot_card.R"))
source(file.path(here::here(), "shiny", "modules", "barplot_card.R"))
source(file.path(here::here(), "shiny", "modules", "flowerplot_card.R"))

#' expand contract menu sidebar subitems
#'
#' ui function to expand and contract subitems in menu sidebar
#' from convertMenuItem by Jamie Afflerbach https://github.com/OHI-Northeast/ne-dashboard/tree/master/functions
#'
#' @param mi menu item as created by menuItem function, including subitems from nested menuSubItem function
#' @param tabName name of the tab that correspond to the mi menu item
#'
#' @return expanded or contracted menu item

convertMenuItem <- function(mi, tabName){
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class) > 0 && mi$attribs$class == "treeview"){
    mi$attribs$class = NULL
  }
  mi
}


#' create text boxes with links
#'
#' @param title the text to be displayed in the box
#' @param url url the box should link to
#' @param box_width width of box, see shinydashboard::box 'width' arguement specifications
#'
#' @return

text_links <- function(title = NULL, url = NULL, box_width = 4){

  box(class = "text_link_button",
      h4(a(title, href = url, target = "_blank")),
      width = box_width,
      status = "info",
      solidHeader = TRUE)
}


## Shiny Global Data ----

## full scores
full_scores_csv <- tbl(bhi_db_con, "scores2015") %>% collect()

## shapefiles: BHI regions, subbasins, MPAs
rgns_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile",
                        "BHI_shapefile") %>%
  dplyr::mutate(Subbasin = as.character(Subbasin)) %>%
  dplyr::mutate(Subbasin = ifelse(Subbasin == "Bothian Sea",
                                  "Bothnian Sea", Subbasin)) # NEED TO FIX THIS TYPO!!!!!!!!
subbasins_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/HELCOM_subbasins_holasbasins",
                             "HELCOM_subbasins_holasbasins")
# mpa_shp <- sf::st_read()

## shps combined with score information, only score dim and current assessment year
mapping_scores_subbasin <- make_subbasin_sf(
  subbasins_shp, full_scores_csv,
  goal_code = "all", dim = "score", year = assess_year,
  simplify_level = 1)

mapping_scores_rgn <- make_rgn_sf(
  rgns_shp, full_scores_csv,
  goal_code = "all", dim = "score", year = assess_year,
  simplify_level = 1)

