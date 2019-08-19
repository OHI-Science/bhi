## Libraries Etc ----
source(file.path(here::here(), "R", "visualization.R"))
source(file.path(here::here(), "R", "flowerplot.R"))
source(file.path(here::here(), "R", "maps.R"))
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(pool)
library(highcharter)

gh_prep <- "https://github.com/OHI-Science/bhi-1.0-archive/blob/draft/baltic2015/prep"
gh_layers <- "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"

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

text_links <- function(title = NULL, url = NULL, box_width = 12){

  box(class = "text_link_button",
      h4(a(title, href = url, target = "_blank")),
      width = box_width,
      height = 90,
      status = "info",
      solidHeader = TRUE)
}


#' print in console pieces to create region menu code
#'
#' @param rgn_tab_con
#'
#' @return no returned object; prints helpful info in console

make_selectRgn_menu <- function(rgn_tab_con = bhi_db_con){

  rgn <- tbl(rgn_tab_con, "regions") %>%
    select(region_id, subbasin, region_name) %>%
    collect() %>%
    arrange(subbasin) %>%
    mutate(print_col = sprintf("`%s` = %s", region_name, region_id))

  cat(paste0("`", unique(rgn$subbasin), "` = c(`", unique(rgn$subbasin), "` = )", sep =  "\n"))

  cat("\n\n")
  for(s in unique(rgn$subbasin)){
    cat(filter(rgn, subbasin  == s)$print_col, sep = ", \n")
  }
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
message("calculating 'mapping_scores_subbasin' sf object...")
mapping_scores_subbasin <- make_subbasin_sf(
  subbasins_shp, full_scores_csv,
  goal_code = "all", dim = "score", year = assess_year,
  simplify_level = 1)

message("calculating 'mapping_scores_rgn' sf object...")
mapping_scores_rgn <- make_rgn_sf(
  rgns_shp, full_scores_csv,
  goal_code = "all", dim = "score", year = assess_year,
  simplify_level = 1)

