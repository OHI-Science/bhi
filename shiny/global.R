## Libraries
source(file.path(here::here(), "R", "visualization.R"))
source(file.path(here::here(), "R", "flowerplot.R"))
source(file.path(here::here(), "R", "maps.R"))
library(shiny)
library(shinydashboard)
library(shinyjs)
library(pool)
library(highcharter)

## Functions

#' ui function to expand and contract subitems in menu sidebar
#' from convertMenuItem https://github.com/OHI-Northeast/ne-dashboard/tree/master/functions
#'
#' @param mi menu item
#' @param tabName
#'
#' @return

convertMenuItem <- function(mi, tabName){
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class) > 0 && mi$attribs$class == "treeview"){
    mi$attribs$class = NULL
  }
  mi
}
