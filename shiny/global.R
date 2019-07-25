## Libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(pool)
# library(highcharter)
# library(here)
# library(readr)
# library(tidyr)
# library(stringr)

## Functions

#' ui function to expand and contract subitems in menu sidebar
#'
#' @param mi
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
