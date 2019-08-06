#' map card module
#'
#' this script contains two functions:
#' \code{map_ui} generates the user interface for each card
#' \code{card_map} generates the plot shown in a card
#' from https://github.com/OHI-Northeast/ne-dashboard/blob/master/modules/map_card.R

## map card ui function ----
mapCardUI <- function(id,
                      title_text = NULL,
                      sub_title_text = NULL,
                      source_text = NULL,
                      select_type = c(NULL, "radio", "drop_down", "checkboxes"),
                      select_choices = c(""),
                      select_label = NULL,
                      selected = NULL){


  ns <- NS(id)

  ## selection layout
  if(missing(select_type) == TRUE){ # without selection
    items <- leafletOutput(ns("plot"), height = 480)

  } else {  # with selection
    if(select_type == "radio"){ # selection type
      select <- radioButtons(ns("select"),
                             choices = select_choices,
                             label = p(select_label),
                             selected = selected,
                             inline = TRUE)
    } else if(select_type == "drop_down"){
      select <- selectInput(ns("select"),
                            choices = select_choices,
                            label = p(select_label),
                            selected = selected)
    } else {
      select <- checkboxGroupInput(ns("select"),
                                   choices = select_choices,
                                   label = p(select_label),
                                   selected = selected)
    }
    items <- list(select, leafletOutput(ns("plot"), height = 480))
  }

  ## put together in box and return box
  tagList(box(collapsible = TRUE,
              title = title_text,
              list(p(sub_title_text), items, p(source_text)),
              width = 7))
}


## map card server function ----
mapCard <- function(input,
                    output,
                    session,
                    goal_code,
                    dimension_selected,
                    spatial_unit_selected,
                    legend_title = NA,
                    popup_title = NA,
                    popup_add_field = NA,
                    popup_add_field_title = NA){

  ## render and return the leaflet map etc
  output$plot <- renderLeaflet({

    ## scores data from bhi database
    scores_csv <- tbl(bhi_db_con, "scores2015") %>%
      filter(dimension == d) %>%
      collect()

    ## for now, read in shp data...
    if(spatial_unit_selected() == "subbasins"){
      shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/HELCOM_subbasins_holasbasins",
                         "HELCOM_subbasins_holasbasins")
    } else {
      shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile",
                         "BHI_shapefile") %>%
        dplyr::mutate(Subbasin = as.character(Subbasin)) %>%
        dplyr::mutate(Subbasin = ifelse(Subbasin == "Bothian Sea",
                                        "Bothnian Sea", Subbasin)) # NEED TO FIX THIS TYPO!!!!!!!!
    }

    result <- leaflet_map(goal_code, spatial_unit_selected(), mapping_data_sp = NULL,
                          shp = shp, scores_csv, simplify_level = 1, dim = d, year = 2014,
                          include_legend = TRUE, legend_title)

    popup_text <- paste("<h5><strong>", popup_title, "</strong>",
                        result$data_sf[[goal_code]], "</h5>",
                        "<h5><strong>", popup_add_field_title, "</strong>",
                        result$data_sf[[popup_add_field]], "</h5>", sep = " ")
    result$map
  })
}
