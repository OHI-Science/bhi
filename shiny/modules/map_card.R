#' map card module
#'
#' this script contains two functions:
#' \code{mapCardUI} generates the user interface for each card
#' \code{mapCard} generates the plot shown in a card
#' from https://github.com/OHI-Northeast/ne-dashboard/blob/master/modules/map_card.R

## map card ui function ----
mapCardUI <- function(id,
                      title_text = NULL,
                      sub_title_text = NULL,
                      box_width = 6,
                      source_text = NULL,
                      select_type = c(NULL, "radio", "drop_down", "checkboxes"),
                      select_choices = c(""),
                      select_label = NULL,
                      selected = NULL){


  ns <- shiny::NS(id)

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
              list(p(sub_title_text),
                   addSpinner(items, spin = "rotating-plane", color = "#d7e5e8"),
                   p(source_text)),
              width = box_width))
}


## map card server function ----
mapCard <- function(input,
                    output,
                    session,
                    goal_code,
                    dimension_selected,
                    spatial_unit_selected,
                    year_selected,
                    legend_title = NA,
                    popup_title = NA,
                    popup_add_field = NA,
                    popup_add_field_title = NA){


  ## render and return the leaflet map etc
  output$plot <- renderLeaflet({

    ## scores data
    scores_csv <- full_scores_csv %>%
      filter(dimension == dimension_selected())

    ## shp data...
    if(spatial_unit_selected() == "subbasins"){
      shp <- subbasins_shp
    } else { shp <- rgns_shp }

    ## create leaflet map
    if(dimension_selected() == "score"){
      if(spatial_unit_selected() == "subbasins"){
        result <- leaflet_map(
          goal_code, spatial_unit_selected(),
          mapping_data_sp = mapping_scores_subbasin,
          shp = NULL, scores_csv = NULL, simplify_level = 1,
          dim = dimension_selected(), year = assess_year,
          include_legend = TRUE, legend_title)

      } else {
        result <- leaflet_map(
          goal_code, spatial_unit_selected(),
          mapping_data_sp = mapping_scores_rgn,
          shp = NULL, scores_csv = NULL, simplify_level = 1,
          dim = dimension_selected(), year = assess_year,
          include_legend = TRUE, legend_title)

      }
    } else {
      result <- leaflet_map(
        goal_code, spatial_unit_selected(), mapping_data_sp = NULL,
        shp = shp, scores_csv, simplify_level = 1, dim = dimension_selected(), year = assess_year,
        include_legend = TRUE, legend_title)
    }

    ## create popup text
    popup_text <- paste("<h5><strong>", popup_title, "</strong>",
                        result$data_sf[[goal_code]], "</h5>",
                        "<h5><strong>", popup_add_field_title, "</strong>",
                        result$data_sf[[popup_add_field]], "</h5>", sep = " ")


    ## return leaflet map with popup added
    result$map %>%
      addPolygons(
        popup = popup_text,
        fillOpacity = 0,
        stroke = FALSE
      )
  })
}
