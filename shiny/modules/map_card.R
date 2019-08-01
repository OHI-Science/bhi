#' map card module
#'
#' this script contains two functions:
#' \code{map_ui} generates the user interface for each card
#' \code{card_map} generates the plot shown in a card
#' from https://github.com/OHI-Northeast/ne-dashboard/blob/master/modules/map_card.R

## map card ui function ----

map_ui <- function(id,
                   title_text = NULL,
                   sub_title_text = NULL,
                   select_type = c(NULL, "radio", "drop_down", "checkboxes"),
                   select_location = c(NULL, "above", "below"),
                   select_choices = c(""),
                   select_label = NULL,
                   selected = NULL,
                   source_text = NULL){

  ## make namespace for the id-specific object
  ns <- NS(id)

  ## selecting layout
  if(missing(select_type) == TRUE){ # output without selection
    items <- splitLayout(cellWidths = c("30%", "70%"),
                         plotlyOutput(ns("barplot"), height  = 480),
                         leafletOutput(ns("plot"), height = 480),
                         cellArgs = list(style = "padding: 10px")
    )
  } else {

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

    ## chart layout
    if(select_location == "above"){
      items <- list(select, splitLayout(cellWidths = c("30%", "70%"),
                                        plotlyOutput(ns("barplot"), height  = 480),
                                        leafletOutput(ns("plot"), height = 480),
                                        cellArgs = list(style = "padding: 10px")))

    } else if(select_location == "below"){
      items <- list(splitLayout(cellWidths = c("30%", "70%"),
                                plotlyOutput(ns("barplot"), height  = 480),
                                leafletOutput(ns("plot"), height = 480),
                                cellArgs = list(style = "padding: 10px")), select)
    }
  }

  ## put together in box and return box
  tagList(box(collapsible = TRUE,
              title = title_text,
              list(p(sub_title_text), items, p(source_text)),
              width = 7))
}


## map card server function ----

card_map <- function(input,
                     output,
                     session,
                     data,
                     field,
                     goal_code,
                     filter_field = NULL,
                     display_field = NULL,
                     legend_title = NA,
                     popup_title = NA,
                     popup_add_field = NA,
                     popup_add_field_title = NA){

  shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile", "BHI_shapefile") %>%
    dplyr::mutate(Subbasin = as.character(Subbasin)) %>%
    dplyr::mutate(Subbasin = ifelse(Subbasin == "Bothian Sea", "Bothnian Sea", Subbasin)) # NEED TO FIX THIS TYPO!!!!!!!!
  scores_csv <- tbl(bhi_db_con, "scores2015") %>%
    filter(dimension == "score") %>%
    collect()

  result <- leaflet_map(scores_csv, goal_code, dim = "score", year = 2014,
                        basin_or_rgns = "regions", shp, simplify_level = 1,
                        include_legend = TRUE)

  leaflet_map <- result[[1]]
  data_shp <- result[[2]]

  output$plot <- renderLeaflet({
    ## get popup
    popup_text <- paste("<h5><strong>", popup_title, "</strong>", data_shp[[field]], "</h5>",
                        "<h5><strong>", popup_add_field_title, "</strong>", data_shp[[popup_add_field]], "</h5>", sep = " ")
    ## call map!
    leaflet_map
  })

  output$barplot <- renderPlotly({
    scores_barplot(scores_csv,
                   basins_or_rgns = "regions",
                   goal_code,
                   make_html = TRUE)
  })


  # if(field != "input"){
  #
  #   output$plot <- renderLeaflet({
  #
  #     ## get popup
  #     popup_text <- paste("<h5><strong>", popup_title, "</strong>", data_shp[[field]], "</h5>",
  #                         "<h5><strong>", popup_add_field_title, "</strong>", data_shp[[popup_add_field]], "</h5>", sep = " ")
  #
  #     ## create the map
  #     leaflet_map(scores_csv, goal_code, dim = "score", year = 2014,
  #                 basin_or_rgns = "regions", shp = data_shp, lookup_tab, simplify_level = 1,
  #                 legend = TRUE, labels = FALSE, scalebar = FALSE, northarrow = TRUE) %>%
  #       setView(-70.0589, 41.5, zoom = 6)
  #   })
  #
  # } else {
  #   filter_field <- enquo(filter_field)
  #   selected_data <- reactive({
  #     df <- data_shp %>% filter(!!filter_field == input$select)
  #     return(df)
  #   })
  #
  #   output$plot <- renderLeaflet({
  #
  #     # get popup
  #     popup_text <- paste("<h5><strong>", popup_title, "</strong>", selected_data()[[display_field]], "</h5>",
  #                         "<h5><strong>", popup_add_field_title, "</strong>", selected_data()[[popup_add_field]], "</h5>", sep = " ")
  #
  #     leaflet_map(scores_csv, goal_code, dim = "score", year = 2014,
  #                 basin_or_rgns = "regions", shp = selected_data(), lookup_tab, simplify_level = 1, legend = TRUE)
  #   })
  #
  # }
}
