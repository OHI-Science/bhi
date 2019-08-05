#' map card module
#'
#' this script contains two functions:
#' \code{map_ui} generates the user interface for each card
#' \code{card_map} generates the plot shown in a card
#' from https://github.com/OHI-Northeast/ne-dashboard/blob/master/modules/map_card.R

## map card ui function ----

map_barplot_ui <- function(id,
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

card_map__barplot <- function(input,
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

  # shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile", "BHI_shapefile") %>%
  #   dplyr::mutate(Subbasin = as.character(Subbasin)) %>%
  #   dplyr::mutate(Subbasin = ifelse(Subbasin == "Bothian Sea", "Bothnian Sea", Subbasin)) # NEED TO FIX THIS TYPO!!!!!!!!
  shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/HELCOM_subbasins_holasbasins", "HELCOM_subbasins_holasbasins")
  scores_csv <- tbl(bhi_db_con, "scores2015") %>%
    filter(dimension == "score") %>%
    collect()

  result <- leaflet_map(goal_code, basins_or_rgns = "subbasins", mapping_data_sp = NULL, # basin_or_rgns = "regions"
                        shp = shp, scores_csv, simplify_level = 1,  dim = "score",
                        year = 2014, include_legend = TRUE)

  output$plot <- renderLeaflet({
    popup_text <- paste("<h5><strong>", popup_title, "</strong>", result$data_sf[[field]], "</h5>",
                        "<h5><strong>", popup_add_field_title, "</strong>", result$data_sf[[popup_add_field]], "</h5>", sep = " ") # get popup

    result$map # call map!
  })

  output$barplot <- renderPlotly({
    scores_barplot(scores_csv,
                   basins_or_rgns = "subbasins", # basin_or_rgns = "regions"
                   goal_code,
                   make_html = TRUE)
  })


  if(field != "input"){

    output$plot <- renderLeaflet({
      popup_text <- paste("<h5><strong>", popup_title, "</strong>", result$data_sf[[field]], "</h5>",
                          "<h5><strong>", popup_add_field_title, "</strong>", result$data_sf[[popup_add_field]], "</h5>", sep = " ")

      result$map # call map!
    })

  } else {
    filter_field <- enquo(filter_field)
    selected_data <- reactive({
      df <- result$data_sf %>% filter(!!filter_field == input$select)
      return(df)
    })

    output$plot <- renderLeaflet({
      popup_text <- paste("<h5><strong>", popup_title, "</strong>", selected_data()[[display_field]], "</h5>",
                          "<h5><strong>", popup_add_field_title, "</strong>", selected_data()[[popup_add_field]], "</h5>", sep = " ")

      result$map # call map!
    })

  }
}
