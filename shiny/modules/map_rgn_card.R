#' map card module
#'
#' this script contains two functions:
#' \code{mapCardUI} generates the user interface for each card
#' \code{mapCard} generates the plot shown in a card
#' from https://github.com/OHI-Northeast/ne-dashboard/blob/master/modules/map_card.R

## map card ui function ----
mapRgnCardUI <- function(id,
                         title_text = NULL,
                         sub_title_text = NULL,
                         box_width = 6,
                         ht = 480,
                         source_text = NULL,
                         select_type = c(NULL, "radio", "drop_down", "checkboxes"),
                         select_choices = c(""),
                         select_label = NULL,
                         selected = NULL){


  ns <- shiny::NS(id)

  ## selection layout
  if(missing(select_type) == TRUE){ # without selection
    items <- leafletOutput(ns("plot"), height = ht)

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
    items <- list(select, leafletOutput(ns("plot"), height = ht))
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
mapRgnCard <- function(input,
                       output,
                       session,
                       goal_code,
                       dimension_selected,
                       spatial_unit_selected,
                       flower_rgn_selected,
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
    result <- leaflet_map(
      goal_code, spatial_unit_selected(), mapping_data_sp = NULL,
      shp = shp, scores_csv, simplify_level = 1, dim = dimension_selected(), year = assess_year,
      include_legend = TRUE, legend_title)

    ## create popup text
    popup_text <- paste("<h5><strong>", popup_title, "</strong>",
                        result$data_sf[[goal_code]], "</h5>",
                        "<h5><strong>", popup_add_field_title, "</strong>",
                        result$data_sf[[popup_add_field]], "</h5>", sep = " ")

    ## create selected-region overlay, based on spatial unit and flowerplot region
    if(flower_rgn_selected() %in% rgn_ids_vec){
      if(spatial_unit_selected() == "regions"){
        rgn_select <- select(result$data_sf, BHI_ID)
      } else {
        rgn_select <- select(rgns_shp,  BHI_ID)
      }

      rgn_select <- filter(rgn_select, BHI_ID == flower_rgn_selected())
      if(spatial_unit_selected() != "regions"){
        rgn_select <- rmapshaper::ms_simplify(input = rgn_select) %>% sf::st_as_sf()
      }

    } else {
      if(spatial_unit_selected() == "subbasins"){
        rgn_select <- select(result$data_sf, HELCOM_ID)
      } else {rgn_select <- select(subbasins_shp,  HELCOM_ID)}

      rgn_select <- rgn_select %>%
        left_join(
          tbl(bhi_db_con, "basins") %>%
            select(BHI_ID = subbasin_id, HELCOM_ID = helcom_id) %>%
            collect() %>%
            mutate(HELCOM_ID = as.factor(HELCOM_ID)),
          by = "HELCOM_ID"
        ) %>%
        filter(BHI_ID == flower_rgn_selected())
    }

    ## return leaflet map with popup added
    result$map %>%
      addPolygons(
        data = rgn_select,
        stroke = TRUE, weight = 4,
        opacity = 0.6, fillOpacity = 0, color = "red",
        smoothFactor = 3) %>%
      addPolygons(
        popup = popup_text, fillOpacity = 0, stroke = FALSE)
  })
}
