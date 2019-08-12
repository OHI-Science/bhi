#' barplot for map card module
#'
#' this script contains two functions:
#' \code{barplotCardUI} generates the user interface for each card
#' \code{mapBarplotCard} generates the plot shown in the card

## barplot card ui function ----
barplotCardUI <- function(id,
                          title_text = NULL,
                          sub_title_text = NULL,
                          source_text = NULL){

  ns <- shiny::NS(id)
  items <- plotlyOutput(ns("barplot"), height  = 480)
  tagList(box(collapsible = TRUE,
              title = title_text,
              list(p(sub_title_text), items, p(source_text)),
              width = 3))
}


## map card server function ----
barplotCard <- function(input,
                        output,
                        session,
                        goal_code,
                        dimension_selected,
                        spatial_unit_selected){

  output$barplot <- renderPlotly({
    ## scores data from bhi database
    d <- dimension_selected()

    scores_csv <- tbl(bhi_db_con, "scores2015") %>%
      filter(dimension == d) %>%
      collect()
    ## map barplot
    scores_barplot(scores_csv,
                   basins_or_rgns = spatial_unit_selected(),
                   goal_code,
                   dim = dimension_selected(),
                   make_html = TRUE)
  })
}
