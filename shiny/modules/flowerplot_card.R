#' flowerplot card module
#'
#' this script contains two functions:
#' \code{flower_ui} generates the user interface for each flowerplot card
#' \code{card_flower} generates the flowerplot shown in a card

## flowerplot card ui function ----
flowerplotCardUI <- function(id,
                             title_text = NULL,
                             sub_title_text = NULL){

  ## make namespace for the id-specific object
  ns <- NS(id)

  ## put together in box and return box
  tagList(box(collapsible = TRUE,
              title = title_text,
              list(p(sub_title_text), highchartOutput(ns("flowerplot"))),
              width = 5))

}

## flowerplot card server function ----
flowerplotCard <- function(input,
                           output,
                           session,
                           dimension,
                           region_id){

  rgn_id <- region_id
  dim <- dimension

  df <- tbl(bhi_db_con, "scores2015") %>%
    collect() %>%
    filter(region_id == rgn_id, dimension == dim, goal != "Index") %>%
    select(goal, y = score) %>%
    mutate(color = "#00000000") %>%
    mutate(goal = as.factor(goal)) %>%
    left_join(tbl(bhi_db_con, "plot_conf") %>%
                select(goal, order_hierarchy, weight) %>%
                collect(), by = "goal") %>%
    filter(!goal %in% c()) %>%
    arrange(order_hierarchy)

  plot_obj <- make_flower_plot(
    tbl(bhi_db_con, "scores2015") %>%
      filter(dimension == "score") %>%
      collect(),
    rgn_id, plot_year = 2014,
    include_ranges = TRUE, labels = "arc") %>%

    image_trim() %>%
    image_scale("x350") %>%
    image_border("white", "20x20")

  tmploc <- file.path("./www", ns("tmp.png"))
  image_write(plot_obj, tmploc)

  output$flowerplot <- renderHighchart({

    highchart() %>%
      hc_chart(type = "column", polar = TRUE, plotBackgroundImage = basename(tmploc)) %>%
      hc_xAxis(categories = "goal") %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          point = list(
            events = list(
              click = JS( "function () { location.href = 'https://github.com/OHI-Science/bhi-prep/tree/master/prep/' + this.options.key; }")
            )))) %>%
      hc_add_series(data = df,
                    type = "column",
                    mapping = hcaes(name = goal, key = goal, color = color),
                    name = "Score",
                    showInLegend = FALSE) %>%
      hc_add_theme(hc_theme_null())})

  # file.remove(tmpfile) # need unique ids but don't want all temp files...
}
