#' flowerplot card module
#'
#' this script contains two functions:
#' \code{flower_ui} generates the user interface for each flowerplot card
#' \code{card_flower} generates the flowerplot shown in a card

## flowerplot card ui function ----

flower_ui <- function(id,
                      title_text = NULL,
                      sub_title_text = NULL){

  ## make namespace for the id-specific object
  ns <- NS(id)

  ## put together in box and return box
  tagList(box(collapsible = TRUE,
              title = "Baltic Sea Scores, 2014",
              list(# h4("Baltic Sea Scores, 2014"),
                p("Flowerplot for entire Baltic Sea, averages over all BHI regions."),
                highchartOutput(ns("flowerplot"))),
              width = 5))

}

## flowerplot card server function ----

card_flower <- function(input,
                        output,
                        session,
                        data,
                        field,
                        region_id){

  rgn_id <- region_id

    df <- tbl(bhi_db_con, "scores2015") %>%
      collect() %>%
      filter(region_id == rgn_id, dimension == field, goal != "Index") %>%
      select(goal, score) %>%
      mutate(no_color = "#00000000") %>%
      left_join(thm$palettes$goals_pal, by = "goal") %>%
      rename(y = score, goal_color = color, color = no_color) %>%
      left_join(tbl(bhi_db_con, "plot_conf") %>%
                  select(goal, order_hierarchy, weight) %>%
                  collect(), by = "goal") %>%
      arrange(order_hierarchy) %>%
      mutate(goal = as.factor(goal))

    plot_obj <- make_flower_plot(
      tbl(bhi_db_con, "scores2015") %>%
        filter(dimension == "score") %>%
        collect(),
      rgn_id = 0, plot_year = 2014,
      include_ranges = TRUE, labels = "arc") %>%

      image_trim() %>%
      image_scale("x350") %>%
      image_border("white", "20x20")

    tmpfile <- plot_obj %>%
      image_write(tempfile(tmpdir = "./www", fileext = ".png"), format = "png")

    output$flowerplot <- renderHighchart({

      highchart() %>%
        hc_chart(type = "column", polar = TRUE, plotBackgroundImage = basename(tmpfile)) %>%
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

}
