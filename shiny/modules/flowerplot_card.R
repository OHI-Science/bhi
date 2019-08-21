#' flowerplot card module
#'
#' this script contains two functions:
#' \code{flowerplotCardUI} generates the user interface for each flowerplot card
#' \code{flowerplotCard} generates the flowerplot shown in a card

## flowerplot card ui function ----
flowerplotCardUI <- function(id,
                             title_text = NULL,
                             sub_title_text = NULL){

  ## make namespace for the id-specific object
  ns <- shiny::NS(id)

  ## put together in box and return box
  tagList(box(collapsible = TRUE,
              title = title_text,
              list(p(sub_title_text),
                   addSpinner(highchartOutput(ns("flowerplot"), height = 480),
                              spin = "rotating-plane", color = "#d7e5e8")),
              width = 5))
}

## flowerplot card server function ----
flowerplotCard <- function(input, output, session, dimension, flower_rgn_selected){


  rgn_id <- flower_rgn_selected()
  dim <- dimension

  ## make flowerplot
  scorerange <- ifelse(rgn_id == 0, TRUE, FALSE)
  plot_obj <- make_flower_plot(
    tbl(bhi_db_con, "scores2015") %>%
      filter(dimension == "score") %>%
      collect(),
    rgn_id, plot_year = 2014,
    include_ranges = scorerange, labels = "arc") %>%

    image_trim() %>%
    image_scale("x350") %>%
    image_border("white", "10x10")

  tmploc <- file.path("./www", paste0("flower", "_tmp.png"))
  image_write(plot_obj, tmploc)


  ## config dataframe for interactive/popups...
  df <- tbl(bhi_db_con, "scores2015") %>%
    collect() %>%
    filter(region_id == rgn_id, dimension == dim, goal != "Index") %>%
    select(goal, y = score) %>%
    left_join(tbl(bhi_db_con, "plot_conf") %>%
                select(goal,  parent, order_htmlplot) %>%
                collect(), by = "goal") %>%
    mutate(color = "#00000000")
  df2 <- filter(df, is.na(parent), !goal %in% df$parent)
  df <- arrange(rbind(filter(df, !goal %in% df$parent), df2), order_htmlplot)

  ## render and return full flowerplot
  output$flowerplot <- renderHighchart({

    highchart() %>%
      hc_chart(type = "column",
               polar = TRUE,
               # height or width so doesn't stretch?!
               plotBackgroundImage = basename(tmploc),
               width = 400) %>%
      hc_xAxis(categories = df$goal) %>%
      hc_yAxis("min" = -100, max = 100) %>%
      hc_pane(startAngle = 10) %>%
      hc_plotOptions(
        series = list(
          cursor = "pointer",
          pointWidth = 0.35,
          point = list(
            events = list(
              click = JS("function(){ location.href = 'https://github.com/OHI-Science/bhi-prep/tree/master/prep/' + this.options.key; }")
            )))) %>%
      hc_add_series(data = df,
                    type = "column",
                    mapping = hcaes(key = goal, color = color),
                    name = "",
                    showInLegend = FALSE) %>%
      hc_add_theme(hc_theme_null())})

  # file.remove(tmpfile) # need unique ids but don't want all temp files...
}
