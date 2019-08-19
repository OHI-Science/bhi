#' flowerplot card module
#'
#' this script contains two functions:
#' \code{flowerplotCardUI} generates the user interface for each flowerplot card
#' \code{flowerplotCard} generates the flowerplot shown in a card

## flowerplot card ui function ----
flowerplotRgnCardUI <- function(id,
                                title_text = NULL,
                                sub_title_text = NULL){

  ## make namespace for the id-specific object
  ns <- shiny::NS(id)

  ## put together in box and return box
  tagList(box(collapsible = TRUE,
              title = title_text,
              list(p(sub_title_text), highchartOutput(ns("flowerplot"), height = 480)),
              width = 5))
}

## flowerplot card server function ----
flowerplotRgnCard <- function(input, output, session, flower_id, region_id_selected){ # year_selected arg later....

  pre_flower <- function(rgn_id){

    df <- tbl(bhi_db_con, "scores2015") %>%
      filter(dimension == "score") %>%
      collect()

    ## make flowerplot
    plot_obj <- make_flower_plot(
      df, rgn_id, plot_year = 2014,
      include_ranges = TRUE, labels = "arc") %>%
      image_trim() %>%
      image_scale("x350") %>%
      image_border("white", "10x10")

    tmploc <- file.path("./www", paste0(rgn_id, "_tmp.png"))
    image_write(plot_obj, tmploc) # saves rather than returns

    ## config dataframe for interactive/popups...
    df <- df %>%
      filter(region_id == rgn_id, goal != "Index") %>%
      select(goal, y = score) %>%
      left_join(tbl(bhi_db_con, "plot_conf") %>%
                  select(goal,  parent, order_htmlplot) %>%
                  collect(), by = "goal") %>%
      mutate(color = "#00000000")
    arrange(
      rbind(filter(df, !goal %in% df$parent),
            filter(df, is.na(parent), !goal %in% df$parent)),
      order_htmlplot
    )
  }

  re_rgn <- eventReactive(
    input$flower_rgn, # trigger on input$flower_rgn
    {
      rgn_id <- region_id_selected()
      df <- pre_flower(rgn_id)

      leafletProxy("index_map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          lat = 60,
          lng = 25,
          radius = 3,
          fillOpacity = 0.1,
          stroke = FALSE
        )
    }
  )

  ## run preflower function with region id 0 to start
  df <- pre_flower(rgn_id = 0)

  ## render and return full flowerplot
  output$flowerplot <- renderHighchart({

    re_rgn() # rerun preflower with detected change in input flower_rgn

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
