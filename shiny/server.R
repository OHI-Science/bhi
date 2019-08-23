function(input, output, session){

  spatial_unit <- reactive({input$spatial_unit})
  dimension <- reactive({input$dimension})

  ## WELCOME ----

  ## flowerplot

  values <- reactiveValues(flower_rgn = 0)

  observeEvent(
    eventExpr = input$flower_rgn, {
      values$flower_rgn <- input$flower_rgn

      flower_rgn <- reactive(values$flower_rgn)
      callModule(flowerplotCard, "baltic_flowerplot",
                 dimension = "score",
                 flower_rgn_selected = flower_rgn)
    }, ignoreNULL = FALSE
  )

  callModule(barplotCard, "index_barplot",
             goal_code = "Index",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)
  # callModule(mapCard, "index_map",
  #            goal_code = "Index",
  #            dimension_selected = dimension,
  #            spatial_unit_selected = spatial_unit,
  #            legend_title = "Scores",
  #            popup_title = "Score:",
  #            popup_add_field = "Name",
  #            popup_add_field_title = "Name:")
  observeEvent(
    eventExpr = input$flower_rgn, {
      values$flower_rgn <- input$flower_rgn

      flower_rgn <- reactive(values$flower_rgn)
      callModule(mapRgnCard, "index_map",
                 goal_code = "Index",
                 dimension_selected = dimension,
                 spatial_unit_selected = spatial_unit,
                 flower_rgn_selected = flower_rgn,
                 legend_title = "Scores",
                 popup_title = "Score:",
                 popup_add_field = "Name",
                 popup_add_field_title = "Name:")
    }, ignoreNULL = FALSE
  )


  ## AO ----
  ## Artisanal Opportunities
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "ao_infobox",
        goal_code = "AO",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "ao_map",
             goal_code = "AO",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "ao_barplot",
             goal_code = "AO",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## BD ----
  ## Biodiversity
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "bd_infobox",
        goal_code = "BD",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "bd_map",
             goal_code = "BD",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "bd_barplot",
             goal_code = "BD",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## CS ----
  ## Carbon Storage
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "cs_infobox",
        goal_code = "CS",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "cs_map",
             goal_code = "CS",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "cs_barplot",
             goal_code = "CS",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## CW ----
  ## Clean Water
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "cw_infobox",
        goal_code = "CW",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "cw_map",
             goal_code = "CW",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "cw_barplot",
             goal_code = "CW",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## CON ----
  ## Contaminants
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "con_infobox",
        goal_code = "CON",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "con_map",
             goal_code = "CON",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "con_barplot",
             goal_code = "CON",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## EUT ----
  ## Eutrophication
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "eut_infobox",
        goal_code = "EUT",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "eut_map",
             goal_code = "EUT",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "eut_barplot",
             goal_code = "EUT",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## TRA ----
  ## Trash
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "tra_infobox",
        goal_code = "TRA",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "tra_map",
             goal_code = "TRA",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "tra_barplot",
             goal_code = "TRA",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## FP ----
  ## Food Provision
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "fp_infobox",
        goal_code = "FP",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "fp_map",
             goal_code = "FP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "fp_barplot",
             goal_code = "FP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## FIS ----
  ## Wild-Caught Fisheries
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "fis_infobox",
        goal_code = "FIS",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "fis_map",
             goal_code = "FIS",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "fis_barplot",
             goal_code = "FIS",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## MAR ----
  ## Mariculture
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "mar_infobox",
        goal_code = "MAR",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "mar_map",
             goal_code = "MAR",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "mar_barplot",
             goal_code = "MAR",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## LE ----
  ## Livelihoods & Economies
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "le_infobox",
        goal_code = "LE",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "le_map",
             goal_code = "LE",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "le_barplot",
             goal_code = "LE",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## ECO ----
  ## Economies
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "eco_infobox",
        goal_code = "ECO",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "eco_map",
             goal_code = "ECO",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "eco_barplot",
             goal_code = "ECO",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## LIV ----
  ## Livelihoods
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "liv_infobox",
        goal_code = "LIV",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "liv_map",
             goal_code = "LIV",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "liv_barplot",
             goal_code = "LIV",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## SP ----
  ## Sense of Place
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "sp_infobox",
        goal_code = "SP",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "sp_map",
             goal_code = "SP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "sp_barplot",
             goal_code = "SP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## ICO ----
  ## Iconic Species
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "ico_infobox",
        goal_code = "ICO",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "ico_map",
             goal_code = "ICO",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "ico_barplot",
             goal_code = "ICO",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## LSP ----
  ## Lasting Special Places
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "lsp_infobox",
        goal_code = "LSP",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "lsp_map",
             goal_code = "LSP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "lsp_barplot",
             goal_code = "LSP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## NP ----
  ## Natural Products
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "np_infobox",
        goal_code = "NP",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "np_map",
             goal_code = "NP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "np_barplot",
             goal_code = "NP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## TR ----
  ## Tourism
  observeEvent(
    eventExpr = input$flower_rgn, {

      values$flower_rgn <- input$flower_rgn
      flower_rgn <- reactive(values$flower_rgn)

      callModule(
        scoreBox,
        "tr_infobox",
        goal_code = "TR",
        flower_rgn_selected = flower_rgn
      )
    },
    ignoreNULL = FALSE
  )
  callModule(mapCard, "tr_map",
             goal_code = "TR",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")
  callModule(barplotCard, "tr_barplot",
             goal_code = "TR",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## COMPARE ----
  output$pressure_ts <- renderPlotly({

    press_var <- input$press_var

    # press_dat <- tbl(bhi_db_con, "pressures") %>%
    press_dat <- tbl(bhi_db_con, "for_now_press_data") %>% # just a few layers for now...
      select(region_id, year, press_var) %>%
      collect() %>%
      left_join(
        select(thm$rgn_name_lookup, region_id, plot_title),
        by = "region_id") %>%
      rename(Name = plot_title, Pressure = press_var, Year = year)

    if(spatial_unit() == "subbasins"){
      press_dat <- press_dat %>%
        filter(region_id %in% subbasin_ids_vec)
    } else {
      press_dat <- press_dat %>%
        filter(region_id %in% rgn_ids_vec)
    }
    if(nrow(press_dat) == 0){
      stop("no pressure data...")
    } else {

      plot_obj <- ggplot2::ggplot(
        data = press_dat,
        aes(x = Year, y = Pressure,
          color = Name,
          text =  sprintf("%s:\n%s", str_replace(Name, ", ", "\n"), Pressure)
        )
      )

      plot_obj <- plot_obj +
        geom_line() +
        theme_bw() +
        theme(legend.position = "none")

      plotly::ggplotly(plot_obj, tooltip = "text")
    }
  })

  output$layers_scatter <- renderPlot({

    gh_lyrs <- "https://raw.githubusercontent.com/OHI-Science/bhi-1.0-archive/draft/baltic2015/layers/"
    dat_x <- readr::read_csv(paste0(gh_lyrs, input$layerscatter_var_x))
    dat_y <- readr::read_csv(paste0(gh_lyrs, input$layerscatter_var_y))

    x_name <- str_to_upper(str_remove(input$layerscatter_var_x, ".csv"))
    y_name <- str_to_upper(str_remove(input$layerscatter_var_y, ".csv"))

    df <- left_join(dat_x, dat_y, by = "rgn_id")
    colnames(df) <- c("region_id", x_name, y_name)

    ggplot(data  = df) +
      geom_point(aes_string(x_name, y_name)) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
  })

  # values <- reactiveValues(
  #   lyr_df = data.frame(
  #     Value1 = 1:10,
  #     Value2 = c("A", "B", "C", "D", "E"),
  #     stringsAsFactors = FALSE,
  #     row.names = 1:10
  #   )
  # )
  #
  # observeEvent(
  #   eventExpr = input$layers_dt_vars, {
  #
  #     lyr_df <- data.frame()
  #     for(l in ){
  #       lyr_df
  #     }
  #
  #     output$layers_datatab <- renderDataTable({lyr_df})
  #
  #   }, ignoreNULL = FALSE
  # )

}
