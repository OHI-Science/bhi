function(input, output, session){

  spatial_unit <- reactive({input$spatial_unit})
  dimension <- reactive({input$dimension})

  ## WELCOME ----

  ## flowerplot

  values <- reactiveValues(flower_rgn = 30)

  observeEvent(
    eventExpr = input$flower_rgn, {
      values$flower_rgn <- input$flower_rgn

      flower_rgn <- reactive(values$flower_rgn)
      callModule(flowerplotCard, "baltic_flowerplot",
                 dimension = "score",
                 flower_rgn_selected = flower_rgn)
    }, ignoreNULL = FALSE
  )
  # callModule(flowerplotRgnCard, "baltic_flowerplot",
  #            region_id_selected = reactive(input$flower_rgn)) # region_id_selected = flower_rgn

  ## overall index scores map
  # callModule(mapBarplotCard, "index_map_barplot",
  #            goal_code = "Index",
  #            dimension_selected = dimension,
  #            spatial_unit_selected = spatial_unit,
  #            legend_title = "Scores",
  #            popup_title = "Score:",
  #            popup_add_field = "Name",
  #            popup_add_field_title = "Name:")

  callModule(barplotCard, "index_barplot",
             goal_code = "Index",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)
  callModule(mapCard, "index_map",
             goal_code = "Index",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Name:")


  ## AO ----
  ## Artisanal Opportunities
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

      plot_obj <- plot_obj + geom_line() + theme_bw() + theme(legend.position = "none")
      plotly::ggplotly(plot_obj, tooltip = "text")
    }
  })

}
