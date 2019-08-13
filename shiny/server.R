function(input, output, session){

  spatial_unit <- reactive({input$spatial_unit})
  dimension <- reactive({input$dimension})

  ## WELCOME ----

  ## flowerplot
  callModule(flowerplotCard, "baltic_flowerplot",
             flower_id = "baltic_flowerplot",
             dimension = "score",
             region_id = 0)

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
  # callModule(mapCard, "bd_map",
  #            goal_code = "BD",
  #            dimension_selected = dimension,
  #            spatial_unit_selected = spatial_unit,
  #            legend_title = "Scores",
  #            popup_title = "Score:",
  #            popup_add_field = "Name",
  #            popup_add_field_title = "Name:")
  # callModule(barplotCard, "bd_barplot",
  #            goal_code = "BD",
  #            dimension_selected = dimension,
  #            spatial_unit_selected = spatial_unit)


  ## CS ----
  ## Carbon Storage


  ## CW ----
  ## Clean Water


  ## CON ----
  ## Contaminants


  ## EUT ----
  ## Eutrophication


  ## TRA ----
  ## Trash


  ## FP ----
  ## Food Provision


  ## FIS ----
  ## Wild-Caught Fisheries


  ## MAR ----
  ## Mariculture


  ## LE ----
  ## Livelihoods & Economies


  ## ECO ----
  ## Economies


  ## LIV ----
  ## Livelihoods


  ## SP ----
  ## Sense of Place


  ## ICO ----
  ## Iconic Species


  ## LSP ----
  ## Lasting Special Places
  # callModule(mapCard, "lsp_map",
  #            goal_code = "LSP",
  #            dimension_selected = dimension,
  #            spatial_unit_selected = spatial_unit,
  #            legend_title = "Scores",
  #            popup_title = "Score:",
  #            popup_add_field = "Name",
  #            popup_add_field_title = "Name:")
  callModule(barplotCard, "lsp_barplot",
             goal_code = "LSP",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit)


  ## NP ----
  ## Natural Products


  ## TR ----
  ## Tourism


  ## COMPARE ----


}
