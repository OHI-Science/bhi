function(input, output, session){

  spatial_unit <- reactive({input$spatial_unit})
  dimension <- reactive({input$dimension})

  ## WELCOME ----

  ## flowerplot
  callModule(flowerplotCard, "baltic_flowerplot",
             dimension = "score",
             region_id = 0)

  ## overall index scores map
  callModule(mapCard, "index_map",
             goal_code = "Index",
             dimension_selected = dimension,
             spatial_unit_selected = spatial_unit,
             legend_title = "Regions Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Basin:")


  # callModule(map_barplot_card, "index_map",
  #            field = "scores",
  #            goal_code = "Index",
  #            spatial_unit = basins_or_rgns,
  #            legend_title = "Regions Scores",
  #            popup_title = "Score:",
  #            popup_add_field = "Name",
  #            popup_add_field_title = "Basin:")


  ## AO ----
  ## Artisanal Opportunities



  # callModule(card_map, "baltic_map_ao",
  #            data = baltic_map_ao,
  #            field = "scores",
  #            goal_code = "AO",
  #            legend_title = "Regions Scores",
  #            popup_title = "Score:",
  #            popup_add_field = "Name",
  #            popup_add_field_title = "Basin:")


  ## BD ----
  ## Biodiversity


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


  ## NP ----
  ## Natural Products


  ## TR ----
  ## Tourism


  ## COMPARE ----


}
