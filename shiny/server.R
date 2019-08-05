function(input, output, session){

  ## WELCOME ----

  # # flowerplot
  # output$flowerplot <- renderImage({
  #   plot_obj <- make_flower_plot(
  #     tbl(bhi_db_con, "scores2015") %>%
  #       filter(dimension == "score") %>%
  #       collect(),
  #     rgn_id = 0, plot_year = 2014,
  #     include_ranges = TRUE, labels = "arc") %>%
  #
  #     image_trim() %>%
  #     image_scale("x350") %>%
  #     image_border("white", "20x20")
  #
  #   tmpfile <- plot_obj %>%
  #     image_write(tempfile(fileext = "jpg"), format = "jpg")
  #   list(src = tmpfile, contentType = "image/jpeg")
  # })
  # overall index scores map
  callModule(card_flower, "overall_baltic_flowerplot",
             data = overall_baltic_flowerplot,
             field = "score",
             region_id = 0)

  # overall index scores map
  callModule(card_map, "overall_baltic_map",
             data = overall_baltic_map,
             field = "scores",
             goal_code = "Index",
             legend_title = "Regions Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Basin:")


  ## AO ----
  ## Artisanal Opportunities



  callModule(card_map, "baltic_map_ao",
             data = baltic_map_ao,
             field = "scores",
             goal_code = "AO",
             legend_title = "Regions Scores",
             popup_title = "Score:",
             popup_add_field = "Name",
             popup_add_field_title = "Basin:")


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
