function(input, output, session){

  ## WELCOME ----

  ## flowerplot
  output$flowerplot <- renderPlot({
    plot_obj <- make_flower_plot(readr::read_csv(paste0(here::here(), "-1.0-archive/baltic2015/scores.csv")) %>%
                       filter(dimension == "score"),
                     rgn_id = 0, plot_year = 2014, include_ranges = TRUE)
    plot_obj
  })

  # overall index scores map
  callModule(card_map, "overall_baltic_map",
             data = overall_baltic_map,
             field = "scores",
             legend_title = "Regions Scores",
             popup_title = "Score:",
             popup_add_field = "rgn_name",
             popup_add_field_title = "")


  ## AO ----
  ## Artisanal Opportunities


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
