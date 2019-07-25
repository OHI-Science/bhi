function(input, output, session){

  ## Overall Index
  output$flowerplot <- renderUI({
    tags$img(src = "https://raw.github.com/OHI-Northeast/ne-scores/master/region/reports/figures/flower_USNortheast.png", height = 450, width = 600)
    })
}
