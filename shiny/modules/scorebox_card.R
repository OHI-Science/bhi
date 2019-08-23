#' goal score infobox module
#'
#' this script contains two functions:
#' \code{scoreBoxUI} generates the user interface for each goal scorebox
#' \code{scoreBox} generates the infobox and text it contains

## scorebox ui function ----
scoreBoxUI <- function(id){
  ns <- shiny::NS(id)
  tagList(list(infoBoxOutput(ns("goal_scorebox"),  width = 12)))
}

## scorebox server function ----
scoreBox<- function(input,
                    output,
                    session,
                    goal_code,
                    flower_rgn_selected){


  scores <- full_scores_csv %>%
    filter(goal == goal_code, dimension == "score")

  output$goal_scorebox <- renderInfoBox(
    infoBox(
      title = "",
      tags$p(
        ifelse(
          flower_rgn_selected() == 0,
          filter(scores, region_id == 0)$score,
          sprintf(
            "%s  |  %s",
            filter(scores, region_id == 0)$score %>% round(1),
            filter(scores, region_id == flower_rgn_selected())$score %>% round(1)
          )
        ),
        style = ifelse(
          flower_rgn_selected() == 0,
          "font-size: 260%; text-align:center; font-weight: lighter;",
          "font-size: 230%; text-align:center; font-weight: lighter;"
        )
        # style = ifelse(
        #   flower_rgn_selected() == 0,
        #   "font-size: 225%; text-align:center; font-weight: lighter;",
        #   "font-size: 185%; font-weight: lighter;"
        # )
      ),
      icon = icon(thm$icons[[goal_code]]),
      color =  filter(thm$palettes$goalpal_shiny, goal == goal_code)$color,
      fill = TRUE
    )
  )
}
