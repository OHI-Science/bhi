## Libraries
library(plotly)
library(ggplot2)
library(dplyr)

## Functions

#' compare scores between two scores.csv tables for two specified years
#'
#' @param scores1 first scores table (dataframe) to use
#' @param year1 year of data within first scores table to use in comparison
#' @param scores2 second scores table
#' @param year2 year within second scores table
#' @param dim a string or vector of strings specifying dimension(s) to investigate
#' @param goal a string or vector of strings specifying goal(s) to investigate
#'
#' @return returns a list with two objects: 1. a faceted plot and 2. a summary table

compare_scores <- function(scores1, year1, scores2, year2, dim = "score", goal = "Index"){

  scores1_yrs <- scores1$year %>% unique()
  scores2_yrs <- scores2$year %>% unique()

  if(!(year1 %in% scores1_yrs & year2 %in% scores2_yrs)){
    return("years to compare must be in respective score data")
  } else {
    g <- goal

    comparison_tab <- scores1 %>%
      dplyr::filter(dimension %in% dim,
                    goal %in% g,
                    year == year1) %>%
      dplyr::full_join(scores2 %>%
                         dplyr::filter(dimension %in% dim,
                                       goal %in% g,
                                       year == year2),
                       by = c("region_id", "dimension", "goal")) %>%
      dplyr::rename(scores1 = score.x, year1 = year.x,
                    scores2 = score.y, year2 = year.y) %>%
      dplyr::mutate(scores_diff = scores1 - scores2,
                    yr_diff = ifelse(year1 - year2 > 0, year1 - year2, 1),
                    yearly_diff = scores_diff/yr_diff)

    ## comparision summary table
    comparison_summary <- comparison_tab %>%
      dplyr::group_by(goal, dimension) %>%
      dplyr::summarise(mean_diff = mean(scores_diff, na.rm = TRUE),
                       sd_diff = sd(scores_diff, na.rm = TRUE),
                       number_na = sum(is.na(scores_diff)),
                       mean_yearly_diff = mean(yearly_diff, na.rm = TRUE),
                       biggest_diff = max(abs(scores_diff), na.rm = TRUE))

    ## create plot(s) for comparing two datasets
    pal <- RColorBrewer::brewer.pal(6, "Set2")
    n_facet <- ceiling(length(goal)/3)

    comparison_plot <- ggplot2::ggplot(data = comparison_tab %>%
                                         na.omit(), aes(scores1, scores2)) +
      geom_point(aes(color = dimension, label = region_id), alpha = 0.5, size = 3) +
      scale_color_manual(values = pal) +
      geom_abline(slope = 1, intercept = 0, color = "gray70") +

      theme_bw() + theme(legend.position = c(0.9, 0.12)) +
      labs(x = sprintf("\n Scores for %s from Table 1", year1),
           y = sprintf("Scores for %s from Table 2 \n", year2)) +
      facet_wrap(~ goal, ncol = n_facet) # split up by (sub)goals

    return(list(comparison_plot, comparison_summary, comparison_tab))
  }
}
