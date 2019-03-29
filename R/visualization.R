## Libraries
source(file.path(here::here(), "R", "common.R"))
library(ggplot2)
library(paletteer) # https://github.com/EmilHvitfeldt/paletteer
library(circlize) # https://jokergoo.github.io/circlize_book/book/
library(dbplot)
library(htmlwidgets)
library(RColorBrewer)
library(grDevices)

## Color Palettes
reds <- grDevices::colorRampPalette(
  c("#A50026","#D73027","#F46D43","#FDAE61", "#ffdcd8"), space = "Lab")(60)
purples <- grDevices::colorRampPalette(
  c("#FEC0C0","#D39DA3","#936978","#68465B","#53344D"))(35)
blues <- grDevices::colorRampPalette(
  c("#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(35)
diverg_continuous_pal <- c(reds, purples)

bhi_goals_pal <- paletteer_d(rcartocolor, Pastel)[1:9]
bhi_goals_subgoals_pal <- c(paletteer_d(rcartocolor, Pastel)[1:9],
                            paletteer_d(rcartocolor, Bold)[1:9])

## Functions

#' customize and create standard theme for plots
#'
#' a function to create a standardized theme for plots
#' defines colors for background, grid lines, and updates the ggtheme
#'
#' @return 1. a named vector containing colors for background/panel features, 2. a table for looking up full BHI region names

plots_base_theme <- function(){
  bkgrnd <- c(light_line = "grey90", med_line = "grey50", med2_line = "grey30", dark_line = "grey20",
              white_fill = "white", light_fill <- "grey80", med_fill = "grey52", dark_fill = "grey22")
  ggplot2::theme_update(axis.ticks = ggplot2::element_blank(),
                        text = ggplot2::element_text(family = "Helvetica", color = bkgrnd["dark_line"], size = 9),
                        plot.title = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = "bold"),
                        legend.position = "right",
                        panel.border = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        panel.grid.major = ggplot2::element_line(colour = bkgrnd["light_line"], size = 0.25),
                        legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                        axis.line = ggplot2::element_line(colour = bkgrnd["med2_line"], size = 0.5))

  rgn_names_tab <- file.path(dir_spatial, "regions_lookup_complete_wide.csv")
  if(file.exists(rgn_names_tab)){
    rgn_name_lookup <- readr::read_csv(rgn_names_tab) %>%
      dplyr::select(region_id, eez_name, subbasin_name) %>%
      dplyr::mutate(plot_title = paste(subbasin_name, "~", eez_name))
  } else {rgn_name_lookup <- NA}

  return(list(bkgrnd, rgn_name_lookup))
}


#' create trends barplot
#'
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R), filtered to region
#' @param plot_year
#' @param color_pal
#' @param legend_tab
#' @param save
#'
#' @return

make_trends_barplot <- function(rgn_scores, plot_year, color_pal, legend_tab = FALSE, save = NA){

  unique_rgn <- unique(rgn_scores$region_id) # check that input scores data is for just one region
  if(length(unique_rgn) != 1){
    stop("rgn_scores input contains data for more than one region")
  }
  if(!"year" %in% names(rgn_scores)){
    warning("rgn_scores doesn't have a year column; assuming data is for the current year")
    rgn_scores <- rgn_scores %>%
      dplyr::mutate(year = substring(date(), 21, 24))
  }
  if(!plot_year %in% unique(rgn_scores$year)){
    stop("no data for plot_year in the rgn_score input")
  }

  tmp <- plots_base_theme()
  bkgrnd <- tmp[[1]]
  region_name_title <- tmp[[2]] %>%
    dplyr::filter(region_id == unique_rgn)
  region_name_title <- region_name_title$plot_title

  rgn_scores <- rgn_scores %>%
    dplyr::filter(year == plot_year) %>%
    dplyr::filter(dimension == "trend")
  rgn_scores$goals_reordered <- factor(rgn_scores$goal,
    levels = c("AO","BD","CW","CON","EUT","TRA","CS","FP","FIS","MAR",
               "LE","ECO","LIV","NP","SP","ICO","LSP","TR"))

  trends_barplot <- rgn_scores %>%
    ggplot2::ggplot(aes(x = goals_reordered, y = score, fill = goal, size = 0.5)) +
    geom_bar(stat = "identity", position = position_dodge(), show.legend = FALSE) +
    ggplot2::labs(title = region_name_title, x = NULL, y = NULL) +
    scale_fill_manual(values = color_pal)
  trends_barplot
}
