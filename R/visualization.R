## Libraries
source(file.path(here::here(), "R", "common.R"))
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(grDevices)
library(circlize) # https://jokergoo.github.io/circlize_book/book/
library(dbplot)
library(htmlwidgets)
library(magick)
library(formattable)
library(DT)
library(webshot) # webshot::install_phantomjs()
library(htmltools)


## Functions

#' customize and create standard theme for plots
#'
#' a function to create a standardized theme for plots, updates ggtheme...
#'
#' @param plot_type if applying theme for a specific type of plot, specify here (options: flowerplot, trends_barplot, ...)
#'
#' @return no return value, simply updates the ggplot theme where called

apply_bhi_theme <- function(plot_type = NA){

  ## plot elements ----
  elmts <- list(
    white = "white",
    lightest = "grey95",
    light_line = "grey90",
    light_fill = "grey80",
    med_line = "grey50",
    med_fill = "grey52",
    med2 = "grey30",
    dark_line = "grey20",
    dark_fill = "grey22",
    bright_line = "maroon",
    text_size = 9,
    title_rel_size = 1.25,
    grid_major = 0.25,
    axis_weight = 0.5,
    legend_pos = "right",
    legend_colour = NA,
    legend_fill = NA,
    blank_circle_rad = 42)

  ## color palettes ----
  bhi_palettes <- list(

    reds = grDevices::colorRampPalette(
      c("#A50026","#D73027","#F46D43","#FDAE61", "#ffdcd8"))(40),
    purples = grDevices::colorRampPalette(
      c("#EEDFFF","#C093F7","#9E5AF0","#822BEA"))(50),
    blues = grDevices::colorRampPalette(
      c("#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(50),

    goals_pal = tibble::tibble(
      goal = c("MAR","FIS","FP","CW","CON","EUT","TRA",
               "SP","LSP","ICO","LE","ECO","LIV",
               "AO","TR","CS","NP", "BD"),
      color = c("#549dad","#4ead9d","#53b3ac","#89b181","#60a777","#7ead6d","#9aad7e",
                "#97a4ba","#9fb3d4","#7c96b4","#9a97ba","#7d7ca3","#9990b4",
                "#e2de9a","#b6859a","#d0a6a1","#ccb4be","#88b1a6")))

  ## region names lookup table ----
  rgn_name_lookup <- rbind(
    readr::read_csv(file.path(dir_spatial, "regions_lookup_complete_wide.csv")) %>%
      dplyr::select(region_id, eez_name, subbasin_name),
    data.frame(region_id = 0, eez_name = "", subbasin_name = "Baltic Sea")) %>%
    dplyr::mutate(plot_title = ifelse(region_id != 0, paste0(subbasin_name, ", ", eez_name), subbasin_name)) %>%
    rowwise() %>%
    dplyr::mutate(name = paste(
      plot_title %>%
        stringr::str_to_lower() %>%
        stringr::str_extract_all("[a-z]+") %>%
        unlist(),
      collapse = "_")) %>%
    ungroup()

  ## theme updates based on plot type ----
  theme_update(
    text = element_text(family = "Helvetica", color = elmts$dark_line, size = elmts$text_size),
    plot.title = element_text(size = ggplot2::rel(elmts$title_rel_size), hjust = 0.5, face = "bold")
  )
  if(!is.na(plot_type)){
    if(plot_type == "flowerplot"){
      theme_update(
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.key = element_rect(colour = elmts$legend_colour, fill = elmts$legend_fill),
        legend.position = elmts$legend_pos,
        axis.line = element_blank(),
        axis.text.y = element_blank()
      )
    }
  }
  return(list(elmts = elmts, bhi_palettes = bhi_palettes, rgn_name_lookup = rgn_name_lookup))
}


#' create trends barplot
#'
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R), filtered to region
#' @param color_pal a continuous color palette, ideally diverging, that will map to trend values in the barplots
#' @param plot_year year by which to filter region score input dataframe
#' @param include_legend boolean indicating if legend should be included or not
#' @param save the plot will not be saved if 'save' is FALSE or NA, will be saved to file.path(save) if a string, or to "reports/figures" directory if TRUE
#'
#' @return

make_trends_barplot <- function(rgn_scores, color_pal, plot_year = NA, include_legend = FALSE, save = NA){

  ## checks, filtering, wrangling ----
  if(!"year" %in% names(rgn_scores)){
    if(is.na(plot_year)){
      plot_year <- substring(date(), 21, 24)
      message("rgn_scores doesn't have a year column; assuming data is for current year\n")
    } else {
      message(paste("rgn_scores doesn't have a year column; assuming data is for given plot_year", plot_year,"\n"))
    }
    rgn_scores$year <- plot_year
  } else {
    if(!plot_year %in% unique(rgn_scores$year)){
      message("no data for given plot_year in the rgn_score input")
    }
    if(is.na(plot_year) | !plot_year %in% unique(rgn_scores$year)){
      plot_year <- max(rgn_scores$year)
      message(paste("plotting using most recent year in the input data:", plot_year,"\n"))
    }
  }

  initial_theme <- theme_get()
  bhi_thm <- apply_bhi_theme("trends_barplot")

  unique_rgn <- unique(rgn_scores$region_id)
  region_name_title <- bhi_thm$rgn_name_lookup %>%
    dplyr::filter(region_id %in% unique_rgn)
  rgn_scores <- rgn_scores %>%
    dplyr::filter(year == plot_year) %>%
    dplyr::filter(dimension == "trend") %>%
    na.omit()
  rgn_scores$goals_reordered <- factor(
    rgn_scores$goal,
    levels = c("TR", "LSP","ICO","SP","NP",
               "LIV","ECO","LE","MAR","FIS","FP","CS",
               "TRA","EUT","CON","CW","BD","AO"))

  if(nrow(rgn_scores) == 0){
    message(sprintf("no trend data, plot for region_id %s will be empty...", unique_rgn))
  }
  if(length(unique_rgn) > 1){
    print("rgn_scores input contains data for more than one region; plotting with facet_wrap")
    facet_nrow <- ceiling(sqrt(length(unique_rgn))*1.5)
    facet_ncol <- ceiling(length(unique_rgn)/facet_nrow)
    rgn_scores <- rgn_scores %>%
      dplyr::left_join(region_name_title, by = "region_id")
    name <- "multiregion"; h <- 7; w <- 11; d <- 450
  } else {
    region_name_title <- region_name_title$plot_title
    name <- region_name_title$name
    start_pal <- (length(color_pal)/2)*(1+min(rgn_scores$score))
    end_pal <- (length(color_pal)/2)*(1+max(rgn_scores$score))
    color_pal <- color_pal[start_pal:end_pal]
    h <- 4; w <- 7.5; d <- 300
  }

  ## creating plots ----
  trends_barplot <- rgn_scores %>%
    ggplot(aes(x = goals_reordered, y = score, fill = score)) +
    geom_bar(stat = "identity", position = position_dodge(), show.legend = include_legend) +
    geom_hline(yintercept = 0) +
    theme_calc() +
    theme(axis.line = element_blank(), element_line(colour = bhi_thm$elmts$light_line)) +
    labs(x = NULL, y = NULL)

  if(length(unique_rgn) > 1){
    trends_barplot <- trends_barplot +
      theme(axis.text.x = element_text(size = 6, angle = 90)) +
      scale_fill_gradientn(colors = color_pal) +
      facet_wrap( ~ plot_title, ncol = facet_ncol)
  } else {
    trends_barplot <- trends_barplot +
      geom_text_repel(aes(label = goals_reordered), size = 3, family = "Helvetica") +
      theme(axis.text.y = element_blank()) +
      scale_fill_gradientn(colors = color_pal) +
      labs(title = region_name_title)  +
      coord_flip()
  }

  ## saving plots ----
  if(isFALSE(save)){save <- NA}
  if(isTRUE(save)){
    save <- file.path(dir_baltic, "reports", "figures", paste0("trendbarplot_", name))
  }
  if(!is.na(save)){
    save_loc <- file.path(save, paste0("trendbarplot_", name, ".png"))
    if(!file.exists(save)){ message("that save location is not valid") } else {
      ggplot2::ggsave(filename = save_loc, plot = trends_barplot, device = "png",
                      height = h, width = w, units = "in", dpi = d)
    }
  }
  theme_set(initial_theme)
  return(invisible(trends_barplot))
}


#' create a BHI flowerplot
#'
#' This function creates BHI/OHI flower plots
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R), filtered to region
#' @param plot_year year by which to filter region score input dataframe; defaults to current year or maximum year in score data input
#' @param dim the dimension the flowerplot petals should represent (typically OHI 'score')
#' @param color_pal a color palette that will map to values of specified dimension 'dim'; ideally discrete for color_by 'goal' and continuous otherise
#' @param color_by either "goal" or "score"
#' @param gradient a boolean indicating whether flowerplot petals should have a gradient, i.e. more intense color near center and more transparent towards edges
#' @param legend_tab boolean indicating if legend should be included or not
#' @param update_legend a boolean indicating whether legend will need to be recalculated for a new plot (creating the legend takes time so avoid if possible)
#' @param labels one of "none", "regular", "curved"
#' @param center_val a boolean indicating whether the region average value should be included in the center of the flower plot
#' @param critical_value value at which a light red line should drawn overlying the plot, to indicate a 'critical point' of some kind...
#' @param save the plot will not be saved if 'save' is FALSE or NA, will be saved to file.path(save) if a string, or to "reports/figures" directory if TRUE
#'
#' @return result is a flower plot; if curved labels or legend table are included, the resulting class is magick-image, otherwise the result is a ggplot object

make_flower_plot <- function(rgn_scores, plot_year = NA, dim = "score",
                             color_pal = NA, color_by = "goal", gradient = FALSE, legend_tab = FALSE, update_legend = FALSE,
                             labels = "none", center_val = TRUE, critical_value = 10, save = NA){

  ## from PlotFlower.R from ohicore package
  ## original function by Casey O'Hara, Julia Lowndes, Melanie Frazier
  ## find original script in R folder of ohicore github repo (as of Mar 27, 2019)

  ## WRANGLING & CHECKS
  ## region scores data checks ----
  ## filtering/wrangling of rgn_scores for years and dimension
  unique_rgn <- unique(rgn_scores$region_id)
  if(length(unique_rgn) != 1){
    message("note: rgn_scores input contains data for more than one region")
  }
  if(!"year" %in% names(rgn_scores)){
    if(is.na(plot_year)){
      plot_year <- substring(date(), 21, 24)
      message("no year column in rgn_scores; assuming data is for current year\n")
    } else {
      message(paste("no year column in rgn_scores; assuming data is for given plot_year", plot_year,"\n"))
    }
    rgn_scores$year <- plot_year
  } else {
    if(is.na(plot_year) | !plot_year %in% unique(rgn_scores$year)){
      plot_year <- max(rgn_scores$year)
      message(paste("plotting using most recent year in the input data:", plot_year,"\n"))
    }
  }
  rgn_scores <- rgn_scores %>%
    dplyr::filter(year == plot_year, dimension == dim)

  ## fis and mar ----
  ## weights for fis vs. mar, uses layers/fp_wildcaught_weight.csv
  ## csv info determines relative width in the flowerplot of the two food provision subgoals
  w_fn <- list.files(file.path(dir_baltic, "layers"),
                     pattern = "fp_wildcaught_weight",
                     full.names = TRUE)
  if(length(w_fn) != 1){
    if(length(w_fn) > 1){
      message("fp_wildcaught_weight.csv not unique...")
    } else {message("fp_wildcaught_weight.csv not found in layers...")}
    w_fn <- NULL
  } else {
    wgts <- readr::read_csv(w_fn) %>%
      dplyr::filter(year == plot_year)
    if(length(wgts$w_fis) != 0){
      mean_wgt <- mean(wgts$w_fis) # mean across regions within the year of interest
      wgts <- rbind(
        data.frame(rgn_id = 0, w_fis = mean_wgt),
        dplyr::filter(wgts, rgn_id %in% unique_rgn) %>%
          dplyr::select(-year))
    } else {
      message(paste("fp_wildcaught_weight.csv doesn't have data for the plot_year:", plot_year))
      w_fn <- NULL
    }
  }
  if(is.null(w_fn)){message("plotting FIS and MAR with equal weighting\n")}

  ## PLOTTING CONFIGURATION
  ## sub/supra goals and positioning ----
  ## pos, pos_end, and pos_supra indicate positions, how wide different petals should be based on weightings
  plot_config <- readr::read_csv(file.path(dir_baltic, "conf", "goals.csv")) # dir_baltic from common.R
  goals_supra <- na.omit(unique(plot_config$parent))

  supra_lookup <- plot_config %>%
    dplyr::filter(goal %in% goals_supra) %>%
    dplyr::select(parent = goal, name_supra = name_flower)

  plot_config <- plot_config %>%
    dplyr::left_join(supra_lookup, by = "parent") %>%
    dplyr::filter(!(goal %in% goals_supra)) %>%
    dplyr::select(-preindex_function, -postindex_function, -description) %>%
    dplyr::mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    dplyr::mutate(name_supra = gsub("\\n", "\n", name_supra, fixed = TRUE)) %>%
    dplyr::arrange(order_hierarchy)
  if(labels == "curved"){
    plot_config <- plot_config %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        f = stringr::str_split(name_flower, "\n"),
        f1 = ifelse(order_calculate <= max(plot_config$order_calculate)/2, f[1],  f[2]),
        f2 = ifelse(order_calculate <= max(plot_config$order_calculate)/2, f[2], f[1])) %>%
      dplyr::select(-f)
  }

  ## some color palette and theme stuff ----
  initial_theme <- theme_get()
  bhi_thm <- apply_bhi_theme(plot_type = "flowerplot") # theme_update() for bhi flowerplot

  color_by <- str_to_lower(color_by)
  if(color_by == "goal"){
    if(length(plot_config$goal) > length(color_pal)){
      color_df <- bhi_thm$bhi_palettes$goals_pal
      message("no palette given or too few colors for plotting by goal; using a predefined color palette\n")
    } else {
      color_pal <- color_pal[1:nrow(plot_config)*length(color_pal)/nrow(plot_config)]
      color_df <- tibble::tibble(goal = plot_config$goal, color = color_pal)
    }
  }
  if(isTRUE(gradient)){
    message("since plotting with a gradient is so intensive, plotting only for first region!!!\n")
    unique_rgn <- unique_rgn[1] # maybe should get first non-zero? setdiff(unique_rgn, 0)[1]
  }

  ## start looping over regions: region-specific configuration and then plotting ----
  for(r in unique_rgn){
    if(!is.null(w_fn)){ # mar vs fis weights differ between regions
      plot_config$weight[plot_config$goal == "FIS"] <- wgts$w_fis[wgts$rgn_id == r]
      plot_config$weight[plot_config$goal == "MAR"] <- 1 - wgts$w_fis[wgts$rgn_id == r]
    }

    ## join plot_config w scores to create plot_df used in plotting ----
    plot_df <- rgn_scores %>%
      dplyr::filter(region_id == r) %>%
      dplyr::inner_join(plot_config, by = "goal") %>%
      dplyr::arrange(order_color) %>%
      dplyr::mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
      dplyr::mutate(pos_end = sum(weight)) %>%
      dplyr::group_by(name_supra) %>%
      dplyr::mutate(pos_supra = ifelse(!is.na(name_supra), mean(pos), NA)) %>% # to deal with unequal weights
      dplyr::ungroup() %>%
      dplyr::filter(weight != 0) %>%
      dplyr::select(-dimension, -region_id, -year, -order_color, -order_hierarchy, -parent, -name) %>%
      dplyr::mutate(plot_NA = ifelse(is.na(score), 100, NA)) # for displaying NAs
    if(color_by == "goal"){
      plot_df <- plot_df %>%
        dplyr::arrange(goal) %>% # for matching colors with correct petals, this arrangment is important...
        dplyr::left_join(color_df, by = "goal")
      # color_pal <- dplyr::arrange(plot_df, order_calculate)$color
    }

    ## if plotting with a gradient, expand plot_df with y column indicating
    plot_df0 <- plot_df
    if(isTRUE(gradient)){
      plot_df <- plot_df %>%
        dplyr::mutate(x = pos - (weight/2), x_end = pos + (weight/2)) %>%
        dplyr::mutate(y_end = ifelse(is.na(score), 0, score)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate( # this sequence, together w alpha + size params in geom_segement, create gradient...
          y = list(Filter(function(x) x < y_end, 11^(seq(0, 1, 0.001))*10-10))) %>%
        dplyr::mutate(y = if(length(unlist(y)) > 0){list(y)} else {list(0)}) %>%
        dplyr::ungroup() %>%
        tidyr::unnest(y) %>%
        dplyr::mutate(
          name_flower = ifelse(y != 0, NA, name_flower),
          name_supra = ifelse(y != 0, NA, name_supra)) %>%
        dplyr::select(-y_end)
    }

    ## CREATING THE FLOWERPLOTS
    ## with or without gradient ----

    ## with gradient
    if(isTRUE(gradient)){
      plot_obj <- ggplot(plot_df, aes(x = x, xend = x_end, y = y, yend = y))
      if(color_by == "goal"){
        plot_obj <- plot_obj +
          geom_segment(aes(color = goal),
                       size = 0.15, alpha = 0.15,
                       show.legend = FALSE,
                       arrow = arrow(length = unit(0.01,"cm"))) +
          scale_color_manual(values = unique(plot_df$color), na.value = "black")
      } else {
        plot_obj <- plot_obj +
          geom_segment(aes(color = y),
                       size = 0.2, alpha = 0.3,
                       show.legend = FALSE,
                       arrow = arrow(length = unit(0.02,"cm"))) +
          scale_color_gradient2(low = color_pal[1],
                                mid = color_pal[length(color_pal)/2],
                                high = color_pal[length(color_pal)], midpoint = 50)
      }
      if(any(!is.na(plot_df$plot_NA))){ # overlay light grey background for NAs
        plot_obj <- plot_obj +
          geom_rect(data = filter(plot_df, !is.na(plot_NA)),
                    inherit.aes = FALSE, aes(xmin = x, xmax = x_end, ymin = 0, ymax = plot_NA),
                    fill = bhi_thm$elmts$lightest)
      }
      plot_obj <- plot_obj +
        geom_segment(aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 0, yend = 0),
                     size = 0.5, color = bhi_thm$elmts$dark_line) +
        geom_segment(aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = critical_value, yend = critical_value),
                     size = 0.1, color = bhi_thm$elmts$bright_line) +
        geom_segment(aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 105, yend = 105),
                     size = 3, color = bhi_thm$elmts$lightest)

    ## without a gradient
    } else {
      if(color_by == "goal"){
        plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = goal)) +
          geom_bar(aes(y = 100), stat = "identity", size = 0.2, color = bhi_thm$elmts$med_line, fill = bhi_thm$elmts$white) +
          geom_bar(stat = "identity", size = 0.2, color = bhi_thm$elmts$med_line, show.legend = FALSE) +
          scale_fill_manual(values = plot_df$color, na.value = "black")
      } else {
        plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = score)) +
          geom_bar(aes(y = 100), stat = "identity", size = 0.2, color = bhi_thm$elmts$med_line, fill = bhi_thm$elmts$white) +
          geom_bar(stat = "identity", size = 0.2, color = bhi_thm$elmts$med_line, show.legend = FALSE) +
          scale_fill_gradientn(colors = color_pal, na.value = "black", limits = c(0, 100))
      }
      if(any(!is.na(plot_df$plot_NA))){ # overlay light grey background for NAs
        plot_obj <- plot_obj +
          geom_bar(aes(y = plot_NA), stat = "identity", size = 0.2, color = bhi_thm$elmts$med_line, fill = bhi_thm$elmts$lightest)
      }
      plot_obj <- plot_obj +
        geom_errorbar(aes(x = pos, ymin = 0, ymax = 0), # bolded baseline at zero
                      size = 0.5, show.legend = NA, color = bhi_thm$elmts$dark_line) +
        geom_errorbar(aes(x = pos, ymin = critical_value, ymax = critical_value), # some kind of tipping-point line?
                      size = 0.25,show.legend = NA, color = bhi_thm$elmts$bright_line) +
        geom_errorbar(aes(x = pos, ymin = 105, ymax = 105), # outer ring indicating room for even more progress?
                      size = 3, show.legend = NA, color = bhi_thm$elmts$lightest)
    }

    ## general plot elements for all flowerplots, gradient or no ----
    goal_labels <- dplyr::select(plot_df, goal, name_flower)
    name_and_title <- bhi_thm$rgn_name_lookup %>%
      dplyr::filter(region_id == r)
    plot_obj <- plot_obj +
      labs(x = NULL, y = NULL) +
      coord_polar(start = pi * 0.5) + # from linear bar chart to polar
      scale_x_continuous(labels = NULL,
                         breaks = plot_df$pos,
                         limits = c(0, max(plot_df$pos_end))) +
      scale_y_continuous(limits = c(-bhi_thm$elmts$blank_circle_rad, # tweak plot-limits of 'polarized' axes
                                    ifelse(first(goal_labels == TRUE)|is.data.frame(goal_labels), 150, 100)))

    ## include average value in center if center_val is true
    if(isTRUE(center_val)){
      score_index <- rgn_scores %>%
        dplyr::filter(region_id == r, goal == "Index", dimension == "score") %>%
        dplyr::select(region_id, score) %>%
        dplyr::mutate(score = round(score))
      plot_obj <- plot_obj +
        geom_text(data = score_index, # include central value
                  inherit.aes = FALSE, aes(label = score_index$score),
                  x = 0, y = -bhi_thm$elmts$blank_circle_rad, hjust = 0.5, vjust = 0.5,
                  size = 9, color = bhi_thm$elmts$dark_line)
    }

    ## LABELING AND LEGENDS

    ## labeling with sub/supra goal names + legend stuff, using magick ----
    if(labels %in% c("regular","standard","normal","level")){
      plot_obj <- plot_obj +
        # labs(title = name_and_title$plot_title) +
        geom_text(aes(label = name_supra, x = pos_supra, y = 150),
                  size = 3.4, hjust = 0.4, vjust = 0.8,
                  color = bhi_thm$elmts$light_fill) +
        geom_text(aes(label = name_flower, x = pos, y = 120),
                  size = 3, hjust = 0.5, vjust = 0.5,
                  color = bhi_thm$elmts$dark_line)
    }
    if(labels == "curved"||isTRUE(legend_tab)){
      temp_plot <- file.path(dir_baltic, "temp", paste0("flowerplot_", name_and_title$name, ".png"))
      ggplot2::ggsave(filename = temp_plot, plot = plot_obj, device = "png", bg = "transparent",
                      height = 6, width = 8, units = "in", dpi = 300) # this causes a big hangup when plot has gradient...
      img_plot <- magick::image_read(temp_plot)

      if(labels == "curved"){
        circ_df <- plot_df0 %>%
          dplyr::select("goal", "f1", "f2", "name_supra", "weight", "order_calculate") %>%
          dplyr::mutate(weight = 0.3 + 0.7 * weight) %>%
          dplyr::mutate(x = sum(weight) - (cumsum(weight) - weight), x_end = sum(weight) - (cumsum(weight))) %>%
          tidyr::gather("start_end", "range", -goal, -name_supra, -weight, -order_calculate, -f1, -f2) %>%
          dplyr::select(-start_end, -weight, -goal) %>%
          dplyr::arrange(order_calculate)

        temp_labels <- file.path(dir_baltic, "temp", paste0("flower_curvetxt_", name_and_title$name, ".jpg"))

        if(!file.exists(temp_labels)){ # don't recreate curved labels if already exist....
          jpeg(temp_labels, width = 2450, height = 2450, quality = 220)
          message("creating curved labels for plot:\n")

          ## curved labels created with 'circlize' package
          circos.clear()
          circos.par("track.height" = 0.1, cell.padding = c(0,0,0,0), "clock.wise" = FALSE, start.degree = 5)
          circos.initialize(factors = circ_df$order_calculate, x = circ_df$range)

          circos.track(factors = circ_df$order_calculate, y = circ_df$range, panel.fun = function(x, y){
            circos.text(CELL_META$xcenter, CELL_META$ycenter,
                        CELL_META$sector.index, col = "white")}, bg.col = NA, bg.border = FALSE)
          circos.track(factors = circ_df$order_calculate, y = circ_df$range, panel.fun = function(x, y){
            circos.text(CELL_META$xcenter, CELL_META$ycenter,
                        circ_df$f1[circ_df$order_calculate == as.numeric(CELL_META$sector.index)][1],
                        cex = 5, col = bhi_thm$elmts$dark_line, adj = c(0.4, 1),
                        facing = "bending.inside", niceFacing = TRUE)}, bg.col = NA, bg.border = FALSE)
          circos.track(factors = circ_df$order_calculate, y = circ_df$range, panel.fun = function(x, y){
            circos.text(CELL_META$xcenter, CELL_META$ycenter,
                        circ_df$f2[circ_df$order_calculate == as.numeric(CELL_META$sector.index)][1],
                        cex = 5, col = bhi_thm$elmts$dark_line, adj = c(0.4, 0),
                        facing = "bending.inside", niceFacing = TRUE)}, bg.col = NA, bg.border = FALSE)

          highlight.sector(circ_df$order_calculate[circ_df$name_supra != "Food Provision"|is.na(circ_df$name_supra)],
                           track.index = 1, text = "Food Provision", cex = 6.5,
                           text.col = bhi_thm$elmts$light_line, col = NA,
                           facing = "bending.outside", niceFacing = TRUE)
          highlight.sector(circ_df$order_calculate[circ_df$name_supra != "Coastal Livelihoods & Economies"|is.na(circ_df$name_supra)],
                           track.index = 1, text = "Coastal Livelihoods & Economies", cex = 6.5,
                           text.col = bhi_thm$elmts$light_line, col = NA,
                           facing = "bending.outside", niceFacing = TRUE)
          highlight.sector(circ_df$order_calculate[circ_df$name_supra != "Sense of Place"|is.na(circ_df$name_supra)],
                           track.index = 1, text = "Sense of Place", cex = 6.5,
                           text.col = bhi_thm$elmts$light_line, col = NA,
                           facing = "bending.outside", niceFacing = TRUE)
          dev.off()
        }
        img_text <- magick::image_read(temp_labels)
        img_plot <- img_plot %>%
          image_trim() %>%
          image_background("none")
        img_plot <- image_composite(image_scale(img_text, 550), image_scale(img_plot, 330), offset = "+110+110")
      }

      ## create legend table to accompany the plot
      if(isTRUE(legend_tab)){

        temp_legend <- file.path(dir_baltic, "temp", paste0("flowerlegend_by_", color_by, "_", name_and_title$name, ".jpg"))
        if(isTRUE(update_legend)||!file.exists(temp_legend)){
          message("creating legend table for plot:\n")

          legend_df <- plot_config %>%
            dplyr::select(goal, parent, name, name_supra) %>%
            dplyr::left_join(dplyr::filter(rgn_scores, region_id == r), by = "goal")
          if(color_by == "goal"){
            legend_cols <- dplyr::mutate(left_join(legend_df, color_df, by = "goal"), color = ifelse(is.na(score), NA, color))$color
          } else { legend_cols <- dplyr::mutate(legend_df, color = color_pal[length(color_pal)*score/100])$color }
          legend_cols[is.na(legend_cols)] <- "#DDDDDD"

          legend_df <- legend_df %>%
            dplyr::mutate(
              Key = "*******",
              Goal = paste(ifelse(is.na(name_supra),"", paste(name_supra, ": ")),
                           name,"(",ifelse(is.na(parent), goal, paste(parent, goal)),")")) %>%
            dplyr::select(Goal, Key, score)
          names(legend_df) <- c("Goal", "Key", stringr::str_to_title(dim))

          tab <- formattable(legend_df, align = c("l","l","r"), list(
            `Goal` = formatter("span", style = ~ style(color = "grey")),
            `Score` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "white", "grey"))),
            `Key` = formatter("span", style = x ~ style("background-color" = legend_cols,
                                                        color = ifelse(is.na(x), "#DDDDDD", legend_cols)))
            )
          )
          ## must have phantomjs installed- can do this with webshot::install_phantomjs()
          path <- htmltools::html_print(as.htmlwidget(tab, width = "48%", height = NULL), background = "white", viewer = NULL)
          url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
          webshot::webshot(url, file = temp_legend, selector = ".formattable_widget", delay = 0.2)
        }
        img_legend <- magick::image_read(temp_legend)

        img_plot <- img_plot %>%
          image_trim() %>%
          image_background("white") %>%
          image_border("white", "80x60") %>%
          image_scale(800)
        img_legend <- img_legend %>%
          image_border("white", "20x92") %>%
          image_scale(600)
        img_plot <- image_append(c(img_plot, img_legend))
      }
      plot_obj <- img_plot
    }

    ## SAVING PLOTS
    ## saving with method based on plot_obj class ----
    if(isFALSE(save)){save <- NA}
    if(isTRUE(save)){
      save <- file.path(dir_baltic, "reports", "figures", paste0("flowerplot_", name_and_title$name, ".png"))
    }
    if(!is.na(save)){
      save_loc <- save
      if(class(plot_obj) == "magick-image"){
        magick::image_write(plot_obj, path = save_loc, format = "png")
      } else {
        ggplot2::ggsave(filename = save_loc, plot = plot_obj, device = "png", height = 3.5, width = 5, units = "in", dpi = 400)
      }
    }
  }
  ## wrap up function ----
  theme_set(initial_theme) # set theme back to whatever it was initially
  return(invisible(plot_obj)) # note: will only return the last plot
}

