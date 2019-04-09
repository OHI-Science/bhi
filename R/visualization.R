## Libraries
source(file.path(here::here(), "R", "common.R"))
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(grDevices)
library(paletteer) # https://github.com/EmilHvitfeldt/paletteer
library(circlize) # https://jokergoo.github.io/circlize_book/book/
library(dbplot)
library(htmlwidgets)
library(RColorBrewer)
library(viridis)


## Functions

#' customize and create standard theme for plots
#'
#' a function to create a standardized theme for plots, updates ggtheme...
#'
#' @param plot_type
#'
#' @return no return value, simply updates the ggplot theme where called

bhi_theme <- function(plot_type = NA){

  ## plot elements and color palettes
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
    text_size = 9,
    title_rel_size = 1.25,
    grid_major = 0.25,
    axis_weight = 0.5,
    legend_pos = "right",
    legend_colour = NA,
    legend_fill = NA,
    blank_circle_rad = 42)

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

  rgn_name_lookup <- readr::read_csv(file.path(dir_spatial, "regions_lookup_complete_wide.csv")) %>%
    dplyr::select(region_id, eez_name, subbasin_name) %>%
    dplyr::mutate(plot_title = paste0(subbasin_name, ", ", eez_name)) %>%
    rowwise() %>%
    dplyr::mutate(name = paste(
      plot_title %>%
        stringr::str_to_lower() %>%
        stringr::str_extract_all("[a-z]+") %>%
        unlist(),
      collapse = "_")) %>%
    ungroup()

  ## theme updates based on plot type
  theme_update(
    text = element_text(family = "Helvetica", color = elmts$dark_line, size = elmts$text_size),
    plot.title = element_text(size = ggplot2::rel(elmts$title_rel_size), hjust = 0.5, face = "bold")
  )
  if(!is.na(plot_type)){
    if(plot_type == "flowerplot"){
      theme_update(
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
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
#' @param plot_year
#' @param color_pal
#' @param legend_tab
#' @param save
#'
#' @return

make_trends_barplot <- function(rgn_scores, plot_year, color_pal, legend = FALSE, save = NA){
  if(!"year" %in% names(rgn_scores)){
    warning("rgn_scores doesn't have a year column; assuming data is for the current year")
    rgn_scores <- rgn_scores %>%
      dplyr::mutate(year = substring(date(), 21, 24))
  }
  if(!plot_year %in% unique(rgn_scores$year)){
    stop("no data for plot_year in the rgn_score input")
  }
  bhi_theme <- bhi_theme("trends_barplot")
  unique_rgn <- unique(rgn_scores$region_id)
  region_name_title <- bhi_theme$rgn_name_lookup %>%
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
    warning(sprintf("no trend data, plot for region_id %s will be empty...", unique_rgn))
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
    name <- paste(
      region_name_title %>%
        stringr::str_to_lower() %>%
        stringr::str_extract_all("[a-z]+") %>%
        unlist(),
      collapse = "_")
    start_pal <- (length(color_pal)/2)*(1+min(rgn_scores$score))
    end_pal <- (length(color_pal)/2)*(1+max(rgn_scores$score))
    color_pal <- color_pal[start_pal:end_pal]
    h <- 4; w <- 7.5; d <- 300
  }
  trends_barplot <- rgn_scores %>%
    ggplot(aes(x = goals_reordered, y = score, fill = score)) +
    geom_bar(stat = "identity", position = position_dodge(), show.legend = legend) +
    geom_hline(yintercept = 0) +
    theme_calc() +
    theme(axis.line = element_blank(), element_line(colour = bhi_theme$elmts$light_line)) +
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

  if(isFALSE(save)){save <- NA}
  if(isTRUE(save)){
    save <- file.path(dir_baltic, "reports", "figures", paste0("trendbarplot_", name))
  }
  if(!is.na(save)){
    save_loc <- file.path(save, paste0("trendbarplot_", name, ".png"))
    ggplot2::ggsave(filename = save_loc, plot = trends_barplot, device = "png",
                    height = h, width = w, units = "in", dpi = d)
  }
  return(invisible(trends_barplot))
}


#' create a BHI flowerplot
#'
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R), filtered to region
#' @param plot_year
#' @param color_pal
#' @param color_by either "goal" or "score"
#' @param center_val
#' @param legend_tab
#' @param save
#' @param dim
#'
#' @return

make_flower_plot <- function(rgn_scores, plot_year = NA, color_pal = NA, color_by = "goal", gradient = FALSE,
                             curve_labels = FALSE, center_val = TRUE, legend_tab = FALSE, save = NA, dim = "score"){
  ## from PlotFlower.R from ohicore package
  ## original function by Casey O'Hara, Julia Lowndes, Melanie Frazier
  ## find original script in R folder of ohicore github repo (as of Mar 27, 2019)

  ## WRANGLING & CHECKS

  ## filtering/wrangling of rgn_scores for years and dimension
  unique_rgn <- unique(rgn_scores$region_id)
  if(length(unique_rgn) != 1){
    message("note: rgn_scores input contains data for more than one region")
  }
  if(!"year" %in% names(rgn_scores)){
    if(is.na(plot_year)){
      plot_year <- substring(date(), 21, 24)
      message("rgn_scores doesn't have a year column; assuming data is for current year\n")
    } else {
      message(paste("rgn_scores doesn't have a year column; assuming data is for given plot_year", plot_year,"\n"))
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

  ## sub/supra goals and positioning
  ## pos, pos_end, and pos_supra indicate positions ie how wide different petals should be based on weightings
  plot_config <- readr::read_csv(file.path(dir_baltic, "conf", "goals.csv")) # dir_baltic from common.R
  goals_supra <- na.omit(unique(plot_config$parent))

  supra_lookup <- plot_config %>%
    dplyr::filter(goal %in% goals_supra) %>%
    dplyr::select(parent = goal, name_supra = name)

  plot_config <- plot_config %>%
    dplyr::left_join(supra_lookup, by = "parent") %>%
    dplyr::filter(!(goal %in% goals_supra)) %>%
    dplyr::select(-preindex_function, -postindex_function, -description) %>%
    dplyr::mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    dplyr::arrange(order_hierarchy)

  ## some color palette and theme stuff
  color_by <- str_to_lower(color_by)
  if(color_by == "goal"){
    if(length(plot_config$goal) > length(color_pal)){
      color_df <- goals_pal
      message("no palette given or too few colors for plotting by goal; using a predefined color palette\n")
    } else {
      color_pal <- color_pal[1:nrow(plot_config)*length(color_pal)/nrow(plot_config)]
      color_df <- tibble::tibble(goal = plot_config$goal, color = color_pal)
    }
  }
  initial_theme <- theme_get()
  bhi_theme <- bhi_theme(plot_type = "flowerplot") # theme_update() for bhi flowerplot

  if(isTRUE(gradient)){
    message("since plotting with a gradient is so intensive, plotting only for first region!")
    unique_rgn <- unique_rgn[1] # first non-zero: setdiff(unique_rgn, 0)[1]
  }

  ## start looping over regions
  for(r in unique_rgn){
    if(!is.null(w_fn)){ # mar vs fis weights differ between regions
      plot_config$weight[plot_config$goal == "FIS"] <- wgts$w_fis[wgts$rgn_id == r]
      plot_config$weight[plot_config$goal == "MAR"] <- 1 - wgts$w_fis[wgts$rgn_id == r]
    }

    ## join config info with scores to create plot_df used for plotting
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
      dplyr::select(goal, score, name_flower, name_supra, weight, pos, pos_end, pos_supra) %>%
      dplyr::mutate(plot_NA = ifelse(is.na(score), 100, NA)) # for displaying NAs
    if(color_by == "goal"){
      plot_df <- plot_df %>%
        dplyr::arrange(goal) %>%
        dplyr::left_join(color_df, by = "goal")
    }
    if(isTRUE(gradient)){
      plot_df <- plot_df %>%
        dplyr::mutate(x = pos - (weight/2), x_end = pos + (weight/2)) %>%
        dplyr::mutate(y_end = ifelse(is.na(score), 0, score)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate( # this line spacing sequence, togther with alpha and size parameters in geom_segement, create the gradient...
          y = list(Filter(function(x) x < y_end, 100^(seq(0, 1, 0.001))))) %>%
        ungroup() %>%
        tidyr::unnest(y) %>%
        dplyr::mutate(y_end = y)
    }

    ## CREATING THE FLOWERPLOTS

    if(isTRUE(gradient)){
      plot_obj <- ggplot(plot_df, aes(x = x, xend = x_end, y = y, yend = y_end))
      if(color_by == "goal"){
        plot_obj <- plot_obj +
          geom_segment(aes(color = goal), size = 0.2, alpha = 0.15, arrow = arrow(length = unit(0.01,"cm"))) +
          scale_color_manual(values = unique(plot_df$color), na.value = "black")
      } else {
        plot_obj <- plot_obj +
          geom_segment(aes(color = y), size = 0.2, alpha = 0.3, arrow = arrow(length = unit(0.02,"cm"))) +
          scale_color_gradient2(low = color_pal[1],
                                mid = color_pal[length(color_pal)/2],
                                high = color_pal[length(color_pal)], midpoint = 50)
      }
      plot_obj <- plot_obj +
        geom_segment(aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 0.01, yend = 0.01),
                     size = 0.1, color = bhi_theme$elmts$dark_line) +
        geom_segment(aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 110.01, yend = 110.01),
                     size = 1, color = bhi_theme$elmts$lightest)

    } else {
      if(color_by == "goal"){
        plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = goal)) +
          geom_bar(aes(y = 100), stat = "identity", size = 0.2, color = bhi_theme$elmts$light_line, fill = bhi_theme$elmts$white) +
          geom_bar(stat = "identity", size = 0.2, color = bhi_theme$elmts$dark_line) +
          scale_fill_manual(values = plot_df$color, na.value = "black")
      } else {
        plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = score)) +
          geom_bar(aes(y = 100), stat = "identity", size = 0.2, color = bhi_theme$elmts$light_line, fill = bhi_theme$elmts$white) +
          geom_bar(stat = "identity", size = 0.2, color = bhi_theme$elmts$dark_line) +
          scale_fill_gradientn(colors = color_pal, na.value = "black", limits = c(0, 100))
      }
      if(any(!is.na(plot_df$plot_NA))){ # overlay light grey background for NAs
        plot_obj <- plot_obj +
          geom_bar(aes(y = plot_NA), stat = "identity", size = 0.2,
                   color = bhi_theme$elmts$light_line, fill = bhi_theme$elmts$lightest)
      }
      plot_obj <- plot_obj +
        geom_errorbar(aes(x = pos, ymin = 0, ymax = 0), size = 0.5, show.legend = NA, color = bhi_theme$elmts$dark_line) + # bolded baseline at zero
        geom_errorbar(aes(x = pos, ymin = 130, ymax = 130), size = 1, show.legend = NA, color = bhi_theme$elmts$lightest) # include some kind of tipping-point line?
    }

    goal_labels <- dplyr::select(plot_df, goal, name_flower)
    name_and_title <- bhi_theme$elmts$rgn_name_lookup %>%
      dplyr::filter(region_id == r)
    plot_obj <- plot_obj +
      labs(title = name_and_title$plot_title, x = NULL, y = NULL) +
      coord_polar(start = pi * 0.5) + # from linear bar chart to polar
      scale_x_continuous(labels = plot_df$goal, breaks = plot_df$pos, limits = c(0, max(plot_df$pos_end))) + # label the petals
      scale_y_continuous(limits = c(-bhi_theme$elmts$blank_circle_rad, # tweak plot-limits of 'polarized' y-axix
                                    ifelse(first(goal_labels == TRUE) | is.data.frame(goal_labels), 150, 100)))

    if(isTRUE(center_val)){
      score_index <- rgn_scores %>%
        dplyr::filter(region_id == r, goal == "Index", dimension == "score") %>%
        dplyr::select(region_id, score) %>%
        dplyr::mutate(score = round(score))
      plot_obj <- plot_obj +
        geom_text(data = score_index, # include central value
                  inherit.aes = FALSE, aes(label = score_index$score),
                  x = 0, y = -bhi_theme$elmts$blank_circle_rad, hjust = 0.5, vjust = 0.5,
                  size = 9, color = bhi_theme$elmts$dark_line)
    }

    # if(isTRUE(curve_labels)){
    #   plot_obj <- plot_obj # https://jokergoo.github.io/circlize_book/book/graphics.html#text ?
    # } else
    plot_obj <- plot_obj +
      geom_text(aes(label = name_flower, x = pos, y = 125), # labeling with supra/sub goal names, use geom_text_repel?
                hjust = 0.5, vjust = 0.5, size = 3, color = bhi_theme$elmts$dark_line)


    ## SAVING PLOTS

    if(isFALSE(save)){save <- NA}
    if(isTRUE(save)){
      save <- file.path(dir_baltic, "reports", "figures", paste0("flowerplot_", name_and_title$name))
    }
    if(!is.na(save)){
      save_loc <- file.path(save, paste0("flowerplot_", name, ".png"))
      ggplot2::ggsave(filename = save_loc, plot = plot_obj, device = "png",
                      height = 6, width = 8, units = "in", dpi = 300)
    }
  }
  theme_set(initial_theme) # set theme back to whatever it was initially
  return(invisible(plot_obj)) # note: will only return the last plot
}

