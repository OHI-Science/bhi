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

## Color Palettes and Other Theme/Plot Elements
reds <- grDevices::colorRampPalette(
  c("#A50026","#D73027","#F46D43","#FDAE61", "#ffdcd8"))(40)
purples <- grDevices::colorRampPalette(
  c("#EEDFFF","#C093F7","#9E5AF0","#822BEA"))(50)
blues <- grDevices::colorRampPalette(
  c("#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(50)

elmts <- list(
  light_line = "grey90",
  med_line = "grey50",
  med2_line = "grey30",
  dark_line = "grey20",
  white_fill = "white",
  light_fill = "grey80",
  med_fill = "grey52",
  dark_fill = "grey22",
  text_size = 9,
  title_rel_size = 1.25,
  grid_major = 0.25,
  axis_weight = 0.5,
  legend_pos = "right",
  legend_colour = NA,
  legend_fill = NA,
  blank_circle_rad = 42,

  rgn_name_lookup = readr::read_csv(file.path(dir_spatial, "regions_lookup_complete_wide.csv")) %>%
    dplyr::select(region_id, eez_name, subbasin_name) %>%
    dplyr::mutate(plot_title = paste0(subbasin_name, ", ", eez_name)) %>%
    dplyr::mutate(name = paste(
      region_name_title %>%
        stringr::str_to_lower() %>%
        stringr::str_extract_all("[a-z]+") %>%
        unlist(),
      collapse = "_"))
)

## Functions

#' #' customize and create standard theme for plots
#' #'
#' #' a function to create a standardized theme for plots, updates ggtheme...
#' #'
#' #' @return no return value, simply updates the ggplot theme where called
#' plot_theme <- function(){
#'
#'   ggplot2::theme_update(
#'     axis.ticks = element_blank(),
#'     text = element_text(family = "Helvetica", color = elmts$dark_line, size = elmts$text_size),
#'     plot.title = element_text(size = ggplot2::rel(elmts$title_rel_size), hjust = 0.5, face = "bold"),
#'     panel.border = element_blank(),
#'     panel.grid.minor = element_blank(),
#'     panel.grid.major = element_line(colour = elmts$light_line, size = elmts$grid_major),
#'     legend.key = element_rect(colour = elmts$legend_colour, fill = elmts$legend_fill),
#'     legend.position = elmts$legend_pos,
#'     axis.line = element_line(colour = elmts$med2_line, size = elmts$axis_weight)
#'   )
#' }


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
  unique_rgn <- unique(rgn_scores$region_id)
  region_name_title <- rgn_name_lookup %>%
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
    theme(axis.line = element_blank(), element_line(colour = elmts$light_line)) +
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
make_flower_plot <- function(rgn_scores, plot_year = NA, color_pal, color_by = "goal", curve_labels = FALSE,
                             center_val = TRUE, legend_tab = FALSE, save = NA, dim = "score"){
  ## from PlotFlower.R from ohicore package
  ## original function by Casey O'Hara, Julia Lowndes, Melanie Frazier
  ## find original script in R folder of ohicore github repo (as of Mar 27, 2019)

  ## WRANGLING & CHECKS

  unique_rgn <- unique(rgn_scores$region_id)
  if(length(unique_rgn) != 1){
    print("rgn_scores input contains data for more than one region")
  }
  if(!"year" %in% names(rgn_scores)){
    if(is.na(plot_year)){
      plot_year <- substring(date(), 21, 24)
      print("rgn_scores doesn't have a year column; assuming data is for current year")
    } else {
      paste("rgn_scores doesn't have a year column; assuming data is for plot_year", plot_year)
    }
    rgn_scores$year <- plot_year
  } else {
    if(is.na(plot_year) | !plot_year %in% unique(rgn_scores$year)){
      plot_year <- max(rgn_scores$year)
      paste("plotting from most recent year in the input data:", plot_year)
    } else {
      paste("plotting from most recent year in the input data:", plot_year)
    }
  }
  ## filtering/wrangling of rgn_scores for rgn_ids and years
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
    message("plotting FIS and MAR with equal weighting\n")

  } else { wgts <- readr::read_csv(w_fn)
    if(is.na(yr)){
      wgts <- dplyr::filter(wgts, year == max(year))
    } else {wgts <- dplyr::filter(wgts, year == yr)}
    mean_wgt <- mean(wgts$w_fis) # mean across regions within the year of interest
    wgts <- rbind(
      data.frame(rgn_id = 0, w_fis = mean_wgt),
      dplyr::filter(wgts, rgn_id %in% unique_rgn) %>%
        dplyr::select(-year)
    )
  }

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

  ## color palette stuff
  if(color_by == "goal"){
    if(length(unique(plot_config$goal)) > length(color_pal)){
      print("color palette doesn't contain enougth colors for plotting by goal; using alternative palette")
      color_pal <-
    }
    if(length(unique(plot_config$goal)) < length(color_pal)){
      print("color palette contains too many colors for plotting by goal; did you supply a continuous palette?")
      color_pal <- color_pal[1:nrow(plot_config)*length(color_pal)/nrow(plot_config)] # space selection out across given palette
    }
  }

  for(r in unique_rgn){
    if(!is.null(w_fn)){
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
      dplyr::mutate(plot_NA = ifelse(is.na(score), 100, NA)) # for displaying NAs

    ## labels and general/basic parameters
    name_and_title <- elmts$rgn_name_lookup %>%
      dplyr::filter(region_id == r)
    score_index <- rgn_scores %>%
      dplyr::filter(region_id == r, goal == "Index", dimension == "score") %>%
      dplyr::select(region_id, score) %>%
      dplyr::mutate(score = round(score))
    goal_labels <- dplyr::select(plot_df, goal, name_flower)
    p_limits <- c(0, max(plot_df$pos_end))

    ## CREATING THE FLOWERPLOT

    ## set up plot background
    if(str_to_lower(color_by) == "goal"){
      plot_obj <- ggplot(plot_df, aes(x = pos, y = score, fill = goal, width = weight)) # color by goal
    } else {
      plot_obj <- ggplot(plot_df, aes(x = pos, y = score, fill = score, width = weight)) # color by score
    }
    plot_obj <- plot_obj +
      geom_bar(aes(y = 100), stat = "identity", size = 0.2,
               color = elmts$light_line, fill = elmts$white_fill)
    if(any(!is.na(plot_df$plot_NA))){ # overlay dark grey background for NAs
      plot_obj <- plot_obj +
        geom_bar(aes(y = plot_NA), stat = "identity", size = 0.2,
                 color = elmts$light_line, fill = elmts$light_fill)
    }

    ## flower plot petals
    plot_obj <- plot_obj +
      geom_bar(stat = "identity", size = 0.2, # overlay score actual value
               color = elmts$dark_line) +
      coord_polar(start = pi * 0.5) # from linear bar chart to polar

    if(str_to_lower(color_by) == "goal"){ # set color palette
      plot_obj <- plot_obj +
        scale_fill_manual(values = color_pal, na.value = "black")
    } else {
      plot_obj <- plot_obj +
        scale_fill_gradient(colours = color_pal, na.value = "black", limits = c(0, 100))
    }

    ## axis adjustments and labeling
    plot_obj <- plot_obj +
      geom_errorbar(aes(x = pos, ymin = 0, ymax = 0), # bolded baseline at zero
                    size = 0.5, show.legend = NA, color = elmts$dark_line) +
      scale_x_continuous(labels = plot_df$goal, # label the petals
                         breaks = plot_df$pos, limits = p_limits) +
      scale_y_continuous(
        limits = c(-blank_circle_rad, # tweak plot-limits of 'polarized' y-axix
                   ifelse(first(goal_labels == TRUE) | is.data.frame(goal_labels), 180, 100))) +
      labs(title = name_and_title$plot_title, x = NULL, y = NULL)


    return(invisible(plot_obj))
  }
}


# create_all_plots <- function(score_data, plot_year, ){
# }
