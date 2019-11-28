## Libraries
library(here)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(formattable)
library(circlize)
library(magick)
library(htmltools)
library(htmlwidgets)
library(webshot)

bhi_db_file <- "/Users/eleanorecampbell/Desktop/bhi-config.sqlite" # for now... depends on local path to sqlite db...
bhi_db_con <- DBI::dbConnect(RSQLite::SQLite(), bhi_db_file)

source(here::here("R", "theme.R"))
thm <- apply_bhi_theme(plot_type = "flowerplot") # theme_update for bhi flowerplot


## https://jokergoo.github.io/circlize_book/book/ about circlize package
## to install phantomjs, used only in flowerlegend function use webshot::install_phantomjs()


## Functions

## from PlotFlower.R from ohicore package
## original function by Casey O'Hara, Julia Lowndes, Melanie Frazier
## find original script in R folder of ohicore github repo (as of Mar 27, 2019)


#' create flowerplot
#'
#' This function combines multple helper functions to create the bhi flower plots
#' requires a dataframe of OHI scores filtered to the region of interest
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R),
#' filtered to region
#' @param rgns vector of bhi region ids by which to filter scores data and for which to create flowerplots
#' @param plot_year year by which to filter region score input dataframe;
#' defaults to current year or maximum year in score data input
#' @param dim the dimension the flowerplot petals should represent (typically OHI 'score')
#' @param include_ranges whether range bars should be calculated and displayed on plot, only for area-weighted averages ie region id = 0
#' @param dir_assess
#' @param dir_config
#' @param labels one of "none", "regular", "curved"
#' @param color_by either "goal" or "score"
#' @param color_pal a color palette that will map to values of specified dimension 'dim';
#' ideally discrete for color_by 'goal' and continuous otherise
#' @param region_id
#' @param plot_config
#' @param w_fn
#' @param wgts
#' @param color_df
#' @param plot_obj
#' @param plot_df
#' @param thm
#'
#' @return result is a flower plot; if curved labels or legend table are included,
#' the resulting class is magick-image, otherwise the result is a ggplot object

flowerdata <- function(rgn_scores, rgns = NA, plot_year = NA, dim = "score", include_ranges = FALSE, dir_assess){

  ## WRANGLING & CHECKS FOR FLOWERPLOT

  ## region scores data checks ----
  ## filtering/wrangling of rgn_scores for years and dimension
  unique_rgn <- unique(rgn_scores$region_id)
  if(length(unique_rgn) != 1){
    message("note: rgn_scores input contains data for more than one region, will plot all unless given rgn_id")
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

  rgn_scores_summary <- rgn_scores %>%
    group_by(goal, dimension, year) %>%
    summarize(
      missing_rgn = list(
        setdiff(0:42, region_id)),
      n_na = sum(is.na(score)),
      scores_range = list(range(score, na.rm = TRUE) %>% round(digits = 1))
    ) %>%
    ungroup()
  if(all(is.numeric(rgns))){
    if(!any(rgns == 0)){
      rgn_scores_summary <- rgn_scores_summary %>%
        select(-scores_range)
    }
    rgn_scores <- dplyr::filter(rgn_scores, region_id %in% rgns)
    unique_rgn <- unique(rgn_scores$region_id)
  }
  if(length(unlist(rgn_scores_summary$missing_rgn)) != 0 & isTRUE(include_ranges)){
    message("note: some missing regions for some goals... rgn_scores must be full scores.csv to include valid/actual ranges!")
  }

  ## fis and mar ----
  ## weights for fis vs. mar, uses layers/fp_wildcaught_weight.csv
  ## csv info determines relative width in the flowerplot of the two food provision subgoals
  w_fn <- list.files(
    file.path(dir_assess, "layers"),
    pattern = "fp_wildcaught_weight",
    full.names = TRUE
  )
  if(length(w_fn) != 1){
    if(length(w_fn) > 1){
      message("fp_wildcaught_weight.csv not unique...")
    } else {message("fp_wildcaught_weight.csv not found in layers...")}
    w_fn <- NULL
  }
  if(is.null(w_fn)){message("plotting FIS and MAR with equal weighting\n")}

  return(list(
    unique_rgn = unique_rgn,
    rgn_scores = rgn_scores,
    rgn_scores_summary = rgn_scores_summary,
    plot_year = plot_year,
    w_fn = w_fn
  ))
}

flowerconfig <- function(dir_config, labels = "none", color_by = "goal", color_pal = NA){

  ## PLOTTING CONFIGURATION, GENERAL

  ## sub/supra goals and positioning ----
  ## pos, pos_end, and pos_supra indicate positions, how wide different petals should be based on weightings
  plot_config <- readr::read_csv(
    file.path(dir_config, "conf", "plot_config_adjust.csv"),
    col_types = cols()
  )
  goals_supra <- na.omit(unique(plot_config$parent))

  supra_lookup <- plot_config %>%
    dplyr::filter(goal %in% goals_supra) %>%
    dplyr::select(parent = goal, name_supra = name_flower)

  plot_config <- plot_config %>%
    dplyr::left_join(supra_lookup, by = "parent") %>%
    dplyr::filter(!(goal %in% goals_supra)) %>%
    dplyr::select(-preindex_function, -postindex_function, -description, -order_calculate) %>%
    dplyr::mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
    dplyr::mutate(name_supra = gsub("\\n", "\n", name_supra, fixed = TRUE)) %>%
    dplyr::arrange(order_hierarchy)
  if(labels %in% c("curved", "arc")){
    plot_config <- plot_config %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        f = stringr::str_split(name_flower, "\n"),
        f1 = ifelse(order_hierarchy <= max(plot_config$order_hierarchy)/1.5, f[1],  f[2]),
        f2 = ifelse(order_hierarchy <= max(plot_config$order_hierarchy)/1.5, f[2], f[1])) %>%
      dplyr::select(-f)
  }

  color_by <- str_to_lower(color_by)
  if(color_by == "goal"){
    if(length(plot_config$goal) > length(color_pal)){
      color_df <- thm$palettes$goals_pal
      message("no palette given or too few colors for plotting by goal; using a predefined color palette\n")
    } else {
      color_pal <- color_pal[1:nrow(plot_config)*length(color_pal)/nrow(plot_config)]
      color_df <- tibble::tibble(goal = plot_config$goal, color = color_pal)
    }
  }

  return(list(
    plot_config = plot_config,
    thm = thm,
    color_df = color_df,
    color_pal = color_pal
  ))
}

rgnconfig <- function(region_id, rgn_scores, plot_config, w_fn, wgts, rgn_scores_summary, color_by = "goal", color_df = NULL, include_ranges){

  ## PLOTTING CONFIGURATION, REGION SPECIFIC

  r <- region_id
  if(!is.null(w_fn)){ # mar vs fis weights differ between regions
    plot_config$weight[plot_config$goal == "FIS"] <- wgts$w_fis[wgts$rgn_id == r]
    plot_config$weight[plot_config$goal == "MAR"] <- 1 - wgts$w_fis[wgts$rgn_id == r]
  }

  ## join plot_config w scores to create plot_df
  plot_df <- rgn_scores %>%
    dplyr::filter(region_id == r) %>%
    dplyr::inner_join(plot_config, by = "goal") %>%
    dplyr::arrange(order_hierarchy) %>%
    dplyr::mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
    dplyr::mutate(pos_end = sum(weight)) %>%
    dplyr::group_by(name_supra) %>%
    dplyr::mutate(pos_supra = ifelse(!is.na(name_supra), mean(pos), NA)) %>% # to deal with unequal weights
    dplyr::ungroup() %>%
    dplyr::filter(weight != 0) %>%
    dplyr::select(-dimension, -region_id, -year, -order_color, -parent, -name) %>%
    dplyr::mutate(plot_NA = ifelse(is.na(score), 100, NA)) # for displaying NAs

  if(color_by == "goal"){
    plot_df <- plot_df %>%
      ## for matching colors with correct petals, this arrangment is important...
      dplyr::arrange(goal) %>%
      dplyr::left_join(color_df, by = "goal")
    # color_pal <- dplyr::arrange(plot_df, order_calculate)$color
  }

  ## for including goal ranges for overall index
  if(r == 0 & isTRUE(include_ranges)){
    plot_df <- plot_df %>%
      left_join(
        rgn_scores_summary %>%
          select(goal, scores_range),
        by = "goal"
      ) %>%
      rowwise() %>%
      mutate(
        min_score = unlist(scores_range)[1],
        max_score = unlist(scores_range)[2]
      )
  }

  return(plot_df)
}

flowerformatting <- function(region_id, rgn_plot_obj, plot_df, flower_rgn_scores, include_ranges, labels, dim = "score", dir_assess){

  ## general flowerplot elements ----
  r <- region_id
  goal_labels <- dplyr::select(plot_df, goal, name_flower)
  name_and_title <- thm$rgn_name_lookup %>%
    dplyr::filter(region_id == r)
  blank_circle_rad <- -42

  ## labels, polar coordinates, adjust axes
  p <- rgn_plot_obj +
    labs(x = NULL, y = NULL) +
    coord_polar(start = pi * 0.5) + # from linear bar chart to polar
    scale_x_continuous(labels = NULL,
                       breaks = plot_df$pos,
                       limits = c(0, max(plot_df$pos_end))) +
    scale_y_continuous(limits = c(blank_circle_rad, ifelse(first(goal_labels == TRUE)|is.data.frame(goal_labels), 150, 100)))
  ## include average value in center
  score_index <- flower_rgn_scores %>%
    dplyr::filter(region_id == r, goal == "Index", dimension == dim) %>%
    dplyr::select(region_id, score) %>%
    dplyr::mutate(score = round(score))

  p <- p +
    geom_text(
      data = score_index, # include central value
      inherit.aes = FALSE, aes(label = score_index$score),
      x = 0, y = blank_circle_rad,
      hjust = 0.5, vjust = 0.5,
      size = 9,
      color = thm$cols$dark_grey3
    )

  # ranges for overall BHI flowerplot if include_ranges is true
  if(r == 0 & isTRUE(include_ranges)){
    p <- p +
      geom_errorbar(
        aes(ymin = min_score, ymax = max_score),
        alpha = 0.4,
        width = 0.05
      ) +
      geom_errorbar(
        aes(ymin = min_score, ymax = max_score),
        alpha = 0.3,
        width = 0.05,
        color = plot_df$color
      )
  }

  ## LABELS ----
  ## labeling with sub/supra goal names

  ## standard labels
  if(labels %in% c("regular", "standard", "normal", "level")){
    p <- p +
      # labs(title = name_and_title$plot_title) +
      geom_text(
        aes(label = name_supra, x = pos_supra, y = 150),
        size = 3.4,
        hjust = 0.4, vjust = 0.8,
        color = thm$cols$med_grey1
      ) +
      geom_text(
        aes(label = name_flower, x = pos, y = 120),
        size = 3,
        hjust = 0.5, vjust = 0.5,
        color = thm$cols$dark_grey3
      )
  }

  ## curved labels ----
  if(labels %in% c("curved", "arc")){
    temp_plot <- file.path(
      dir_assess, "reports", "flowerplots",
      sprintf("flowerplot%sbase_%s.png", name_and_title$region_id, name_and_title$name)
    )}

    ggplot2::ggsave(
      filename = temp_plot,
      plot = p + theme(plot.background = element_rect(fill = "transparent", colour = NA)),
      device = "png",
      height = 6, width = 8, units = "in", dpi = 300
    )

    temp_labels <- file.path(
      dir_assess, "reports", "flowerplots",
      paste0("flower_curvetxt_", name_and_title$name, ".png")
    )
    ## don't recreate curved labels if already exist....
    if(!file.exists(temp_labels)){
      circ_df <- plot_df %>%
        # dplyr::mutate(f1 = ifelse(weight <= 0.01, "", f1)) %>%
        # dplyr::mutate(f2 = ifelse(weight <= 0.01, "", f2)) %>%
        dplyr::select("goal", "f1", "f2", "name_supra", "weight", "order_hierarchy") %>%
        dplyr::mutate(weight = 0.15 + weight) %>% # spacing of labels around the circle based on weights...
        dplyr::mutate(x = sum(weight)-(cumsum(weight)-weight), x_end = sum(weight)-(cumsum(weight))) %>%
        tidyr::gather("start_end", "range", -goal, -name_supra, -weight, -order_hierarchy, -f1, -f2) %>%
        dplyr::select(-start_end, -weight, -goal) %>%
        dplyr::arrange(order_hierarchy)

      ## start creating plot and save with grDevices function
      # jpeg(temp_labels, width = 2450, height = 2450, quality = 220) # jpeg format is lossy, png seems better...
      png(temp_labels, width = 2490, height = 2490, bg = "transparent")
      message("creating curved labels for plot:\n")

      ## curved labels created with 'circlize' package ----
      ## setup/initialize new circlize plot
      circos.clear()
      circos.par(
        "track.height" = 0.1,
        cell.padding = c(0.02, 0, 0.02, 0),
        "clock.wise" = FALSE,
        start.degree = 5
      )
      circos.initialize(
        factors = circ_df$order_hierarchy,
        x = circ_df$range
      )

      ## make tracks
      circos.track(
        factors = circ_df$order_hierarchy,
        y = circ_df$range,
        panel.fun = function(x, y){
          circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index, col = "white")},
        bg.col = NA,
        bg.border = FALSE
      )
      circos.track(
        factors = circ_df$order_hierarchy,
        y = circ_df$range,
        panel.fun = function(x, y){
          circos.text(
            CELL_META$xcenter,
            CELL_META$ycenter,
            circ_df$f1[circ_df$order_hierarchy == as.numeric(CELL_META$sector.index)][1],
            cex = 5,
            col = thm$cols$med_grey3,
            adj = c(0.4, 1),
            facing = "bending.inside",
            niceFacing = TRUE
          )
        },
        bg.col = NA,
        bg.border = FALSE
      )
      circos.track(
        factors = circ_df$order_hierarchy,
        y = circ_df$range,
        panel.fun = function(x, y){
          circos.text(
            CELL_META$xcenter,
            CELL_META$ycenter,
            circ_df$f2[circ_df$order_hierarchy == as.numeric(CELL_META$sector.index)][1],
            cex = 5,
            col = thm$cols$med_grey3,
            adj = c(0.5, 0),
            facing = "bending.inside",
            niceFacing = TRUE
          )
        },
        bg.col = NA,
        bg.border = FALSE
      )

      ## add supra goal labeling
      highlight.sector(
        circ_df$order_hierarchy[circ_df$name_supra != "Food Provision"|is.na(circ_df$name_supra)],
        track.index = 1,
        text = "Food Provision",
        cex = 6.5,
        padding = c(0, 0, 0, 2.67),
        text.col = thm$cols$light_grey2,
        col = NA,
        facing = "bending.outside",
        niceFacing = TRUE
      )
      highlight.sector(
        circ_df$order_hierarchy[circ_df$name_supra != "Coastal Livelihoods & Economies"|is.na(circ_df$name_supra)],
        track.index = 1,
        text = "Coastal Livelihoods & Economies",
        cex = 6.5,
        text.col = thm$cols$light_grey2,
        col = NA,
        facing = "bending.outside",
        niceFacing = TRUE
      )
      highlight.sector(
        circ_df$order_hierarchy[circ_df$name_supra != "Sense of Place"|is.na(circ_df$name_supra)],
        track.index = 1,
        text = "Sense of Place",
        cex = 6.5,
        padding = c(0, 0, 0, 2.62),
        text.col = thm$cols$light_grey2,
        col = NA,
        facing = "bending.outside",
        niceFacing = TRUE
      )
      highlight.sector(
        circ_df$order_hierarchy[circ_df$name_supra != "Clean Waters"|is.na(circ_df$name_supra)],
        track.index = 1,
        text = "Clean Waters",
        cex = 6.5,
        padding = c(0, 0.16, 0, 0),
        text.col = thm$cols$light_grey2,
        col = NA,
        facing = "bending.outside",
        niceFacing = TRUE
      )
      dev.off() # end saving labels image with grDevices function
    }
    ## combine plot and labels into one graphic
    img_text <- magick::image_read(temp_labels)
    img_plot <- magick::image_read(temp_plot)
    plot_labeled <- image_composite(
      img_plot,
      image_scale(img_text, 1810),
      offset = "+305-15"
    ) %>% image_crop(geometry_area(1750, 1620, 325, 120))

    ## rescale and save the final plot
    magick::image_write(
      image_scale(plot_labeled, 600), # can adjust here to make smaller, sacrificing image quality...
      path = plotpath <- file.path(
        dir_assess, "reports", "flowerplots",
        sprintf("flowerplot%s_%s.png", name_and_title$region_id, name_and_title$name)
      ),
      format = "png"
    )

  return(plot_labeled)
}

flowerplot <- function(rgn_scores, rgns = NA, plot_year = NA, dim = "score", include_ranges = FALSE, dir_assess,
                       dir_config, labels = "none", color_by = "goal", color_pal = NA){


  ## flowerplot data
  rflowerdata <- flowerdata(rgn_scores, rgns, plot_year, dim, include_ranges, dir_assess)

  ## flowerplot configuration
  rflowerconfig <- flowerconfig(dir_config, labels, color_by, color_pal)

  ## petal weights for food provision goal ----
  if(!is.null(rflowerdata$w_fn)){
    wgts <- readr::read_csv(rflowerdata$w_fn)
    if("year" %in% names(wgts)){
      wgts <- wgts %>%
        dplyr::filter(year == rflowerdata$plot_year) %>%
        dplyr::select(-year)
    }
    if(length(wgts$w_fis) != 0){
      mean_wgt <- mean(wgts$w_fis) # mean across regions within the year of interest

      ## area weighted subbasin means
      wgts_basin <- tbl(bhi_db_con, "regions") %>%
        select(rgn_id = region_id, area_km2, subbasin) %>%
        collect() %>%
        left_join(wgts, by = "rgn_id") %>%
        group_by(subbasin) %>%
        summarize(w_fis = weighted.mean(w_fis, area_km2)) %>%
        left_join(
          tbl(bhi_db_con, "basins") %>%
            select(subbasin, rgn_id = subbasin_id) %>%
            collect(),
          by = "subbasin"
        ) %>%
        ungroup() %>%
        select(rgn_id, w_fis)

      wgts <- rbind(
        data.frame(rgn_id = 0, w_fis = mean_wgt),
        dplyr::filter(wgts, rgn_id %in% rflowerdata$unique_rgn),
        wgts_basin
      )
    } else {
      message(paste("fp_wildcaught_weight.csv doesn't have data for the plot_year:", rflowerdata$plot_year))
      rflowerdata$w_fn <- NULL
    }
  }

  ## start looping over regions
  for(r in rflowerdata$unique_rgn){
    ## flowerplot region-specific configuration
    ## note some of these arguments come from flowerdata or flowerconfig functions
    plot_df <- rgnconfig(
      r,
      rflowerdata$rgn_scores,
      rflowerconfig$plot_config,
      rflowerdata$w_fn,
      wgts,
      rflowerdata$rgn_scores_summary,
      color_by,
      rflowerconfig$color_df,
      include_ranges
    )

    ## CREATING THE FLOWERPLOTS ----
    ## without a gradient along the petals
    if(color_by == "goal"){
      plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = goal)) +
        geom_bar(
          aes(y = 100),
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          fill = "white"
        ) +
        geom_bar(
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          show.legend = FALSE
        ) +
        scale_fill_manual(values = plot_df$color, na.value = "black")

    } else {
      plot_obj <- ggplot(plot_df, aes(x = pos, y = score, width = weight, fill = score)) +
        geom_bar(
          aes(y = 100),
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          fill = "white"
        ) +
        geom_bar(
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          show.legend = FALSE
        ) +
        scale_fill_gradientn(colors = color_pal, na.value = "black", limits = c(0, 100))
    }

    if(any(!is.na(rgn_plot_df$plot_NA))){ # overlay light grey background for NAs
      plot_obj <- plot_obj +
        geom_bar(
          aes(y = plot_NA),
          stat = "identity",
          size = 0.2,
          color = thm$cols$med_grey3,
          fill = thm$cols$light_grey1
        )
    }
    plot_obj <- plot_obj +
      geom_errorbar(
        aes(x = pos, ymin = 0, ymax = 0), # bolded baseline at zero
        size = 0.5,
        show.legend = NA,
        color = thm$cols$dark_grey3
      ) +
      geom_errorbar(
        aes(x = pos, ymin = 0, ymax = 0), # some kind of tipping-point line? currently zero...
        size = 0.25,
        show.legend = NA,
        color = thm$cols$accent_bright
      ) +
      geom_errorbar(
        aes(x = pos, ymin = 109, ymax = 109), # outer ring indicating room for even more progress?
        size = 5,
        show.legend = NA,
        color = thm$cols$light_grey1
      )

    ## format plot
    ## for details see flowerformatting function above
    plotformatted <- flowerformatting(r, plot_obj, plot_df, rflowerdata$rgn_scores, include_ranges, labels, dim, dir_assess)
  }
  return(plotformatted) # returns only the last plot...
}

flowerwgradient <- function(rgn_scores, rgns = NA, plot_year = NA, dim = "score", include_ranges = FALSE, dir_assess = dir_assess,
                            dir_config = dir_assess, labels = "none", color_by = "goal", color_pal = NA){


  ## flowerplot data
  rflowerdata <- flowerdata(rgn_scores, rgns, plot_year, dim, include_ranges, dir_assess)

  ## flowerplot configuration
  rflowerconfig <- flowerconfig(dir_config, labels, color_by, color_pal)

  ## petal weights for food provision goal ----
  if(!is.null(rflowerdata$w_fn)){
    wgts <- readr::read_csv(rflowerdata$w_fn)
    if("year" %in% names(wgts)){
      wgts <- wgts %>%
        dplyr::filter(year == rflowerdata$plot_year) %>%
        dplyr::select(-year)
    }
    if(length(wgts$w_fis) != 0){
      mean_wgt <- mean(wgts$w_fis) # mean across regions within the year of interest

      ## area weighted subbasin means
      wgts_basin <- tbl(bhi_db_con, "regions") %>%
        select(rgn_id = region_id, area_km2, subbasin) %>%
        collect() %>%
        left_join(wgts, by = "rgn_id") %>%
        group_by(subbasin) %>%
        summarize(w_fis = weighted.mean(w_fis, area_km2)) %>%
        left_join(
          tbl(bhi_db_con, "basins") %>%
            select(subbasin, rgn_id = subbasin_id) %>%
            collect(),
          by = "subbasin"
        ) %>%
        ungroup() %>%
        select(rgn_id, w_fis)

      wgts <- rbind(
        data.frame(rgn_id = 0, w_fis = mean_wgt),
        dplyr::filter(wgts, rgn_id %in% rflowerdata$unique_rgn),
        wgts_basin
      )
    } else {
      message(paste("fp_wildcaught_weight.csv doesn't have data for the plot_year:", rflowerdata$plot_year))
      rflowerdata$w_fn <- NULL
    }
  }

  ## plotting by region
  r <- rflowerdata$unique_rgn[1]
  message("since plotting with a gradient is so intensive, plotting only for first region!!!\n")

  plot_df <- rgnconfig(
    r,
    rflowerdata$rgn_scores,
    rflowerconfig$plot_config,
    rflowerdata$w_fn,
    wgts,
    rflowerdata$rgn_scores_summary,
    color_by,
    rflowerconfig$color_df,
    include_ranges
  )

  ## CREATING THE FLOWERPLOTS ----
  ## if plotting with a gradient, expand plot_df with y column indicating
  ##  plot_df and  plot_df0 will be same if no gradient on petals
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

  ## CREATING THE FLOWERPLOTS ----
  ## with a gradient
  plot_obj <- ggplot(plot_df, aes(x = x, xend = x_end, y = y, yend = y)) +
    geom_rect(
      inherit.aes = FALSE,
      aes(xmin = x, xmax = x_end, ymin = 0, ymax = 100),
      size = 0.15,
      color = thm$cols$light_grey2,
      fill = "white"
    )
  if(color_by == "goal"){
    plot_obj <- plot_obj +
      geom_segment(
        aes(color = goal),
        size = 0.15, alpha = 0.15,
        show.legend = FALSE,
        arrow = arrow(length = unit(0.01, "cm"))
      ) +
      scale_color_manual(
        values = unique(plot_df$color),
        na.value = "black"
      )
  } else {
    plot_obj <- plot_obj +
      geom_segment(
        aes(color = y),
        size = 0.2, alpha = 0.3,
        show.legend = FALSE,
        arrow = arrow(length = unit(0.02, "cm"))
      ) +
      scale_color_gradient2(
        low = color_pal[1],
        mid = color_pal[length(color_pal)/2],
        high = color_pal[length(color_pal)], midpoint = 50
      )
  }
  if(any(!is.na(plot_df$plot_NA))){ # overlay light grey background for NAs
    plot_obj <- plot_obj +
      geom_rect(
        data = filter(plot_df, !is.na(plot_NA)),
        inherit.aes = FALSE,
        aes(xmin = x, xmax = x_end, ymin = 0, ymax = plot_NA),
        fill = thm$cols$light_grey1
      )
  }
  plot_obj <- plot_obj +
    geom_segment(
      aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 0, yend = 0),
      size = 0.5,
      color = thm$cols$dark_grey3
    ) +
    geom_segment(
      aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = critical_value, yend = critical_value),
      size = 0.1,
      color = thm$cols$accent_bright
    ) +
    geom_segment(
      aes(x = min(plot_df$x), xend = max(plot_df$x_end), y = 109, yend = 109),
      size = 5,
      color = thm$cols$light_grey1
    )

  ## format plot
  ## for details see flowerformatting function above
  plotformatted <- flowerformatting(r, plot_obj, plot_df, rflowerdata$rgn_scores, include_ranges, labels, dim, dir_assess)
  return(plotformatted)
}

flowerlegendtab <- function(plot_config, color_by = "goal", color_df, dir_assess = dir_assess){
  ## creating a legend table to accompany flowerplot
  message("creating legend table for flowerplot:\n")

  temp_legend <- file.path(
    dir_assess, "reports", "flowerplots",
    paste0("flowerlegend_by_", color_by, "_", name_and_title$name, ".jpg")
  )

  ## formattable dataframe ----
  legend_df <- plot_config %>%
    dplyr::select(goal, parent, name, name_supra) %>%
    dplyr::left_join(
      dplyr::filter(rgn_scores, region_id == r),
      by = "goal"
    )

  if(color_by == "goal"){
    legend_cols <- dplyr::mutate(
      left_join(legend_df, color_df,  by = "goal"),
      color = ifelse(is.na(score), NA, color)
    )$color

  } else {
    legend_cols <- dplyr::mutate(
      legend_df, color = color_pal[length(color_pal)*score/100]
    )$color
  }

  legend_cols[is.na(legend_cols)] <- "#DDDDDD"

  legend_df <- legend_df %>%
    dplyr::mutate(
      Key = "*******",
      Goal = paste(
        ifelse(is.na(name_supra),"",
               paste(name_supra, ": ")),
        name,"(",ifelse(is.na(parent), goal, paste(parent, goal)),")")
    ) %>%
    dplyr::select(Goal, Key, score)

  names(legend_df) <- c("Goal", "Key", stringr::str_to_title(dim))


  ## formattable formatted table ----
  tab <- formattable(
    legend_df,
    align = c("l","l","r"),

    list(
      `Goal` = formatter("span", style = ~ style(color = "grey")),
      `Score` = formatter("span", style = x ~ style(color = ifelse(is.na(x), "white", "grey"))),
      `Key` = formatter("span", style = x ~ style("background-color" = legend_cols, color = ifelse(is.na(x), "#DDDDDD", legend_cols)))
    ))

  ## NOTE: for the following to work phantomjs must be installed!
  ## this can be done with webshot::install_phantomjs()
  path <- htmltools::html_print(
    as.htmlwidget(tab, width = "48%", height = NULL),
    background = "white",
    viewer = NULL
  )

  ## webshot and save widget
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot::webshot(
    url,
    file = temp_legend,
    selector = ".formattable_widget",
    delay = 0.2
  )
  img_legend <- magick::image_read(temp_legend) %>%
    magick::image_border("white", "20x92") %>%
    magick::image_scale(600)

  return(img_legend)

}
