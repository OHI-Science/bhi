## Libraries
source(file.path(here::here(), "R", "common.R"))
library(ggplot2)
library(plotly)
library(ggrepel)
library(ggthemes)
library(htmlwidgets)
library(magick)
library(webshot) # webshot::install_phantomjs()
library(htmltools)
library(formattable)


## Functions

#' create trends barplot by region
#'
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R),
#' filtered to region
#' @param color_pal a continuous color palette, ideally diverging,
#' that will map to trend values in the barplots
#' @param plot_year year by which to filter region score input dataframe
#' @param include_legend boolean indicating if legend should be included or not
#' @param save the plot will not be saved if 'save' is FALSE or NA, will be saved to file.path(save) if a string,
#' or to "reports/figures" directory if TRUE
#'
#' @return

make_rgn_trends_barplot <- function(rgn_scores, color_pal = NA, plot_year = NA, include_legend = FALSE, save = NA){

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
  thm <- apply_bhi_theme("trends_barplot")
  if(is.na(color_pal)){
    color_pal <- c(thm$bhi_palettes$reds, thm$bhi_palettes$blues)
  }

  unique_rgn <- unique(rgn_scores$region_id)
  region_name_title <- thm$rgn_name_lookup %>%
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
    region_title <- region_name_title$plot_title
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
    theme_light() +
    theme(axis.line = element_blank(), element_line(colour = thm$elmts$light_line)) +
    labs(x = NULL, y = NULL)

  if(length(unique_rgn) > 1){
    trends_barplot <- trends_barplot +
      theme(axis.text.x = element_text(size = 6, angle = 90)) +
      scale_fill_gradientn(colors = color_pal) +
      facet_wrap( ~ plot_title, ncol = facet_ncol)
  } else {
    trends_barplot <- trends_barplot +
      geom_label_repel(aes(label = goals_reordered),
                      size = 2.5, nudge_y = 0.01,
                      segment.alpha = 0, family = "Helvetica",
                      show.legend = FALSE) +
      theme(axis.text.y = element_blank()) +
      scale_fill_gradientn(colors = color_pal) +
      labs(title = region_title)  +
      coord_flip()
  }

  ## saving plots ----
  if(isFALSE(save)){save <- NA}
  if(isTRUE(save)){
    save <- file.path(dir_assess, "reports", "figures")
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

#' create trends barplot by goal
#'
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R)
#' @param plot_year year by which to filter region score input dataframe
#' @param by the spatial unit by which to aggregate and plot; one of region, subbasin, or eez
#' @param color_pal a continuous color palette, ideally diverging,
#' that will map to trend values in the barplots
#' @param include_legend boolean indicating if legend should be included or not
#' @param save the plot will not be saved if 'save' is FALSE or NA, will be saved to file.path(save) if a string,
#' or to "reports/figures" directory if TRUE
#'
#' @return

trends_barplots_by_goal <- function(scores, plot_year = NA, by = NA,
                                    color_pal = NA, include_legend = FALSE, save = NA){

  ## checks, filtering, wrangling ----
  if(!"year" %in% names(scores)){
    if(is.na(plot_year)){
      plot_year <- substring(date(), 21, 24)
      message("scores doesn't have a year column; assuming data is for current year\n")
    } else {
      message(paste("scores doesn't have a year column; assuming data is for given plot_year", plot_year,"\n"))
    }
    scores$year <- plot_year
  } else {
    if(!plot_year %in% unique(scores$year)){
      message("no data for given plot_year in the scores input")
    }
    if(is.na(plot_year) | !plot_year %in% unique(scores$year)){
      plot_year <- max(scores$year)
      message(paste("plotting using most recent year in the input data:", plot_year,"\n"))
    }
  }

  thm <- apply_bhi_theme("trends_barplot")
  if(any(is.na(color_pal))||length(color_pal) == 0){
    color_pal <- c(thm$bhi_palettes$reds, thm$bhi_palettes$blues)
  }

  goal_subgoal_list <- unique(scores$goal)
  goal_names_titles <- readr::read_csv(file.path(dir_assess, "conf", "goals.csv"), col_types = cols()) %>%
    select(order_hierarchy, goal, name)

  scores <- scores %>%
    dplyr::filter(year == plot_year) %>%
    dplyr::filter(dimension == "trend") %>%
    left_join(readr::read_csv(file.path(dir_spatial, "bhi_basin_country_lookup.csv"),
                              col_types = cols()) %>%
                rename(region_id = BHI_ID, area_km2_rgn = Area_km2, subbasin = Subbasin) %>%
                select(-HELCOM_ID), by = "region_id")

  for(g in goal_subgoal_list){

    goal_scores <- filter(scores, goal == g)

    if(!is.na(by)){
      by <- str_to_lower(by)
      if(by == "basin"|| by =="subbasin"){
        goal_scores <- goal_scores %>%
          dplyr::group_by(goal, subbasin) %>%
          dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm = TRUE)) %>%
          ungroup()
        ordering <- readr::read_csv(
            file.path(dir_spatial, "subbasins_ordered.csv"),
            col_types = cols()) %>%
          left_join(goal_scores, by = "subbasin") %>%
          arrange(order)
        goal_scores$north_south_reordered <- factor(
          goal_scores$subbasin,
          levels = ordering$subbasin)

      } else if(by == "eez"){
        goal_scores <- goal_scores %>%
          dplyr::group_by(goal, rgn_key) %>%
          dplyr::summarise(score = weighted.mean(score, area_km2_rgn, na.rm = TRUE)) %>%
          ungroup() %>%
          rename(eez = rgn_key)
        goal_scores$north_south_reordered <- factor(
          goal_scores$eez,
          levels = c("DNK", "DEU", "POL",
                     "LVA", "LTU", "SWE",
                     "EST", "RUS", "FIN"))

      } else {
        print("for each goal or subgoal, plotting trend vs. all 42 BHI regions")
      }
    }

    plot_title <- ifelse(g == "Index", "Index", filter(goal_names_titles, goal == g)$name)
    non_NaN <- goal_scores$score[goal_scores$score != "NaN"]
    if(length(non_NaN) == 0){
      message(paste("cannot plot for", g, "goal, lacking valid data for trend"))
    } else {
      start_pal <- (length(color_pal)/2)*(1 + min(non_NaN))
      end_pal <- (length(color_pal)/2)*(1 + max(non_NaN))
      plot_color_pal <- color_pal[start_pal:(end_pal+1)]

      ## creating plots ----
      trends_barplot <- goal_scores %>%
        ggplot(aes(x = north_south_reordered, y = score, fill = score)) +
        geom_bar(stat = "identity", position = position_dodge(), show.legend = include_legend) +
        geom_hline(yintercept = 0) +
        geom_label_repel(aes(label = north_south_reordered), # geom_text_repel
                        size = 3, angle = 0, segment.alpha = 0.3, direction = "x", segment.size = 0.1, # nudge_y = 0.01, # angle = 90
                        family = "Helvetica", show.legend = FALSE) +
        theme_light() +
        theme(axis.line = element_blank(), element_line(colour = thm$elmts$light_line)) +
        labs(title = plot_title, y = NULL, x = NULL) + # x = "Subbasin") +
        # theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # element_text(angle = 90)) +
        theme(axis.text.y =  element_blank()) + # element_text(hjust = 1)) +
        coord_flip() +
        scale_fill_gradientn(colors = plot_color_pal)


      ## saving plots ----
      if(isFALSE(save)){save <- NA}
      if(isTRUE(save)){
        save <- file.path(dir_assess, "reports", "figures")
      }
      if(!is.na(save)){
        save_loc <- file.path(save, paste0(by, "s_trendbarplot_", str_to_lower(g), ".png"))
        if(!file.exists(save)){ message("that save location is not valid") } else {
          ggplot2::ggsave(filename = save_loc, plot = trends_barplot, device = "png",
                          height = 5, width = 8.5, units = "in", dpi = 300)
        }
      }
    }
  }

  return(invisible(trends_barplot))
}


#' create trend table
#'
#' requires a dataframe of OHI scores filtered to the region of interest
#' reads in information from supplement/tables/
#'
#' @param rgn_scores scores dataframe e.g. output of ohicore::CalculateAll (typically from calculate_scores.R),
#' filtered to region
#' @param plot_year year by which to filter region score input dataframe;
#' defaults to current year or maximum year in score data input
#' @param dim the dimension the flowerplot petals should represent (typically OHI 'score')
#' @param thresholds two element vector with thresholds values indicating where colors and up/down arrows should switch
#' @param save the plot will not be saved if 'save' is FALSE or NA, will be saved to file.path(save) if a string,
#' or to "reports/figures" directory if TRUE
#'
#' @return result is a formattable table, saved only if save location is specified as TRUE or a filepath

future_dims_table <- function(rgn_scores, plot_year = NA, dim = "trend",
                              thresholds = c(-0.1, 0.1), include_vals, save = NA){

  ## wrangle scores with basin info into form for table ----
  dim_df <- filter_score_data(rgn_scores, dims = dim, years = plot_year)[[1]] %>%
    select(-dimension) %>%
    rename(BHI_ID = region_id) %>%
    filter(goal != "Index")
  status_df <- filter_score_data(rgn_scores, dims = "status", years = plot_year)[[1]] %>%
    select(-dimension) %>%
    rename(BHI_ID = region_id) %>%
    filter(goal != "Index")

  area_wgt_mean <- function(df){
    readr::read_csv(file.path(dir_bhi, "spatial", "bhi_basin_country_lookup.csv"), col_types = cols()) %>%
      dplyr::right_join(df, by = "BHI_ID") %>%
      mutate(score = ifelse(score == "NaN", NA, score)) %>%
      group_by(Subbasin, goal) %>%
      summarize(
        basin_means = weighted.mean(score, Area_km2, na.rm = TRUE) %>%
          round(digits = 4)
      ) %>%
      filter(!is.na(Subbasin)) %>%
      mutate(basin_means = ifelse(basin_means == "NaN", NA, basin_means)) %>%
      tidyr::spread(key = goal, value = basin_means) %>%
      left_join(read_csv(file.path(dir_spatial, "subbasins_ordered.csv"), col_types = cols()) %>%
                  rename(Subbasin = subbasin),
                by = "Subbasin") %>%
      arrange(order) %>%
      select(-CS, -order) %>%
      ungroup()
  }
  table_df <- area_wgt_mean(dim_df)
  formatting_df <- area_wgt_mean(status_df)

  ## row formatter to include arrow icons ----
  goals_formatter <- formatter("span", style = x ~ style(
    color = ifelse(is.na(x), "white",
                   ifelse(x < thresholds[1], "firebrick",
                          ifelse(x > thresholds[2], "darkcyan", "gainsboro")))),
    x ~ icontext(ifelse(x < thresholds[1], "circle-arrow-down",
                        ifelse(x > thresholds[2], "circle-arrow-up", "circle-arrow-right")),
                 round(x, digits = 2)))

  general_formatter <- formatter("span", style = x ~ style(
    color = ifelse(is.na(x), "white",
                   ifelse(x < thresholds[1], "firebrick",
                          ifelse(x > thresholds[2], "darkcyan", "gainsboro")))),
    x ~ icontext(ifelse(x < thresholds[1], "circle-arrow-down",
                        ifelse(x > thresholds[2], "circle-arrow-up", "circle-arrow-right")),
                 round(x, digits = 2)))

  format_by_goal <- function(g){
    formatter("span", style = x ~ style(
      color = ifelse(is.na(x), "white", ifelse(x < formatting_df[, g], "firebrick", "darkcyan"))),
      x ~ icontext(ifelse(x < formatting_df[, g], "circle-arrow-down", "circle-arrow-up"), round(x, digits = 2)))
  }

  ## create the table ----
  tab <- formattable(table_df, align = c("l", rep("c", ncol(table_df)-1)), list(
    `Subbasin` = formatter("span", style = ~ style(color = "grey")),
    `AO` = general_formatter, `BD` = general_formatter, `CON` = general_formatter,
    `CW` = general_formatter, `ECO` = general_formatter, `EUT` = general_formatter,
    `FIS` = general_formatter, `FP` = general_formatter, `ICO` = general_formatter,
    `LE` = general_formatter, `LIV` = general_formatter, `LSP` = general_formatter,
    `MAR` = general_formatter, `NP` = general_formatter, `SP` = general_formatter,
    `TR` = general_formatter, `TRA` = general_formatter
  ))

  tab <- formattable(table_df, align = c("l", rep("c", ncol(table_df)-1)), list(
    `Subbasin` = formatter("span", style = ~ style(color = "grey")),
    `AO` = format_by_goal("AO"), `BD` = format_by_goal("BD"), `CON` = format_by_goal("CON"),
    `CW` = format_by_goal("CW"), `ECO` = format_by_goal("ECO"), `EUT` = format_by_goal("EUT"),
    `FIS` = format_by_goal("FIS"), `FP` = format_by_goal("FP"), `ICO` = format_by_goal("ICO"),
    `LE` = format_by_goal("LE"), `LIV` = format_by_goal("LIV"), `LSP` = format_by_goal("LSP"),
    `MAR` = format_by_goal("MAR"), `NP` = format_by_goal("NP"), `SP` = format_by_goal("SP"),
    `TR` = format_by_goal("TR"), `TRA` = format_by_goal("TRA")
  ))
  # for(g in setdiff(colnames(table_df), "Subbasin")){
  #   print(data.frame(table_df$Subbasin, table_df[,g], formatting_df[,g], table_df[,g] >= formatting_df[,g]))
  # }


  ## save table ----
  ## must have phantomjs installed- can do this with webshot::install_phantomjs()
  if(isFALSE(save)){ save <- NA }
  if(isTRUE(save)){
    save <- file.path(dir_assess, "reports", "figures", paste0(dim, "s_table", ".png"))
  }
  if(!is.na(save)){
    save_loc <- save
    path <- htmltools::html_print(as.htmlwidget(tab, width = "100%", height = NULL),
                                  background = "white", viewer = NULL)
    url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
    webshot::webshot(url, file = save, selector = ".formattable_widget", delay = 0.2)
  }

  return(tab)
}


#' create barplot to accompany maps
#'
#' create a barplot with subbasin scores, arranged vertically approximately north-to-south
#' intended to present side-by-side with map, to show distances from reference points/room for improvement
#'
#' @param scores_csv scores dataframe with goal, dimension, region_id, year and score columns,
#' e.g. output of ohicore::CalculateAll typically from calculate_scores.R
#' @param goal_code the two or three letter code indicating which goal/subgoal to create the plot for
#' @param uniform_width if TRUE all subbasin bars will be the same width, otherwise a function of area
#' @param make_html if TRUE, will create an hmtl/plottly version rather than ggplot to use e.g. for the website or shiny app
#' @param save can be either a directory in which to save the plot, or if TRUE will save to a default location
#'
#' @return

scores_barplot <- function(scores_csv, basins_or_rgns = "subbasins", goal_code,
                           uniform_width = FALSE, make_html = FALSE, save = FALSE){

  scores <- scores_csv
  if("dimension" %in% colnames(scores)){
    scores <- scores %>%
      filter(dimension == "score") %>%
      select(-dimension)
  }
  if("goal" %in% colnames(scores)){
    scores <- scores %>%
      filter(goal == goal_code) %>%
      select(-goal)
  }

  ## apply bhi_theme, in this case the same as for flowerplot
  thm <- apply_bhi_theme(plot_type = "flowerplot")

  ## wrangle and join info to create plotting dataframe ----
  if(basins_or_rgns == "subbasins"){

    order_df <- tbl(bhi_db_con, "basins") %>%
      select(name = subbasin, order) %>%
      collect() %>%
      mutate(order = as.factor(order))

    areas_df <- tbl(bhi_db_con, "basins") %>%
      select(name = subbasin, area_km2)

    scores <- scores %>%
      filter(region_id >= 500) %>%
      collect() %>%
      left_join(tbl(bhi_db_con, "basins") %>%
                  select(region_id = subbasin_id, name = subbasin) %>%
                  collect(),
                by = "region_id") %>%
      select(name, score)

  } else {
    order_df <- tbl(bhi_db_con, "regions") %>%
      select(name = region_id, order) %>%
      collect() %>%
      mutate(order = as.factor(order))

    areas_df <- tbl(bhi_db_con, "regions") %>%
      select(name = region_id, area_km2)

    scores <- scores %>%
      filter(region_id < 100 & region_id != 0) %>%
      rename(name = region_id) %>%
      collect() %>%
      left_join(tbl(bhi_db_con, "regions") %>%
                  select(name = region_id, region_name) %>%
                  collect(), by = "name")
  }

  ## use uniform_width argument to define whether bars are uniform or scaled by a function of area
  if(uniform_width){
    weights <- areas_df %>%
      mutate(weight = 1) %>%
      select(-area_km2) %>%
      collect()
  } else {
    weights <- areas_df %>%
      ## scaling proportional to area results in some very thin bars
      ## can try different functions...
      mutate(weight = area_km2^0.6) %>% # mutate(weight = area)
      collect()
  }

  if(basins_or_rgns == "subbasins"){
    plot_df <- scores %>%
      dplyr::left_join(weights, by = "name") %>%
      dplyr::left_join(order_df, by = "name") %>%
      dplyr::mutate(Name = name,
                    score_unrounded = score,
                    Score = round(score, 2),
                    Area = paste(round(area_km2), "km2"))
  } else {
    plot_df <- scores %>%
      dplyr::left_join(weights, by = "name") %>%
      dplyr::left_join(order_df, by = "name") %>%
      dplyr::mutate(Name = as.factor(region_name),
                    score_unrounded = score,
                    Score = round(score, 2),
                    Area = paste(round(area_km2), "km2"))
  }
  plot_df <- plot_df %>%
    dplyr::arrange(order) %>%
    dplyr::mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
    dplyr::mutate(pos_end = sum(weight)) %>%
    dplyr::mutate(plotNAs = ifelse(is.na(score_unrounded), 100, NA)) %>% # for displaying NAs
    dplyr::select(order, Name, weight, Area,
                  score_unrounded, Score,
                  pos, pos_end, plotNAs)


  ## create plot ----
  plot_obj <- ggplot(plot_df,
                     aes(x = pos, y = score_unrounded,
                         text =  sprintf("%s:\n%s", str_replace(Name, ", ", "\n"), Score),
                         # Name = Name, Score = Score, Area = Area,
                         width = weight, fill = score_unrounded)) +
    geom_bar(aes(y = 100),
             stat = "identity",
             size = 0.2,
             color = "#acb9b6",
             alpha = 0.6,
             fill = "white") +
    geom_bar(stat = "identity",
             size = 0.2,
             color = "#acb9b6",
             alpha = 0.85,
             show.legend = FALSE) +
    scale_fill_gradientn(colours = c("#8c031a", "#cc0033", "#fff78a", "#f6ffb3", "#009999", "#0278a7"),
                         breaks = c(15, 40, 60, 75, 90, 100),
                         limits = c(0, 101),
                         na.value = "black")

  ## overlay light grey for NAs
  if(any(!is.na(plot_df$plotNAs))){
    plot_obj <- plot_obj +
      geom_bar(aes(y = plotNAs),
               stat = "identity",
               size = 0.2,
               color = "#acb9b6",
               fill = "#fcfcfd")
  }
  ## some formatting
  plot_obj <- plot_obj +
    geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                  size = 0.5,
                  show.legend = NA,
                  color = thm$cols$dark_grey3) +
    geom_errorbar(aes(x = pos, ymin = 100, ymax = 100),
                  size = 1,
                  show.legend = NA,
                  color = "black") +

    labs(x = NULL, y = NULL) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank())

  ## html plotly vs standard ggplot ----
  if(make_html){
    # plot_obj <- plot_obj +
    #   geom_text(aes(label = subbasin),
    #             family = "Helvetica",
    #             size = 3) # geom_text_repel not supported in ggplotly yet...
    plot_obj <- suppressWarnings(plotly::ggplotly(plot_obj, tooltip = "text"))

  } else {
    plot_obj <- plot_obj +
      ggrepel::geom_text_repel(
        aes(label = Name),
        family = "Helvetica",
        size = 3, angle = 0, direction = "x",
        segment.alpha = 0.1, segment.size = 0.1, box.padding = 0.8,
        show.legend = FALSE)
  }

  ## saving plots ----
  save_loc <- NULL
  if(is.character(save)){
    if(file.exists(save)){
      save_loc <- file.path(save, paste0("barplot_", goal_code, ".png"))
    }
  }
  if(isTRUE(save)){
    if("plotly" %in% class(plot_obj)){
      save_loc <- file.path(dir_assess, "reports", "widgets",
                            paste0("barplot_", goal_code, ".png"))
    } else {
      save_loc <- file.path(dir_assess, "reports", "figures",
                            paste0("barplot_", goal_code, ".png"))
    }
  }
  if(!is.null(save_loc)){
    if("plotly" %in% class(plot_obj)){
      htmlwidgets::saveWidget(as.widget(html_plot_obj), save_loc)
    } else {
      ggplot2::ggsave(filename = save_loc, plot = plot_obj, device = "png",
                      height = 5, width = 3, units = "in", dpi = 400)
    }
  }

  return(invisible(plot_obj))
}

