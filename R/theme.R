## Customized/Standardized Theme ----
#' customize and create standard theme for plots and maps
#'
#' a function to create a standardized theme for plots, updates ggtheme...
#'
#' @param plot_type if applying theme for a specific type of plot,
#' specify here (options: flowerplot, trends_barplot, ...)
#'
#' @return no return value, simply updates the ggplot theme where called

apply_bhi_theme <- function(plot_type = NA){


  ## color palettes ----
  palettes <- list(

    ## continuous color palettes
    reds = grDevices::colorRampPalette(c("#A50026","#D73027","#F46D43","#FDAE61", "#ffdcd8"))(50),
    purples = grDevices::colorRampPalette(c("#EEDFFF","#C093F7","#9E5AF0","#822BEA"))(50),
    blues = grDevices::colorRampPalette(c("#E0F3F8","#ABD9E9","#74ADD1","#4575B4","#313695"))(50),
    divergent_red_blue = c("#8c031a", "#cc0033", "#fff78a", "#f6ffb3", "#009999", "#0278a7"),

    ## discrete color palettes
    dims_pal = tibble::tibble(
      dimension =  c("present state", "likely future", "trend", "pressures", "resilience", "status"),
      color = c("#a0bbd0e8", "#ead19cf0", "#de8b5fe8", "#b13a23db", "#63945ade", "#9483afed")
    ),

    goals_pal = tibble::tibble(
      goal = c(
        "MAR","FIS","FP","CW","CON","EUT","TRA",
        "SP","LSP","ICO","LE","ECO","LIV",
        "AO","TR","CS","NP", "BD"
      ),
      color = c(
        "#549dad","#4ead9d","#53b3ac","#89b181","#60a777","#7ead6d","#9aad7e",
        "#97a4ba","#9fb3d4","#7c96b4","#9a97ba","#7d7ca3","#9990b4",
        "#e2de9a","#b6859a","#d0a6a1","#ccb4be","#88b1a6"
      )
    ),

    goalpal_shiny = tibble::tibble(
      goal = c(
        "MAR","FIS","FP","CW","CON","EUT","TRA",
        "SP","LSP","ICO","LE","ECO","LIV",
        "AO","TR","CS","NP", "BD"
      ),
      color = c(
        "aqua","aqua","aqua","olive","olive","olive","olive",
        "blue","blue","blue","purple","purple","purple",
        "yellow","red","orange","fuchsia","green"
      )
    )

  ) # end define color palettes


  ## plot elements ----
  elmts <- list(
    text_size = 9,
    title_rel_size = 1.25,
    grid_major = 0.25,
    axis_weight = 0.5,
    legend_pos = "right",
    legend_colour = NA,
    legend_fill = NA
  )

  icons <- list(
    FP = "utensils", MAR = "anchor", FIS = "fish", AO = "ship",
    CW = "burn", CON = "flask", EUT = "vial", TRA = "eraser",
    SP = "monument", LSP = "map-marked", ICO = "kiwi-bird", BD = "dna",
    LE = "landmark", ECO = "money-bill", LIV = "dharmachakra",
    TR = "suitcase", CS = "seedling", NP = "mortar-pestle"
  )

  ## plot colors ----
  cols <- list(
    light_grey1 = "grey95",
    light_grey2 = "grey90",
    light_greygreen = "#eef7f5",
    med_grey1 = "grey80",
    med_grey2 = "grey50",
    med_grey3 = "grey52",
    dark_grey1 = "grey30",
    dark_grey2 = "grey20",
    dark_grey3 = "grey22",
    accent_bright = "maroon",
    map_background1 = "#fcfcfd",
    map_background2 = "#f0e7d6",
    map_background3 = "aliceblue",
    map_polygon_border1 = "#acb9b6",
    map_polygon_border2 = "#b2996c",
    web_font_light1 = "#f0fcdf",
    web_font_light2 = "#f3ffe3",
    web_font_light3 = "#8db1a8",
    web_font <- "#516275",
    web_banner = "#1c3548",
    web_banner_light = "#006687",
    web_sidebar_dark = "#111b19")


  ## region names lookup table ----
  rgn_name_lookup <- rbind(
    tbl(bhi_db_con, "regions") %>%
      dplyr::select(region_id, subbasin, plot_title = region_name) %>%
      dplyr::collect(),
    tbl(bhi_db_con, "basins") %>%
      dplyr::select(region_id = subbasin_id, subbasin) %>%
      dplyr::collect() %>%
      mutate(plot_title = subbasin),
    data.frame(region_id = 0, subbasin = "Baltic", plot_title = "Baltic Sea")) %>%
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
    text = element_text(family = "Helvetica", color = cols$dark_grey3, size = elmts$text_size),
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
    if(plot_type == "timeseries"){
      theme_update()
    }
  }
  return(list(
    elmts = elmts,
    icons = icons,
    cols = cols,
    palettes = palettes,
    rgn_name_lookup = rgn_name_lookup
  ))
}
