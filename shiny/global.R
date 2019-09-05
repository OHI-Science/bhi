## Libraries ----
source(file.path(here::here(), "R", "visualization.R"))
source(file.path(here::here(), "R", "flowerplot.R"))
source(file.path(here::here(), "R", "maps.R"))
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(pool)
library(highcharter)

setwd(file.path(here::here(), "shiny"))
gh_prep <- "https://github.com/OHI-Science/bhi-1.0-archive/blob/draft/baltic2015/prep"
gh_layers <- "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
thm <- apply_bhi_theme()


## Functions ----

source(file.path(here::here(), "shiny", "modules", "map_card.R"))
source(file.path(here::here(), "shiny", "modules", "map_rgn_card.R"))
source(file.path(here::here(), "shiny", "modules", "barplot_card.R"))
source(file.path(here::here(), "shiny", "modules", "flowerplot_card.R"))
source(file.path(here::here(), "shiny", "modules", "scorebox_card.R"))

#' expand contract menu sidebar subitems
#'
#' ui function to expand and contract subitems in menu sidebar
#' from convertMenuItem by Jamie Afflerbach https://github.com/OHI-Northeast/ne-dashboard/tree/master/functions
#'
#' @param mi menu item as created by menuItem function, including subitems from nested menuSubItem function
#' @param tabName name of the tab that correspond to the mi menu item
#'
#' @return expanded or contracted menu item

convertMenuItem <- function(mi, tabName){
  mi$children[[1]]$attribs['data-toggle'] = "tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class) > 0 && mi$attribs$class == "treeview"){
    mi$attribs$class = NULL
  }
  mi
}


#' create text boxes with links
#'
#' @param title the text to be displayed in the box
#' @param url url the box should link to
#' @param box_width width of box, see shinydashboard::box 'width' arguement specifications
#'
#' @return

text_links <- function(title = NULL, url = NULL, box_width = 12){

  box(class = "text_link_button",
      h4(a(paste("\n", title), href = url, target = "_blank")),
      width = box_width,
      height = 65,
      background = "light-blue",
      status = "primary",
      solidHeader = TRUE)
}


#' print in console pieces to create region menu code
#'
#' @param rgn_tab_con
#'
#' @return no returned object; prints helpful info in console

make_rgn_menu <- function(rgn_tab_con = bhi_db_con){

  rgn <- tbl(rgn_tab_con, "regions") %>%
    select(region_id, subbasin, region_name) %>%
    collect() %>%
    arrange(subbasin) %>%
    mutate(print_col = sprintf("`%s` = %s", region_name, region_id))

  cat(paste0("`", unique(rgn$subbasin), "` = c(`", unique(rgn$subbasin), "` = )", sep =  "\n"))

  cat("\n\n")
  for(s in unique(rgn$subbasin)){
    cat(filter(rgn, subbasin  == s)$print_col, sep = ", \n")
  }
}

#' layers scatterplot variables selection menu
#'
#' @param layers_dir directory containing the layers
#' @param print boolean indicating whether to print copy-and-pasteable text in console
#'
#' @return no returned object; prints helpful info in console

make_lyrs_menu <- function(layers_dir, print = FALSE){

  lyrs <- list.files(layers_dir) %>%
    grep(pattern = "_bhi2015.csv", value = TRUE) %>%
    grep(pattern = "_trend", value = TRUE, invert = TRUE) %>%
    grep(pattern = "_scores", value = TRUE, invert = TRUE) %>%
    sort()

  cat("\n\n")
  for(f in lyrs){
    cat(
      sprintf(
        "`%s` = \"%s\"",
        f %>%
          str_remove(pattern  = "_bhi2015.csv+") %>%
          str_to_upper(),
        f
      ),
      sep = ", \n"
    )
  }

  vec <- lyrs
  names(vec) <- lyrs %>%
    str_remove(pattern  = "_bhi2015.csv+") %>%
    str_to_upper()
  return(vec)
}

#' print in console goal-pages ui and server code
#'
#' helper/timesaver function, could maybe be a module but thats another layer of complexity...
#'
#' @param goal_code the goal for which to input information /create code chunk
#' @param goal_code_templatize the goal to use as a template
#' @param ui_server one of either ui or server, whichever code is to be generated for
#'
#' @return

templatize_goalpage <- function(goal_code, goal_code_templatize, ui_server){

  if(ui_server  ==  "ui"){
    ## ui template, read then parse to goal to copy/templatize
    templatetxt <- scan(file.path(getwd(), "ui.R"),
                        what = "character",
                        sep = "\n")
    breaks <- templatetxt %>% grep(pattern = "##\\s>>\\s[a-z]{2,3}\\s----")
    breakstart <- templatetxt %>% grep(pattern = sprintf("## >> %s ----", str_to_lower(goal_code_templatize)))
    breakend <- min(breaks[which(breaks > breakstart)]) - 1
    templatetxt <- templatetxt[breakstart:breakend]

  } else if(ui_server  ==  "server"){
    ## server template, read then parse to goal to copy/templatize
    templatetxt <- scan(file.path(getwd(), "server.R"),
                        what = "character",
                        sep = "\n")
    breaks <- templatetxt %>% grep(pattern = "##\\s[A-Z]{2,3}\\s----")
    breakstart <- templatetxt %>%
      grep(
        pattern = sprintf(
          "##\\s%s\\s----",
          str_to_upper(goal_code_templatize)
        )
      )
    breakend <- min(breaks[which(breaks > breakstart)]) - 1
    templatetxt <- templatetxt[breakstart:breakend]

  } else {
    message("ui_server argument must be one of 'ui' or 'server'")
  }

  ## replacement info
  goalinfo <- tbl(bhi_db_con, "plot_conf") %>%
    select(name, goal, parent) %>%
    collect() %>%
    filter(goal %in% c(str_to_upper(goal_code), str_to_upper(goal_code_templatize)))

  ## inject info for new goal
  txt <- templatetxt
  for(p in c("\"%s\"", " %s ", "\"%s_", " %s_")){
    pttn <- sprintf(p, str_to_lower(goal_code_templatize))
    repl <- sprintf(p, str_to_lower(goal_code)) # print(paste(pttn, "-->", repl))
    txt <- str_replace_all(txt, pattern = pttn, replacement = repl)
  }
  for(p in c("\"%s\"", " %s ", "\"%s ", " %s\"", "/%s/", "%s\\)")){
    pttn <- sprintf(p, str_to_upper(goal_code_templatize))
    repl <- sprintf(p, str_to_upper(goal_code)) # print(paste(pttn, "-->", repl))
    txt <- str_replace_all(txt, pattern = pttn, replacement = repl)
  }
  # if(any(!is.na(goalinfo$parent))){"?"}
  for(p in c("\"%s\"", " %s ", "%s ", " %s", "\"%s ", " %s\"", "/%s/", "%s\\)")){
    pttn <- sprintf(p, filter(goalinfo, goal == goal_code_templatize)$name)
    repl <- sprintf(p, filter(goalinfo, goal == goal_code)$name) # print(paste(pttn, "-->", repl))
    txt <- str_replace_all(txt , pattern = pttn, replacement = repl)
  }
  #biodiversity --> #artisanal-fishing-opportunities

  txt <- txt %>%
    str_replace_all(
      pattern = sprintf(
        "#%s",
        filter(goalinfo, goal == goal_code_templatize)$name %>%
          str_to_lower() %>%
          str_replace_all(pattern = " ", replacement = "-")
      ),
      replacement = sprintf(
        "#%s",
        filter(goalinfo, goal == goal_code)$name %>%
          str_to_lower() %>%
          str_replace_all(pattern = " ", replacement = "-")
      )) %>%
    str_replace_all(pattern = "p\\(\"[A-Za-z0-9 ]+\"\\)", replacement = "p(\"\")")

  ## print result in console
  cat(txt, sep = "\n")
}


## Shiny Global Data ----

## full scores and goal definitions
full_scores_csv <- tbl(bhi_db_con, "scores2015") %>% collect()
goals_csv <- tbl(bhi_db_con, "goals") %>% collect()

## shapefiles: BHI regions, subbasins, MPAs
rgns_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile",
                        "BHI_shapefile") %>%
  dplyr::mutate(Subbasin = as.character(Subbasin)) %>%
  dplyr::mutate(Subbasin = ifelse(Subbasin == "Bothian Sea",
                                  "Bothnian Sea", Subbasin)) # NEED TO FIX THIS TYPO!!!!!!!!
subbasins_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/HELCOM_subbasins_holasbasins",
                             "HELCOM_subbasins_holasbasins")
# mpa_shp <- sf::st_read()

## shps combined with score information, only score dim and current assessment year
# message("calculating 'mapping_scores_subbasin' sf object...")
# mapping_scores_subbasin <- make_subbasin_sf(
#   subbasins_shp, full_scores_csv,
#   goal_code = "all", dim = "score", year = assess_year,
#   simplify_level = 1)

# message("calculating 'mapping_scores_rgn' sf object...")
# mapping_scores_rgn <- make_rgn_sf(
#   rgns_shp, full_scores_csv,
#   goal_code = "all", dim = "score", year = assess_year,
#   simplify_level = 1)

