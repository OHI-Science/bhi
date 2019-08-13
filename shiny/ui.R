source(file.path(here::here(), "shiny", "global.R"))

## Setting up Dashboard
dashboardPage(
  dashboardHeader(
    title = "Ocean Health Index for the Baltic Sea",
    titleWidth = 380
    ),

  ## Dashboard Sidebar ----
  dashboardSidebar(
    width = 290,

    setSliderColor("LightSteelBlue", 1),
    chooseSliderSkin("Flat"),

    sidebarMenu(
      ## WELCOME ----
      menuItem("WELCOME", tabName = "welcome"), # badgeLabel = "draft"

      ## EXPLORE GOALS ----
      convertMenuItem(
        menuItem("EXPLORE THE GOALS", tabName = "explore", startExpanded = TRUE,
                 ## AO Artisanal Fishing Opportunity
                 menuSubItem("Artisanal Fishing Opportunity", tabName = "ao", icon = icon(thm$icons$AO)),
                 ## BD Biodiversity
                 menuSubItem("Biodiversity", tabName = "bd", icon = icon(thm$icons$BD)),
                 ## CS Carbon Storage
                 menuSubItem("Carbon Storage", tabName = "cs", icon = icon(thm$icons$CS)),

                 ## CW Clean Water
                 convertMenuItem(menuItem(icon = icon(thm$icons$CW), "Clean Water",
                                          tabName = "cw", startExpanded = FALSE,

                                          menuSubItem("Contaminants", tabName = "con"),
                                          menuSubItem("Eutrophication", tabName = "eut"),
                                          menuSubItem("Trash", tabName = "tra")), "cw"),

                 ## FP Food Provision
                 convertMenuItem(menuItem(icon = icon(thm$icons$FP), "Food Provision",
                                          tabName = "fp", startExpanded = FALSE,

                                          menuSubItem("Wild-Caught Fisheries", tabName = "fis"),
                                          menuSubItem("Mariculture", tabName = "mar")), "fp"),

                 ## LE Livelihoods & Economies
                 convertMenuItem(menuItem(icon = icon(thm$icons$LE), "Livelihoods & Economies",
                                          tabName = "le", startExpanded = FALSE,

                                          menuSubItem("Economies", tabName = "eco"),
                                          menuSubItem("Livelihoods", tabName = "liv")), "le"),

                 ## SP Sense of Place
                 convertMenuItem(menuItem(icon = icon(thm$icons$SP), "Sense of Place", # icon = icon("kiwi-bird")
                                          tabName = "sp", startExpanded = FALSE,

                                          menuSubItem("Iconic Species", tabName = "ico"),
                                          menuSubItem("Lasting Special Places", tabName = "lsp")), "sp"),


                 ## NP Natural Products
                 menuSubItem("Natural Products", tabName = "np", icon = icon(thm$icons$NP)),
                 ## TR Tourism
                 menuSubItem("Tourism", tabName = "tr", icon = icon(thm$icons$TR))), "explore"),


      ## COMPARE/SUMMARIZE ----
      menuItem("COMPARE & SUMMARIZE", tabName = "summaries", startExpanded = FALSE,
               menuSubItem("Likely Future versus Present", tabName = "futures"),
               menuSubItem("Pressures", tabName = "pressures"),
               menuSubItem("Data Layers", tabName = "layers")),

      ## VIEW OPTIONS, SELECT INPUTS ----
      menuItem("VIEW OPTIONS", tabName = "summaries", startExpanded = FALSE,
               # sliderInput("view_year", "Year",
               #             min = min(full_scores_csv$year), max = max(full_scores_csv$year),
               #             value = assess_year, step = 1),
               sliderInput("view_year", "Year", 2012, 2019, 2014, step = 1, sep = ""),
               selectInput("spatial_unit", "Spatial Units",
                           choices = c(`Subbasins` = "subbasins", `BHI Regions` = "regions"),
                           selected = "subbasins"),
               selectInput("dimension", "Index Dimension",
                           choices = c(`Score` = "score",
                                       `Likely Future` = "future",
                                       `Pressures` = "pressures",
                                       `Resilience` = "resilience",
                                       `Current Status` = "status",
                                       `Short Term Trend` = "trend"),
                           selected = "score")
      ))), ## end sidebarMenu and dashboardSidebar


  ## Dashboard Body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$script(HTML("$('body').addClass('fixed');")), # lock side and top bars

    ## color overrides ----
    tags$style(type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "yellow",
        dplyr::filter(thm$palettes$goals_pal, goal == "AO")$color)
    ),
    tags$style(type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "fuchsia",
        dplyr::filter(thm$palettes$goals_pal, goal == "NP")$color)
    ),
    tags$style(type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "orange",
        dplyr::filter(thm$palettes$goals_pal, goal == "CS")$color)
    ),
    tags$style(type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "red",
        dplyr::filter(thm$palettes$goals_pal, goal == "TR")$color)
    ),
    tags$style(type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "purple",
        dplyr::filter(thm$palettes$goals_pal, goal == "LE")$color)
    ),
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "green",
        dplyr::filter(thm$palettes$goals_pal, goal == "BD")$color)
    ),
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "olive",
        dplyr::filter(thm$palettes$goals_pal, goal == "CW")$color)
    ),
    tags$style(type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "blue",
        dplyr::filter(thm$palettes$goals_pal, goal == "SP")$color)
    ),
    tags$style(type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "aqua",
        dplyr::filter(thm$palettes$goals_pal, goal == "FP")$color)
    ),

    ## Sidebar Pages/Tabs ----
    tabItems(

      ## WELCOME ----
      tabItem(tabName = "welcome",
              fluidRow(box(h1("Ocean Health Dashboard for the Baltic Sea"), width = 12)),
              fluidRow(box(h3("How healthy are our oceans?"),
                           "SHORT OHI INFO BLURB",
                           "MORE INFO, BORROW FROM GITHUB README OR WEBSITE?",
                           width = 12)
              ),

              fluidRow(
                ## flowerplot
                flowerplotCardUI(id = "baltic_flowerplot",
                                 title_text = "Baltic Sea Scores, 2014",
                                 sub_title_text = "Goal scores for the Baltic Sea. Area-weighted averages, with ranges shown."),

                ## map of overall scores
                # mapBarplotCardUI(id = "index_map_barplot",
                #                  title_text = "Overall Scores",
                #                  sub_title_text = "This map shows scores from the previous assessment (2014)")
                barplotCardUI(id = "index_barplot", title_text = "", box_width = 2),
                mapCardUI(id = "index_map",
                          title_text = "Map of Index Scores",
                          sub_title_text = "This map shows scores from the previous assessment (2014)",
                          box_width = 5)
              )
      ),

      ## AO ----
      ## Artisanal Opportunities
      tabItem(tabName = "ao",
              fluidRow(box(h1("Artisanal Fishing Opportunities"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "AO")$score,
                                  style = "font-size: 225%; font-family: Raleway; text-align:center; font-weight: lighter;"),
                           icon = icon(thm$icons$AO),
                           color = "yellow",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              ),

              fluidRow(
                barplotCardUI(id = "ao_barplot", title_text = "", box_width = 3),
                mapCardUI(id = "ao_map",
                          title_text = "Artisanal Opportunity Scores",
                          sub_title_text = "This map shows scores from the previous assessment (2014)",
                          box_width = 6),

                column(
                  width = 3,
                  text_links(
                    "AO DATA PREP",
                    "https://github.com/OHI-Science/bhi-1.0-archive/blob/draft/baltic2015/prep/AO/ao_prep.md",
                    box_width = 12
                  ),
                  text_links(
                    "MEANING OF GOAL",
                    "http://ohi-science.org/goals/#artisanal-fishing-opportunities",
                    box_width = 12
                  ),
                  text_links(
                    "ACCESS DATA",
                    "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers",
                    box_width = 12
                  ),
                  text_links(
                    "GET FILTERED VIEW",
                    "https://github.com/OHI-Science/bhi-1.0-archive/blob/draft/baltic2015/prep/AO/ao_prep.md",
                    box_width = 12
                  ),
                  text_links(
                    "SAVE CURRENT MAP",
                    "http://ohi-science.org/goals/#artisanal-fishing-opportunities",
                    box_width = 12
                  ),
                  text_links(
                    "DATA INPUTS",
                    "https://github.com/OHI-Science/bhi-prep/tree/master/data/AO/v2019",
                    box_width = 12
                  )
                )
              )
      ),

      ## BD ----
      ## Biodiversity
      tabItem(tabName = "bd",
              fluidRow(box(h1("Biodiversity"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "BD")$score,
                                  style = "font-size: 225%; font-family: Raleway; text-align:center; font-weight: lighter;"),
                           icon = icon(thm$icons$BD),
                           color = "green",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## CS ----
      ## Carbon Storage
      tabItem(tabName = "cs",
              fluidRow(box(h1("Carbon Storage"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "CS")$score,
                                  style = "font-size: 225%; font-family: Raleway; text-align:center; font-weight: lighter;"),
                           icon = icon(thm$icons$CS),
                           color = "orange",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## CW ----
      ## Clean Water
      tabItem(tabName = "cw",
              fluidRow(box(h1("Clean Water"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "CW")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$CW),
                           color = "olive",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## CON ----
      ## Contaminants
      tabItem(tabName = "con",
              fluidRow(box(h1("Contaminants"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "CON")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$CON),
                           color = "olive",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## EUT ----
      ## Eutrophication
      tabItem(tabName = "eut",
              fluidRow(box(h1("Eutrophication"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "EUT")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$EUT),
                           color = "olive",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## TRA ----
      ## Trash
      tabItem(tabName = "tra",
              fluidRow(box(h1("Trash"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "TRA")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$TRA),
                           color = "olive",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## FP ----
      ## Food Provision
      tabItem(tabName = "fp",
              fluidRow(box(h1("Food Provision"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "FP")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$FP),
                           color = "aqua",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## FIS ----
      ## Wild-Caught Fisheries
      tabItem(tabName = "fis",
              fluidRow(box(h1("Wild-Caught Fisheries"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "FIS")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$FIS),
                           color = "aqua",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## MAR ----
      ## Mariculture
      tabItem(tabName = "mar",
              fluidRow(box(h1("Mariculture"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "MAR")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$MAR),
                           color = "aqua",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## LE ----
      ## Livelihoods & Economies
      tabItem(tabName = "le",
              fluidRow(box(h1("Livelihoods & Economies"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "LE")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$LE),
                           color = "purple",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )

      ),

      ## ECO ----
      ## Economies
      tabItem(tabName = "eco",
              fluidRow(box(h1("Economies"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "ECO")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$ECO),
                           color = "purple",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## LIV ----
      ## Livelihoods
      tabItem(tabName = "liv",
              fluidRow(box(h1("Coastal Livelihoods"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "LIV")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$LIV),
                           color = "purple",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## SP ----
      ## Sense of Place
      tabItem(tabName = "sp",
              fluidRow(box(h1("Sense of Place"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "SP")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$SP),
                           color = "blue",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## ICO ----
      ## Iconic Species
      tabItem(tabName = "ico",
              fluidRow(box(h1("Iconic Species"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "ICO")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$ICO),
                           color = "blue",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## LSP ----
      ## Lasting Special Places
      tabItem(tabName = "lsp",
              fluidRow(box(h1("Lasting Special Places"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "LSP")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$LSP),
                           color = "blue",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              ),

              fluidRow(
                barplotCardUI(id = "lsp_barplot", title_text = "", box_width = 3),
                # mapCardUI(id = "lsp_map",
                #           title_text = "Lasting Special Places Scores",
                #           sub_title_text = "This map shows scores from the previous assessment (2014)",
                #           box_width = 6),

                column(
                  width = 3,
                  text_links(
                    "LSP DATA PREP",
                    "https://github.com/OHI-Science/bhi-1.0-archive/blob/draft/baltic2015/prep/LSP/lsp_prep.md",
                    box_width = 12
                  ),
                  text_links(
                    "ACCESS DATA",
                    "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers",
                    box_width = 12
                  )
                )
              )
      ),

      ## NP ----
      ## Natural Products
      tabItem(tabName = "np",
              fluidRow(box(h1("Natural Products"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "NP")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$NP),
                           color = "fuchsia",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## TR ----
      ## Tourism
      tabItem(tabName = "tr",
              fluidRow(box(h1("Tourism"), width = 9),

                       column(
                         width = 3,

                         infoBox(
                           title = "",
                           # subtitle = HTML("&nbsp;"),
                           tags$p(filter(full_scores_csv, region_id == 0, goal == "TR")$score,
                                  style = "font-size: 200%;"),
                           icon = icon(thm$icons$TR),
                           color = "red",
                           fill = TRUE,
                           width = 12)
                       ),

                       box("SOMETHING ABOUT GOAL PHILOSOPHY", width = 12)
              )
      ),

      ## COMPARE ----
      tabItem(tabName = "summaries",
              fluidRow(box(h1("Compare & Summarize"), "SOMETHING ABOUT DIFFERENT DIMENSIONS OF OHI", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      tabItem(tabName = "futures",
              fluidRow(box(h1("Likely Future versus Present"), width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      tabItem(tabName = "pressures",
              fluidRow(box(h1("Pressures"), "SOMETHING ABOUT BHI PRESSURES", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
              # withTags(div(class = "box-con",
              #              a(target = "_blank",
              #                href = "https://github.com/OHI-Science/bhi",
              #                div(class = "float box box-more",
              #                    p(class = "intro-text", "GitHub Repo"),
              #                    p("Access to code and documentation of analysis and results.")))))
      )
    )
  )
)
