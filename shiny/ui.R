source(file.path(here::here(), "shiny", "global.R"))

## Setting up Dashboard
dashboardPage(
  dashboardHeader(
    title = "Ocean Health Index for the Baltic Sea",
    titleWidth = 380
    ),

  ## Dashboard Sidebar ----
  dashboardSidebar(
    width = 300,

    sidebarMenu(
      ## WELCOME
      menuItem("WELCOME", tabName = "welcome"), # badgeLabel = "draft"

      ## EXPLORE GOALS
      convertMenuItem(
        menuItem("EXPLORE THE GOALS", tabName = "explore", startExpanded = FALSE,
                 ## AO Artisanal Fishing Opportunity
                 menuSubItem("Artisanal Fishing Opportunity", tabName = "ao", icon = icon("fish")), # icon("anchor")
                 ## BD Biodiversity
                 menuSubItem("Biodiversity", tabName = "bd", icon = icon("dna")),
                 ## CS Carbon Storage
                 menuSubItem("Carbon Storage", tabName = "cs", icon = icon("seedling")),

                 ## CW Clean Water
                 convertMenuItem(menuItem(icon = icon("burn"), "Clean Water",
                                          tabName = "cw", startExpanded = FALSE,

                                          menuSubItem("Contaminants", tabName = "con"),
                                          menuSubItem("Eutrophication", tabName = "eut"),
                                          menuSubItem("Trash", tabName = "tra")), "cw"),

                 ## FP Food Provision
                 convertMenuItem(menuItem(icon = icon("ship"), "Food Provision",
                                          tabName = "fp", startExpanded = FALSE,

                                          menuSubItem("Wild-Caught Fisheries", tabName = "fis"),
                                          menuSubItem("Mariculture", tabName = "mar")), "fp"),

                 ## LE Livelihoods & Economies
                 convertMenuItem(menuItem(icon = icon("landmark"), "Livelihoods & Economies",
                                          tabName = "le", startExpanded = FALSE,

                                          menuSubItem("Economies", tabName = "eco"),
                                          menuSubItem("Livelihoods", tabName = "liv")), "le"),

                 ## SP Sense of Place
                 convertMenuItem(menuItem(icon = icon("monument"), "Sense of Place", # icon = icon("kiwi-bird")
                                          tabName = "sp", startExpanded = FALSE,

                                          menuSubItem("Iconic Species", tabName = "ico"),
                                          menuSubItem("Lasting Special Places", tabName = "lsp")), "sp"),


                 ## NP Natural Products
                 menuSubItem("Natural Products", tabName = "np", icon = icon("mortar-pestle")),
                 ## TR Tourism
                 menuSubItem("Tourism", tabName = "tr", icon = icon("suitcase"))), "explore"),


      ## COMPARE/SUMMARIZE
      menuItem("COMPARE & SUMMARIZE", tabName = "summaries", startExpanded = FALSE,
               menuSubItem("Likely Future versus Present", tabName = "futures"),
               menuSubItem("Pressures", tabName = "pressures")),

      ## VIEW OPTIONS, SELECT INPUTS
      menuItem("VIEW OPTIONS", tabName = "summaries", startExpanded = FALSE,
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
                           selected = "score")))),


  ## Dashboard Body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$script(HTML("$('body').addClass('fixed');")), # lock side and top bars


    ## Sidebar Pages/Tabs
    tabItems(

      ## WELCOME ----
      tabItem(tabName = "welcome",
              fluidRow(box(h1("Ocean Health Dashboard for the Baltic Sea"), "SHORT OHI INFO BLURB", width = 12)),
              fluidRow(box(h3("How healthy are our oceans?"), "MORE INFO, BORROW FROM GITHUB README OR WEBSITE?", width = 12)),

              fluidRow(
                ## flowerplot
                flowerplotCardUI(id = "baltic_flowerplot",
                                 title_text = "Baltic Sea Scores, 2014",
                                 sub_title_text = "Flowerplot with ranges, for entire Baltic Sea by BHI regions. Goal-averages."),

                ## map of overall scores
                # mapBarplotUI(id = "overall_baltic_map",
                #              title_text = "Overall Scores",
                #              sub_title_text = "This map shows scores from the previous assessment (2014)")
                mapCardUI(id = "index_map",
                          title_text = "Overall Scores",
                          sub_title_text = "This map shows scores from the previous assessment (2014)")
              )
      ),

      ## AO ----
      ## Artisanal Opportunities
      tabItem(tabName = "ao",
              fluidRow(box(h1("Artisanal Opportunities"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## BD ----
      ## Biodiversity
      tabItem(tabName = "bd",
              fluidRow(box(h1("Biodiversity"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## CS ----
      ## Carbon Storage
      tabItem(tabName = "cs",
              fluidRow(box(h1("Carbon Storage"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## CW ----
      ## Clean Water
      tabItem(tabName = "cw",
              fluidRow(box(h1("Clean Water"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## CON ----
      ## Contaminants
      tabItem(tabName = "con",
              fluidRow(box(h1("Contaminants"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## EUT ----
      ## Eutrophication
      tabItem(tabName = "eut",
              fluidRow(box(h1("Eutrophication"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## TRA ----
      ## Trash
      tabItem(tabName = "tra",
              fluidRow(box(h1("Trash"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## FP ----
      ## Food Provision
      tabItem(tabName = "fp",
              fluidRow(box(h1("Food Provision"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## FIS ----
      ## Wild-Caught Fisheries
      tabItem(tabName = "fis",
              fluidRow(box(h1("Wild-Caught Fisheries"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## MAR ----
      ## Mariculture
      tabItem(tabName = "mar",
              fluidRow(box(h1("Mariculture"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## LE ----
      ## Livelihoods & Economies
      tabItem(tabName = "le",
              fluidRow(box(h1("Livelihoods & Economies"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## ECO ----
      ## Economies
      tabItem(tabName = "eco",
              fluidRow(box(h1("Economies"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## LIV ----
      ## Livelihoods
      tabItem(tabName = "liv",
              fluidRow(box(h1("Livelihoods"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## SP ----
      ## Sense of Place
      tabItem(tabName = "sp",
              fluidRow(box(h1("Sense of Place"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## ICO ----
      ## Iconic Species
      tabItem(tabName = "ico",
              fluidRow(box(h1("Iconic Species"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## LSP ----
      ## Lasting Special Places
      tabItem(tabName = "lsp",
              fluidRow(box(h1("Lasting Special Places"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## NP ----
      ## Natural Products
      tabItem(tabName = "np",
              fluidRow(box(h1("Natural Products"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
              )
      ),

      ## TR ----
      ## Tourism
      tabItem(tabName = "tr",
              fluidRow(box(h1("Tourism"),
                           "SOMETHING ABOUT GOAL PHILOSOPHY",
                           width = 12)
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
