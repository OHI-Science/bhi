source(file.path(here::here(), "shiny", "global.R"))

## Setting up Dashboard
dashboardPage(
  dashboardHeader(
    title = "Ocean Health Index for the Baltic Sea",
    titleWidth = 380),

  ## Dashboard Sidebar ----
  dashboardSidebar(
    width = 280,

    sidebarMenu(
      menuItem("WELCOME", tabName = "welcome"), # badgeLabel = "draft"

      convertMenuItem(
        menuItem("EXPLORE THE GOALS", tabName = "explore", startExpanded = TRUE,
                 ## AO Artisanal Fishing Opportunity
                 menuSubItem("Artisanal Fishing Opportunity", tabName = "ao"),
                 ## BD Biodiversity
                 menuSubItem("Biodiversity", tabName = "bd"),
                 ## CS Carbon Storage
                 menuSubItem("Carbon Storage", tabName = "cs"),
                 ## CW Clean Water
                 convertMenuItem(menuItem("Clean Water", tabName = "cw", startExpanded = FALSE,
                                          menuSubItem("Contaminants", tabName = "con"),
                                          menuSubItem("Eutrophication", tabName = "eut"),
                                          menuSubItem("Trash", tabName = "tra")), "cw"),
                 ## FP Food Provision
                 convertMenuItem(menuItem("Food Provision", tabName = "fp", startExpanded = FALSE,
                                          menuSubItem("Wild-Caught Fisheries", tabName = "fis"),
                                          menuSubItem("Mariculture", tabName = "mar")), "fp"),
                 ## LE Livelihoods & Economies
                 convertMenuItem(menuItem("Livelihoods & Economies", tabName = "le", startExpanded = FALSE,
                                          menuSubItem("Economies", tabName = "eco"),
                                          menuSubItem("Livelihoods", tabName = "liv")), "le"),
                 ## SP Sense of Place
                 convertMenuItem(menuItem("Sense of Place", tabName = "sp", startExpanded = FALSE,
                                          menuSubItem("Iconic Species", tabName = "ico"),
                                          menuSubItem("Lasting Special Places", tabName = "lsp")), "sp"),
                 ## convertMenuItem by Jamie Afflerbach https://github.com/OHI-Northeast/ne-dashboard/tree/master/functions

                 ## NP Natural Products
                 menuSubItem("Natural Products", tabName = "np"),
                 ## TR Tourism
                 menuSubItem("Tourism", tabName = "tr")), "explore"),

      menuItem("COMPARE & SUMMARIZE", tabName = "summaries", startExpanded = FALSE,
               menuSubItem("Likely Future versus Present", tabName = "futures"),
               menuSubItem("Pressures", tabName = "pressures")))), ## end dashboard sidebar


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
              fluidRow(box(h3("How healthy are our oceans?"), "MORE INFO, BORROW FROM GITHUB README OR WEBSITE?", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## AO ----
      ## Artisanal Opportunities
      tabItem(tabName = "ao",
              fluidRow(box(h1("Artisanal Opportunities"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## BD ----
      ## Biodiversity
      tabItem(tabName = "bd",
              fluidRow(box(h1("Biodiversity"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## CS ----
      ## Carbon Storage
      tabItem(tabName = "cs",
              fluidRow(box(h1("Carbon Storage"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## CW ----
      ## Clean Water
      tabItem(tabName = "cw",
              fluidRow(box(h1("Clean Water"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## CON ----
      ## Contaminants
      tabItem(tabName = "con",
              fluidRow(box(h1("Contaminants"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## EUT ----
      ## Eutrophication
      tabItem(tabName = "eut",
              fluidRow(box(h1("Eutrophication"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## TRA ----
      ## Trash
      tabItem(tabName = "tra",
              fluidRow(box(h1("Trash"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## FP ----
      ## Food Provision
      tabItem(tabName = "fp",
              fluidRow(box(h1("Food Provision"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## FIS ----
      ## Wild-Caught Fisheries
      tabItem(tabName = "fis",
              fluidRow(box(h1("Wild-Caught Fisheries"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## MAR ----
      ## Mariculture
      tabItem(tabName = "mar",
              fluidRow(box(h1("Mariculture"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## LE ----
      ## Livelihoods & Economies
      tabItem(tabName = "le",
              fluidRow(box(h1("Livelihoods & Economies"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## ECO ----
      ## Economies
      tabItem(tabName = "eco",
              fluidRow(box(h1("Economies"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## LIV ----
      ## Livelihoods
      tabItem(tabName = "liv",
              fluidRow(box(h1("Livelihoods"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## SP ----
      ## Sense of Place
      tabItem(tabName = "sp",
              fluidRow(box(h1("Sense of Place"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## ICO ----
      ## Iconic Species
      tabItem(tabName = "ico",
              fluidRow(box(h1("Iconic Species"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## LSP ----
      ## Lasting Special Places
      tabItem(tabName = "lsp",
              fluidRow(box(h1("Lasting Special Places"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## NP ----
      ## Natural Products
      tabItem(tabName = "np",
              fluidRow(box(h1("Natural Products"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
      ),

      ## TR ----
      ## Tourism
      tabItem(tabName = "tr",
              fluidRow(box(h1("Tourism"), "SOMETHING ABOUT GOAL PHILOSOPHY", width = 12))
              # fluidRow("card_ui and/or map_ui and/or something else will go below here...")
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
      )
    )
  )
)
