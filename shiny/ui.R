source(file.path(here::here(), "shiny", "global.R"))

## Setting up Dashboard
dashboardPage(
  dashboardHeader(
    title = "Ocean Health Index for the Baltic Sea",
    titleWidth = 380
  ),

  ## DASHBOARD SIDEBAR ----
  dashboardSidebar(
    width = 290,

    setSliderColor("LightSteelBlue", 1),
    chooseSliderSkin("Flat"),

    sidebarMenu(
      ## >> welcome ----
      menuItem("WELCOME", tabName = "welcome"),

      ## >> explore goals ----
      convertMenuItem(
        menuItem(
          "EXPLORE THE GOALS",
          tabName = "explore",
          startExpanded = TRUE,

          ## AO Artisanal Fishing Opportunity
          menuSubItem("Artisanal Fishing Opportunity", tabName = "ao", icon = icon(thm$icons$AO)),

          ## BD Biodiversity
          menuSubItem("Biodiversity", tabName = "bd", icon = icon(thm$icons$BD)),

          ## CS Carbon Storage
          menuSubItem("Carbon Storage", tabName = "cs", icon = icon(thm$icons$CS)),

          ## CW Clean Water
          convertMenuItem(
            menuItem(
              icon = icon(thm$icons$CW),
              "Clean Water",
              tabName = "cw",
              startExpanded = FALSE,

              menuSubItem("Contaminants", tabName = "con"),
              menuSubItem("Eutrophication", tabName = "eut"),
              menuSubItem("Trash", tabName = "tra")
            ), # end clean water menu item
          tabName = "cw"), # end clean water collapse menu item

          ## FP Food Provision
          convertMenuItem(
            menuItem(
              icon = icon(thm$icons$FP),
              "Food Provision",
              tabName = "fp",
              startExpanded = FALSE,

              menuSubItem("Wild-Caught Fisheries", tabName = "fis"),
              menuSubItem("Mariculture", tabName = "mar")
            ), # end food provision menu item
          tabName = "fp"), # end food provision collapse menu item

          ## LE Livelihoods & Economies
          convertMenuItem(
            menuItem(
              icon = icon(thm$icons$LE),
              "Livelihoods & Economies",
              tabName = "le",
              startExpanded = FALSE,

              menuSubItem("Economies", tabName = "eco"),
              menuSubItem("Livelihoods", tabName = "liv")
            ), # end liv and econ menu item
          tabName = "le"), # end liv and econ menu item collapse menu item'

          ## SP Sense of Place
          convertMenuItem(
            menuItem(
              icon = icon(thm$icons$SP),
              "Sense of Place",
              tabName = "sp",
              startExpanded = FALSE,

              menuSubItem("Iconic Species", tabName = "ico"),
              menuSubItem("Lasting Special Places", tabName = "lsp")
            ), # end sense of place menu item
          tabName = "sp"), # end sense of place menu item collapse menu item

          ## NP Natural Products
          menuSubItem("Natural Products", tabName = "np", icon = icon(thm$icons$NP)),

          ## TR Tourism
          menuSubItem("Tourism", tabName = "tr", icon = icon(thm$icons$TR))


        ), # end explore goals menu item
      tabName = "explore"), # end explore goals collapse menu item

      ## >> compare and summarize ----
      menuItem(
        "COMPARE & SUMMARIZE",
        tabName = "summaries",
        startExpanded = FALSE,

        menuSubItem(
          "Likely Future versus Present",
          tabName = "futures"
        ),
        menuSubItem(
          "Pressures",
          tabName = "pressures"
        ),
        menuSubItem(
          "Scenario Exploration",
          tabName = "scenarios"
        ),
        menuSubItem(
          "Data Layers",
          tabName = "layers"
        )
      ), # end compare and summarize sidebar

      ## >> view options ----
      menuItem(
        "VIEW OPTIONS",
        tabName = "summaries",
        startExpanded = FALSE,

        ## input year ----
        # sliderInput("view_year", "Year",
        #             min = min(full_scores_csv$year), max = max(full_scores_csv$year),
        #             value = assess_year, step = 1),
        sliderInput("view_year", "Year", 2012, 2019, 2014, step = 1, sep = ""),

        ## input region ----
        selectInput(
          "flower_rgn",
          "Flowerplot Region",
          list(
            `Baltic Sea` = 0,

            `Aland Sea` = c(
              `Aland Sea` = 514,
              `Aland Sea, Sweden` = 35,
              `Aland Sea, Finland` = 36
            ),

            `Arkona Basin` = c(
              `Arkona Basin` = 506,
              `Arkona Basin, Sweden` = 11,
              `Arkona Basin, Denmark` = 12,
              `Arkona Basin, Germany` = 13
            ),

            `Bay of Mecklenburg` = c(
              `Bay of Mecklenburg` = 505,
              `Bay of Mecklenburg, Denmark` = 9,
              `Bay of Mecklenburg, Germany` = 10
            ),

            `Bornholm Basin` = c(
              `Bornholm Basin` = 507,
              `Bornholm Basin, Sweden` = 14,
              `Bornholm Basin, Denmark` = 15,
              `Bornholm Basin, Germany` = 16,
              `Bornholm Basin, Poland` = 17
            ),

            `Bothnian Bay` = c(
              `Bothnian Bay` = 517,
              `Bothnian Bay, Sweden` = 41,
              `Bothnian Bay, Finland` = 42
            ),

            `Bothnian Sea` = c(
              `Bothnian Sea` = 515,
              `Bothnian Sea, Sweden` = 37,
              `Bothnian Sea, Finland` = 38
            ),

            `Eastern Gotland Basin` = c(
              `Eastern Gotland Basin` = 509,
              `Eastern Gotland Basin, Sweden` = 20,
              `Eastern Gotland Basin, Poland` = 21,
              `Eastern Gotland Basin, Russia` = 22,
              `Eastern Gotland Basin, Lithuania` = 23,
              `Eastern Gotland Basin, Latvia` = 24,
              `Eastern Gotland Basin, Estonia` = 25
            ),

            `Gdansk Basin` = c(
              `Gdansk Basin` = 508,
              `Gdansk Basin, Poland` = 18,
              `Gdansk Basin, Russia` = 19
            ),

            `Great Belt` = c(
              `Great Belt` = 502,
              `Great Belt, Denmark` = 3,
              `Great Belt, Germany` = 4
            ),

            `Gulf of Finland` = c(
              `Gulf of Finland` = 513,
              `Gulf of Finland, Finland` = 32,
              `Gulf of Finland, Russia` = 33,
              `Gulf of Finland, Estonia` = 34
            ),

            `Gulf of Riga` = c(
              `Gulf of Riga` = 511,
              `Gulf of Riga, Latvia` = 27,
              `Gulf of Riga, Estonia` = 28
            ),

            `Kattegat` = c(
              `Kattegat` = 501,
              `Kattegat, Sweden` = 1,
              `Kattegat, Denmark` = 2
            ),

            `Kiel Bay` = c(
              `Kiel Bay` = 504,
              `Kiel Bay, Denmark` = 7,
              `Kiel Bay, Germany` = 8
            ),

            `Northern Baltic Proper` = c(
              `Northern Baltic Proper` = 500,
              `Northern Baltic Proper, Sweden` = 29,
              `Northern Baltic Proper, Finland` = 30,
              `Northern Baltic Proper, Estonia` = 31
            ),

            `The Quark` = c(
              `The Quark` = 516,
              `The Quark, Sweden` = 39,
              `The Quark, Finland` = 40
            ),

            `The Sound` = c(
              `The Sound` = 503,
              `The Sound, Sweden` = 5,
              `The Sound, Denmark` = 6
            ),

            `Western Gotland Basin` = c(
              `Western Gotland Basin` = 510,
              `Western Gotland Basin, Sweden` = 26
            )
          )
        ),

        ## input spatial unit ----
        selectInput(
          "spatial_unit",
          "Spatial Units",
          choices = c(`Subbasins` = "subbasins",
                      `BHI Regions` = "regions"),
          selected = "subbasins"
        ),

        ## input dimension ----
        selectInput(
          "dimension",
          "Index Dimension",
          choices = c(`Score` = "score",
                      `Likely Future` = "future",
                      `Pressures` = "pressures",
                      `Resilience` = "resilience",
                      `Current Status` = "status",
                      `Short Term Trend` = "trend"),
          selected = "score"
        )
      ) # end view options sidebar


    ) # end sidebarMenu
  ), # end dashboardSidebar

  ## DASHBOARD BODY ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tags$script(HTML("$('body').addClass('fixed');")), # lock side and top bars

    ## color overrides ----
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "yellow",
        dplyr::filter(thm$palettes$goals_pal, goal == "AO")$color)
    ),
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "fuchsia",
        dplyr::filter(thm$palettes$goals_pal, goal == "NP")$color)
    ),
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "orange",
        dplyr::filter(thm$palettes$goals_pal, goal == "CS")$color)
    ),
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "red",
        dplyr::filter(thm$palettes$goals_pal, goal == "TR")$color)
    ),
    tags$style(
      type = "text/css",
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
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "blue",
        dplyr::filter(thm$palettes$goals_pal, goal == "SP")$color)
    ),
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "aqua",
        dplyr::filter(thm$palettes$goals_pal, goal == "FP")$color)
    ),
    tags$style(
      type = "text/css",
      sprintf(
        ".bg-%s {background-color: %s!important; }",
        "light-blue",
        "#ecf0f6"
      )
    ),

    ## PAGES ----
    tabItems(
      ## >> welcome ----
      tabItem(
        tabName = "welcome",

        ## header and intro
        fluidRow(
          box(
            h1("Ocean Health Dashboard for the Baltic Sea"),
            width = 12
          )
        ),
        fluidRow(
          box(
            h3("How healthy are our oceans?"),
            p("The Baltic Health Index is a regional study under the global Ocean Health Index framework.
              The aim is to continue the development of a tool that can be used by decision-makers to guide management of the Baltic Sea region towards increased sustainability.
              Oceans in general provide a diverse array of benefits to humans.
              Managing for such a broad range of benefits requires a method of measurement that is both comprehensive and quantitative;
              establishing such a method was the motivation behind the Ocean Health Index.
              We strive to use the best open source tools available, to make our ocean health metrics, results and underlying data easily accessible and transparent."),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(

          ## flowerplot
          flowerplotCardUI( # flowerplotRgnCardUI(
            id = "baltic_flowerplot",
            title_text = "Flowerplot of Scores",
            sub_title_text = "Select region under View Options to visualize region-specific scores."
          ),

          ## map of overall scores, with barplot
          barplotCardUI(
            id = "index_barplot",
            title_text = "Proximity to Target",
            sub_title_text = "",
            box_width = 2
          ),
          mapRgnCardUI(
            id = "index_map",
            title_text = "Map of Index Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 5
          )
        )
      ), # end welcome tab

      ## >> ao ----
      ## Artisanal Fishing Opportunity
      tabItem(
        tabName = "ao",
        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Artisanal Fishing Opportunity"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "ao_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY AND STATUS/TRENDS"),
            width = 12
          )
        ),
        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "ao_barplot",
            title_text = "Artisanal Fishing Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "ao_map",
            title_text = "Map of Artisanal Fishing Opportunity Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),
          column(
            width = 3,
            text_links(
              "AO DATA PREP",
              sprintf("%s/AO/ao_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "http://ohi-science.org/goals/#artisanal-fishing-opportunities"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/AO/ao_data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end AO tab item

      ## >> bd ----
      ## Biodiversity
      tabItem(
        tabName = "bd",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Biodiversity"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "bd_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "bd_barplot",
            title_text = "Biodiversity Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "bd_map",
            title_text = "Map of Biodiversity Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "BD DATA PREP",
              sprintf("%s/SPP/spp_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "http://ohi-science.org/goals/#biodiversity"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/SPP/spatial_data_prep"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end BD tab item

      ## >> cs ----
      ## Carbon Storage
      tabItem(
        tabName = "cs",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Carbon Storage"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "cs_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "cs_barplot",
            title_text = "",
            box_width = 3
          ),
          mapCardUI(
            id = "cs_map",
            title_text = "Carbon Storage Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6
          ),

          column(
            width = 3,
            text_links(
              "CS DATA PREP",
              sprintf("%s/CS/cs_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "http://ohi-science.org/goals/#carbon-storage"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/CS/zostera_raster"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end CS tab item

      ## >> cw ----
      ## Clean Water
      tabItem(
        tabName = "cw",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Clean Water"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "cw_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "cw_barplot",
            title_text = "Clean Waters Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "cw_map",
            title_text = "Map of Clean Water Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "CW DATA PREP",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/CW"
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#clean-waters"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end CW tab item

      ## >> con ----
      ## Contaminants
      tabItem(
        tabName = "con",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Contaminants"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "con_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "con_barplot",
            title_text = "Contaminants Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "con_map",
            title_text = "Map of Contaminants Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "CON DATA PREP",
              sprintf("%s/CW/contaminants/contaminants_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#clean-waters"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/CW/contaminants/contaminants_data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end CON tab item

      ## >> eut ----
      ## Eutrophication
      tabItem(
        tabName = "eut",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Eutrophication"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "eut_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "eut_barplot",
            title_text = "Eutrophication Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "eut_map",
            title_text = "Map of Eutrophication Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "EUT DATA PREP",
              sprintf("%s/CW/eutrophication/eutrophication_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#clean-waters"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/CW/eutrophication"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end EUT tab item

      ## >> tra ----
      ## Trash
      tabItem(
        tabName = "tra",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Trash"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "tra_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "tra_barplot",
            title_text = "Trash Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "tra_map",
            title_text = "Map of  Trash (Clean Water) Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "TRA DATA PREP",
              sprintf("%s/CW/trash/tra_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#clean-waters"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/CW/trash/raw"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end TRA tab item

      ## >> fp ----
      ## Food Provision
      tabItem(
        tabName = "fp",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Food Provision"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "fp_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "fp_barplot",
            title_text = "Food Provision Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "fp_map",
            title_text = "Map of Food Provision Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "FP DATA PREP",
              sprintf("%s/FP/fp_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#food-provision"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end FP tab item

      ## >> fis ----
      ## Wild-Caught Fisheries
      tabItem(
        tabName = "fis",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Wild-Caught Fisheries"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "fis_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "fis_barplot",
            title_text = "Fisheries Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "fis_map",
            title_text = "Map of Wild-Caught Fisheries Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "FIS DATA PREP",
              sprintf("%s/FIS/fis_np_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#food-provision"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/FIS/raw"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end FIS tab item

      ## >> mar ----
      ## Mariculture
      tabItem(
        tabName = "mar",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Mariculture"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "mar_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "mar_barplot",
            title_text = "Mariculture Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "mar_map",
            title_text = "Map of Mariculture Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht =  555
          ),

          column(
            width = 3,
            text_links(
              "MAR DATA PREP",
              sprintf("%s/MAR/mar_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#food-provision"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/MAR/mar_data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end MAR tab item

      ## >> le ----
      ## Livelihoods & Economies
      tabItem(
        tabName = "le",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Livelihoods & Economies"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "le_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "le_barplot",
            title_text = "Livelihoods & Economies Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "le_map",
            title_text = "Map of Coastal Livelihoods & Economies Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#livelihoods-and-economies"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end LE tab item

      ## >> eco ----
      ## Economies
      tabItem(
        tabName = "eco",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Economies"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "eco_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "eco_barplot",
            title_text = "Economies Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "eco_map",
            title_text = "Map of Coastal Economies Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "ECO DATA PREP",
              sprintf("%s/ECO/eco_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#livelihoods-and-economies"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/ECO/eco_data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end ECO tab item

      ## >> liv ----
      ## Livelihoods
      tabItem(
        tabName = "liv",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Coastal Livelihoods"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "liv_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "liv_barplot",
            title_text = "Livelihoods Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "liv_map",
            title_text = "Map of Coastal Livelihoods Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "LIV DATA PREP",
              sprintf("%s/LIV/liv_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#livelihoods-and-economies"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/LIV/liv_data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end LIV tab item

      ## >> sp ----
      ## Sense of Place
      tabItem(
        tabName = "sp",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Sense of Place"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "sp_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "sp_barplot",
            title_text = "Sense of Place Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "sp_map",
            title_text = "Map of Sense of Place Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "MEANING OF THE GOAL",
              "http://ohi-science.org/goals/#sense-of-place"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end SP tab item

      ## >> ico ----
      ## Iconic Species
      tabItem(
        tabName = "ico",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Iconic Species"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "ico_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "ico_barplot",
            title_text = "Iconic Species Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "ico_map",
            title_text = "Map of Iconic Species Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "ICO DATA PREP",
              sprintf("%s/ICO/ico_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "http://ohi-science.org/goals/#sense-of-place"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/ICO/data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end ICO tab item

      ## >> lsp ----
      ## Lasting Special Places
      tabItem(
        tabName = "lsp",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Lasting Special Places"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "lsp_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "lsp_barplot",
            title_text = "Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "lsp_map",
            title_text = "Map of Lasting Special Places Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "LSP DATA PREP",
              sprintf("%s/LSP/lsp_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "http://ohi-science.org/goals/#sense-of-place"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/LSP/mpa_data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end LSP tab item

      ## >> np ----
      ## Natural Products
      tabItem(
        tabName = "np",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Natural Products"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "np_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "np_barplot",
            title_text = "Natural Products Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "np_map",
            title_text = "Natural Products Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "NP DATA PREP",
              sprintf("%s/NP/np_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#natural-products"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end NP tab item

      ## >> tr ----
      ## Tourism
      tabItem(
        tabName = "tr",

        ## header with scorebox and goal intro
        fluidRow(
          box(
            h1("Tourism"),
            width = 9
          ),
          column(
            width = 3,
            scoreBoxUI(id = "tr_infobox")
          ),
          box(
            p("SOMETHING ABOUT GOAL PHILOSOPHY"),
            width = 12
          )
        ),

        ## plots and maps and links
        fluidRow(
          barplotCardUI(
            id = "tr_barplot",
            title_text = "Tourism Goal Headway",
            sub_title_text = "Environmental benefit versus work still to be done. Bar lengths represent proximity to target level of 100, widths are region or basin (log-transformed) area.",
            box_width = 3
          ),
          mapCardUI(
            id = "tr_map",
            title_text = "Map of Tourism Scores",
            sub_title_text = "This map shows scores from the previous assessment (2014)",
            box_width = 6,
            ht = 555
          ),

          column(
            width = 3,
            text_links(
              "TR DATA PREP",
              sprintf("%s/TR/tr_prep.md", gh_prep)
            ),
            text_links(
              "MEANING OF THE GOAL",
              "https://ohi-science.org/goals/#tourism-and-recreation"
            ),
            text_links(
              "GOAL DATA",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/prep/TR/tr_data_database"
            ),
            text_links(
              "ALL DATA LAYERS",
              "https://github.com/OHI-Science/bhi-1.0-archive/tree/draft/baltic2015/layers"
            )
          )
        )
      ), # end TR tab item

      ## >> compare and summarize

      ## >> futures tab ----
      tabItem(
        tabName = "futures",
        fluidRow(
          box(
            h1("Likely Future versus Present"),
            width = 12
          ),
          box(
            p("SOMETHING ABOUT LIKELY FUTURE STATUS AND DIFFERENT DIMENSIONS OF OHI"),
            width = 12
          )
        )
      ), # end futures tab item

      ## pressures tab
      tabItem(
        tabName = "pressures",
        fluidRow(
          box(
            h1("Pressures (Page Under Construction)", style = "color:#9b363d"),
            width = 12
          ),
          box(
            p("SOMETHING ABOUT BHI PRESSURES, WITH TIME SERIES PLOTS"),
            width = 12
          ),

          ## pressures time series plot
          box(
            width = 9,
            addSpinner(plotlyOutput("pressure_ts"), spin = "rotating-plane", color = "#d7e5e8")
          ),
          ## pressure time series plot select variable ----
          box(
            width = 3,

            selectInput(
              "press_var",
              label = "Pressure Variable to Plot",
              choices = c(`Eutrophication` = "eut_time_data",
                          `Contaminants PCB` = "con_pcb_time_data",
                          `Contaminants Dioxin` = "con_dioxin_time_data",
                          `Anoxia Pressure` = "anoxia_press",
                          `Nitrogen Load Tonnes` = "N_basin_tonnes")
            )
          )
        )
      ), # end pressures tab item ----
      tabItem(
        tabName = "scenarios",
        fluidRow(
          box(
            h1("Scenario Exploration"),
            width = 12
          ),
          box(
            p("SOMETHING ABOUT SCENARIOS TESTING APPROACHES,  TRIALS,  RESULTS"),
            width = 12
          )
        )
      ), # end scenarios tab item

      ## >> layers tab ----
      tabItem(
        tabName = "layers",
        fluidRow(
          box(
            h1("Data Layers (Page Under Construction)", style = "color:#9b363d"),
            width = 12
          ),
          box(
            p("SOMETHING ABOUT PROCESS OF GENERATING LAYERS, AND LAYER VS LAYER SCATTERPLOT"),
            width = 12
          ),

          ## scatterplot from selected layers
          box(
            width = 9,
            plotOutput("layers_scatter")
          ),
          ## select variables for layers scatterplot ----
          box(
            width = 3,

            selectInput(
              "layerscatter_var_x",
              label = "Select Layer X",
              choices = c(
                `AO_STOCK_SLOPE` = "ao_stock_slope_bhi2015.csv",
                `AO_STOCK_STATUS` = "ao_stock_status_bhi2015.csv",
                `BD_SPP_STATUS` = "bd_spp_status_bhi2015.csv",
                `CC_SAL_DEEP` = "cc_sal_deep_bhi2015.csv",
                `CC_SAL_SURF` = "cc_sal_surf_bhi2015.csv",
                `CC_SST` = "cc_sst_bhi2015.csv",
                `CS_STATUS` = "cs_status_bhi2015.csv",
                `CW_CON_DIOXIN_STATUS` = "cw_con_dioxin_status_bhi2015.csv",
                `CW_CON_ICES6_STATUS` = "cw_con_ices6_status_bhi2015.csv",
                `CW_CON_PENALTY` = "cw_con_penalty_bhi2015.csv",
                `CW_CON_PFOS_STATUS` = "cw_con_pfos_status_bhi2015.csv",
                `CW_EUT_STATUS_SCORE` = "cw_eut_status_score_bhi2015.csv",
                `CW_NUT_ANOXIA_STATUS` = "cw_nut_anoxia_status_bhi2015.csv",
                `FIS_BBMSY` = "fis_bbmsy_bhi2015.csv",
                `FIS_FFMSY` = "fis_ffmsy_bhi2015.csv",
                `FIS_LANDINGS` = "fis_landings_bhi2015.csv",
                `FP_OVER_HARVEST` = "fp_over_harvest_bhi2015.csv",
                `FP_WILDCAUGHT_WEIGHT` = "fp_wildcaught_weight_bhi2015.csv",
                `HAB_ANOXIA` = "hab_anoxia_bhi2015.csv",
                `HAB_BOTTOM_TRAWL` = "hab_bottom_trawl_bhi2015.csv",
                `HAB_COASTAL_POP` = "hab_coastal_pop_bhi2015.csv",
                `HAB_ILLEGAL_OIL` = "hab_illegal_oil_bhi2015.csv",
                `ICO_STATUS` = "ico_status_bhi2015.csv",
                `LE_GDP_COUNTRY` = "le_gdp_country_bhi2015.csv",
                `LE_GDP_REGION` = "le_gdp_region_bhi2015.csv",
                `LIV_NATIONAL_EMPLOY` = "liv_national_employ_bhi2015.csv",
                `LIV_REGIONAL_EMPLOY` = "liv_regional_employ_bhi2015.csv",
                `LSP_STATUS_BY_RGN` = "lsp_status_by_rgn_bhi2015.csv",
                `MAR_COASTALPOPN2005_INLAND25KM` = "mar_coastalpopn2005_inland25km_bhi2015.csv",
                `MAR_HARVEST_SPECIES` = "mar_harvest_species_bhi2015.csv",
                `MAR_HARVEST_TONNES` = "mar_harvest_tonnes_bhi2015.csv",
                `MAR_SUSTAINABILITY_SCORE` = "mar_sustainability_score_bhi2015.csv",
                `NP_BBMSY` = "np_bbmsy_bhi2015.csv",
                `NP_FFMSY` = "np_ffmsy_bhi2015.csv",
                `NP_LANDINGS` = "np_landings_bhi2015.csv",
                `PO_ATMOS_PCB153` = "po_atmos_pcb153_bhi2015.csv",
                `PO_ATMOS_PCDDF` = "po_atmos_pcddf_bhi2015.csv",
                `PO_INVERSE_SECCHI` = "po_inverse_secchi_bhi2015.csv",
                `PO_NLOAD` = "po_nload_bhi2015.csv",
                `PO_PLOAD` = "po_pload_bhi2015.csv",
                `PO_TRASH` = "po_trash_bhi2015.csv",
                `RES_BIODIVERSITY` = "res_biodiversity_bhi2015.csv",
                `RES_REG_BIRDS` = "res_reg_birds_bhi2015.csv",
                `RES_REG_BSAP` = "res_reg_bsap_bhi2015.csv",
                `RES_REG_BWD` = "res_reg_bwd_bhi2015.csv",
                `RES_REG_CBD` = "res_reg_cbd_bhi2015.csv",
                `RES_REG_CFP` = "res_reg_cfp_bhi2015.csv",
                `RES_REG_CITES` = "res_reg_cites_bhi2015.csv",
                `RES_REG_COP21` = "res_reg_cop21_bhi2015.csv",
                `RES_REG_HD` = "res_reg_hd_bhi2015.csv",
                `RES_REG_HELCOM` = "res_reg_helcom_bhi2015.csv",
                `RES_REG_IED` = "res_reg_ied_bhi2015.csv",
                `RES_REG_MSFD` = "res_reg_msfd_bhi2015.csv",
                `RES_REG_MSPD` = "res_reg_mspd_bhi2015.csv",
                `RES_REG_ND` = "res_reg_nd_bhi2015.csv",
                `RES_REG_NEC` = "res_reg_nec_bhi2015.csv",
                `RES_REG_POP` = "res_reg_pop_bhi2015.csv",
                `RES_REG_REACH` = "res_reg_reach_bhi2015.csv",
                `RES_REG_UWWTD` = "res_reg_uwwtd_bhi2015.csv",
                `RES_REG_WFD` = "res_reg_wfd_bhi2015.csv",
                `SP_INVASIVES` = "sp_invasives_bhi2015.csv",
                `SPP_DIV_VULN` = "spp_div_vuln_bhi2015.csv",
                `SS_WGI` = "ss_wgi_bhi2015.csv",
                `TR_ACCOMMODATION_STAYS` = "tr_accommodation_stays_bhi2015.csv",
                `WGI_ALL` = "wgi_all_bhi2015.csv"
              )
            ),
            selectInput(
              "layerscatter_var_y",
              label = "Select Layer Y",
              choices = c(
                `AO_STOCK_SLOPE` = "ao_stock_slope_bhi2015.csv",
                `AO_STOCK_STATUS` = "ao_stock_status_bhi2015.csv",
                `BD_SPP_STATUS` = "bd_spp_status_bhi2015.csv",
                `CC_SAL_DEEP` = "cc_sal_deep_bhi2015.csv",
                `CC_SAL_SURF` = "cc_sal_surf_bhi2015.csv",
                `CC_SST` = "cc_sst_bhi2015.csv",
                `CS_STATUS` = "cs_status_bhi2015.csv",
                `CW_CON_DIOXIN_STATUS` = "cw_con_dioxin_status_bhi2015.csv",
                `CW_CON_ICES6_STATUS` = "cw_con_ices6_status_bhi2015.csv",
                `CW_CON_PENALTY` = "cw_con_penalty_bhi2015.csv",
                `CW_CON_PFOS_STATUS` = "cw_con_pfos_status_bhi2015.csv",
                `CW_EUT_STATUS_SCORE` = "cw_eut_status_score_bhi2015.csv",
                `CW_NUT_ANOXIA_STATUS` = "cw_nut_anoxia_status_bhi2015.csv",
                `FIS_BBMSY` = "fis_bbmsy_bhi2015.csv",
                `FIS_FFMSY` = "fis_ffmsy_bhi2015.csv",
                `FIS_LANDINGS` = "fis_landings_bhi2015.csv",
                `FP_OVER_HARVEST` = "fp_over_harvest_bhi2015.csv",
                `FP_WILDCAUGHT_WEIGHT` = "fp_wildcaught_weight_bhi2015.csv",
                `HAB_ANOXIA` = "hab_anoxia_bhi2015.csv",
                `HAB_BOTTOM_TRAWL` = "hab_bottom_trawl_bhi2015.csv",
                `HAB_COASTAL_POP` = "hab_coastal_pop_bhi2015.csv",
                `HAB_ILLEGAL_OIL` = "hab_illegal_oil_bhi2015.csv",
                `ICO_STATUS` = "ico_status_bhi2015.csv",
                `LE_GDP_COUNTRY` = "le_gdp_country_bhi2015.csv",
                `LE_GDP_REGION` = "le_gdp_region_bhi2015.csv",
                `LIV_NATIONAL_EMPLOY` = "liv_national_employ_bhi2015.csv",
                `LIV_REGIONAL_EMPLOY` = "liv_regional_employ_bhi2015.csv",
                `LSP_STATUS_BY_RGN` = "lsp_status_by_rgn_bhi2015.csv",
                `MAR_COASTALPOPN2005_INLAND25KM` = "mar_coastalpopn2005_inland25km_bhi2015.csv",
                `MAR_HARVEST_SPECIES` = "mar_harvest_species_bhi2015.csv",
                `MAR_HARVEST_TONNES` = "mar_harvest_tonnes_bhi2015.csv",
                `MAR_SUSTAINABILITY_SCORE` = "mar_sustainability_score_bhi2015.csv",
                `NP_BBMSY` = "np_bbmsy_bhi2015.csv",
                `NP_FFMSY` = "np_ffmsy_bhi2015.csv",
                `NP_LANDINGS` = "np_landings_bhi2015.csv",
                `PO_ATMOS_PCB153` = "po_atmos_pcb153_bhi2015.csv",
                `PO_ATMOS_PCDDF` = "po_atmos_pcddf_bhi2015.csv",
                `PO_INVERSE_SECCHI` = "po_inverse_secchi_bhi2015.csv",
                `PO_NLOAD` = "po_nload_bhi2015.csv",
                `PO_PLOAD` = "po_pload_bhi2015.csv",
                `PO_TRASH` = "po_trash_bhi2015.csv",
                `RES_BIODIVERSITY` = "res_biodiversity_bhi2015.csv",
                `RES_REG_BIRDS` = "res_reg_birds_bhi2015.csv",
                `RES_REG_BSAP` = "res_reg_bsap_bhi2015.csv",
                `RES_REG_BWD` = "res_reg_bwd_bhi2015.csv",
                `RES_REG_CBD` = "res_reg_cbd_bhi2015.csv",
                `RES_REG_CFP` = "res_reg_cfp_bhi2015.csv",
                `RES_REG_CITES` = "res_reg_cites_bhi2015.csv",
                `RES_REG_COP21` = "res_reg_cop21_bhi2015.csv",
                `RES_REG_HD` = "res_reg_hd_bhi2015.csv",
                `RES_REG_HELCOM` = "res_reg_helcom_bhi2015.csv",
                `RES_REG_IED` = "res_reg_ied_bhi2015.csv",
                `RES_REG_MSFD` = "res_reg_msfd_bhi2015.csv",
                `RES_REG_MSPD` = "res_reg_mspd_bhi2015.csv",
                `RES_REG_ND` = "res_reg_nd_bhi2015.csv",
                `RES_REG_NEC` = "res_reg_nec_bhi2015.csv",
                `RES_REG_POP` = "res_reg_pop_bhi2015.csv",
                `RES_REG_REACH` = "res_reg_reach_bhi2015.csv",
                `RES_REG_UWWTD` = "res_reg_uwwtd_bhi2015.csv",
                `RES_REG_WFD` = "res_reg_wfd_bhi2015.csv",
                `SP_INVASIVES` = "sp_invasives_bhi2015.csv",
                `SPP_DIV_VULN` = "spp_div_vuln_bhi2015.csv",
                `SS_WGI` = "ss_wgi_bhi2015.csv",
                `TR_ACCOMMODATION_STAYS` = "tr_accommodation_stays_bhi2015.csv",
                `WGI_ALL` = "wgi_all_bhi2015.csv"
              )
            ),
            selectizeInput(
              "layers_dt_vars",
              label = "Data Table Variables",
              choices = c(
                # `AO_STOCK_SLOPE` = "ao_stock_slope_bhi2015.csv",
                # `AO_STOCK_STATUS` = "ao_stock_status_bhi2015.csv",
                # `BD_SPP_STATUS` = "bd_spp_status_bhi2015.csv",
                `CC_SAL_DEEP` = "cc_sal_deep_bhi2015.csv",
                `CC_SAL_SURF` = "cc_sal_surf_bhi2015.csv",
                `CC_SST` = "cc_sst_bhi2015.csv",
                `CS_STATUS` = "cs_status_bhi2015.csv",
                `CW_CON_DIOXIN_STATUS` = "cw_con_dioxin_status_bhi2015.csv",
                `CW_CON_ICES6_STATUS` = "cw_con_ices6_status_bhi2015.csv",
                `CW_CON_PENALTY` = "cw_con_penalty_bhi2015.csv",
                `CW_CON_PFOS_STATUS` = "cw_con_pfos_status_bhi2015.csv",
                `CW_EUT_STATUS_SCORE` = "cw_eut_status_score_bhi2015.csv",
                `CW_NUT_ANOXIA_STATUS` = "cw_nut_anoxia_status_bhi2015.csv"
                # `FIS_BBMSY` = "fis_bbmsy_bhi2015.csv",
                # `FIS_FFMSY` = "fis_ffmsy_bhi2015.csv",
                # `FIS_LANDINGS` = "fis_landings_bhi2015.csv",
                # `FP_OVER_HARVEST` = "fp_over_harvest_bhi2015.csv",
                # `FP_WILDCAUGHT_WEIGHT` = "fp_wildcaught_weight_bhi2015.csv",
                # `HAB_ANOXIA` = "hab_anoxia_bhi2015.csv",
                # `HAB_BOTTOM_TRAWL` = "hab_bottom_trawl_bhi2015.csv",
                # `HAB_COASTAL_POP` = "hab_coastal_pop_bhi2015.csv",
                # `HAB_ILLEGAL_OIL` = "hab_illegal_oil_bhi2015.csv",
                # `ICO_STATUS` = "ico_status_bhi2015.csv",
                # `LE_GDP_COUNTRY` = "le_gdp_country_bhi2015.csv",
                # `LE_GDP_REGION` = "le_gdp_region_bhi2015.csv",
                # `LIV_NATIONAL_EMPLOY` = "liv_national_employ_bhi2015.csv",
                # `LIV_REGIONAL_EMPLOY` = "liv_regional_employ_bhi2015.csv",
                # `LSP_STATUS_BY_RGN` = "lsp_status_by_rgn_bhi2015.csv",
                # `MAR_COASTALPOPN2005_INLAND25KM` = "mar_coastalpopn2005_inland25km_bhi2015.csv",
                # `MAR_HARVEST_SPECIES` = "mar_harvest_species_bhi2015.csv",
                # `MAR_HARVEST_TONNES` = "mar_harvest_tonnes_bhi2015.csv",
                # `MAR_SUSTAINABILITY_SCORE` = "mar_sustainability_score_bhi2015.csv",
                # `NP_BBMSY` = "np_bbmsy_bhi2015.csv",
                # `NP_FFMSY` = "np_ffmsy_bhi2015.csv",
                # `NP_LANDINGS` = "np_landings_bhi2015.csv",
                # `PO_ATMOS_PCB153` = "po_atmos_pcb153_bhi2015.csv",
                # `PO_ATMOS_PCDDF` = "po_atmos_pcddf_bhi2015.csv",
                # `PO_INVERSE_SECCHI` = "po_inverse_secchi_bhi2015.csv",
                # `PO_NLOAD` = "po_nload_bhi2015.csv",
                # `PO_PLOAD` = "po_pload_bhi2015.csv",
                # `PO_TRASH` = "po_trash_bhi2015.csv",
                # `RES_BIODIVERSITY` = "res_biodiversity_bhi2015.csv",
                # `RES_REG_BIRDS` = "res_reg_birds_bhi2015.csv",
                # `RES_REG_BSAP` = "res_reg_bsap_bhi2015.csv",
                # `RES_REG_BWD` = "res_reg_bwd_bhi2015.csv",
                # `RES_REG_CBD` = "res_reg_cbd_bhi2015.csv",
                # `RES_REG_CFP` = "res_reg_cfp_bhi2015.csv",
                # `RES_REG_CITES` = "res_reg_cites_bhi2015.csv",
                # `RES_REG_COP21` = "res_reg_cop21_bhi2015.csv",
                # `RES_REG_HD` = "res_reg_hd_bhi2015.csv",
                # `RES_REG_HELCOM` = "res_reg_helcom_bhi2015.csv",
                # `RES_REG_IED` = "res_reg_ied_bhi2015.csv",
                # `RES_REG_MSFD` = "res_reg_msfd_bhi2015.csv",
                # `RES_REG_MSPD` = "res_reg_mspd_bhi2015.csv",
                # `RES_REG_ND` = "res_reg_nd_bhi2015.csv",
                # `RES_REG_NEC` = "res_reg_nec_bhi2015.csv",
                # `RES_REG_POP` = "res_reg_pop_bhi2015.csv",
                # `RES_REG_REACH` = "res_reg_reach_bhi2015.csv",
                # `RES_REG_UWWTD` = "res_reg_uwwtd_bhi2015.csv",
                # `RES_REG_WFD` = "res_reg_wfd_bhi2015.csv",
                # `SP_INVASIVES` = "sp_invasives_bhi2015.csv",
                # `SPP_DIV_VULN` = "spp_div_vuln_bhi2015.csv",
                # `SS_WGI` = "ss_wgi_bhi2015.csv",
                # `TR_ACCOMMODATION_STAYS` = "tr_accommodation_stays_bhi2015.csv",
                # `WGI_ALL` = "wgi_all_bhi2015.csv"
              )
            )
          )
        )
      ) # end layers tab item ----

    ) # end tabItems ----
  ) # end dashboardBody ----
) # end dashboardPage ----
