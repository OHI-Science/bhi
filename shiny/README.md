## Baltic Health Index Dashboard

The Baltic Health Index Dashboard is a data exploration tool, for visualizing the results as well as input data for the assessment.

---

**`server.R`**

The server-side logic of the dashboard. Calls module server functions, maps user inputs to create different outputs, etc. Organized in same order as dashboard sidebar: welcome, goals ordered alphabetically with subgoals nested alphabetically, futures, pressures, scenarios, data layers.

**`ui.R`**

The user-interface side logic. Defines the layout of the app app and reactive inputs/outputs which feed into the server logic.

**`global.R`**

Libraries:

Functions:

Shiny Global Data:

---

### modules

**`flowerplot_card.R`**

**`map_card.R`**

**`map_rgn_card.R`**

**`scorebox_card.R`**

**`barplot_card.R`**


### www

**`style.css`**

Extra css code to override base template, to customize dashboard appearance. Note: some style-specific css is sprinkled throughout `ui.R` as well.
