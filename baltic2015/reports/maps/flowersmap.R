library(here)
library(cowplot)
source(here("R", "setup.R"))
source(here("R", "theme.R"))

## contrived data filled with  NAs for Index, so can  have empty polygons
## replace "Index" scores since map_general function requires using one of existing goals
scores_map <- readr::read_csv(here("baltic2015", "scores.csv")) %>%
  filter(goal != "Index") %>%
  rbind(
    data.frame(
      goal = rep("Index", 69),
      dimension = "score",
      region_id = c(0:42, 301:309, 501:517),
      score = rep(NA, 69)
    )
  )

## update theme for NA map color, then source map function
thm <- apply_bhi_theme()
thm$cols$map_background1 <- "#dfe5ecf7"
source(here("R", "maps.R"))

subbasin_shp <- st_read("/Volumes/BHI_share/Shapefiles/HELCOM_subbasins_holasbasins",  "HELCOM_subbasins_holasbasins")
balticmap <- map_general("Index", "subbasins", shp = subbasin_shp, scores_csv = scores_map) +
  theme(panel.grid.major = element_line(color = "#b7c1ccfa"))

## make flowerplotss if needed...
source(here("R", "flowerplot.R"))
scores <- readr::read_csv(here("baltic2015", "scores.csv"))
flowerplot(
  rgn_scores = scores, rgns = c(0, 501:517), plot_year = 2014, dim = "score",
  include_ranges = TRUE, dir_assess = here("baltic2015"),
  dir_config = here("baltic"), labels = "arc", color_by = "goal"
)

## flowerplots for basins, make backgrounds transparent
flowerplotloc <- here("baltic2015", "reports", "flowerplots")
basin1flower <- magick::image_transparent(
  magick::image_read(file.path(
    flowerplotloc,
    "flowerplot517base_bothnian_bay.png"
  )),
  color = "white"
)
basin2flower <- magick::image_transparent(
  magick::image_read(file.path(
    flowerplotloc,
    "flowerplot515base_bothnian_sea.png"
  )),
  color = "white"
)
basin3flower <- magick::image_transparent(
  magick::image_read(file.path(
    flowerplotloc,
    "flowerplot513base_gulf_of_finland.png"
  )),
  color = "white"
)
basin4flower <- magick::image_transparent(
  magick::image_read(file.path(
    flowerplotloc,
    "flowerplot509base_eastern_gotland_basin.png"
  )),
  color = "white"
)

## combine everything into one figure using cowplot!
ggdraw(balticmap) +
  ## main flowerplot
  draw_image(
    magick::image_transparent(
      magick::image_read(file.path(
        flowerplotloc,
        "flowerplot0base_baltic_sea.png"
      )),
      color = "white"
    ),
    x = 0.07, y = 0.4,
    width = 0.46, height = 0.46
  ) +
  draw_image(
    file.path(
      flowerplotloc,
      "flower_curvetxt_baltic_sea.png"
    ),
    x = 0.07, y = 0.4,
    width = 0.46, height = 0.46
  ) +
  draw_image(basin1flower, x = 0.52, y = 0.77, width = 0.22, height = 0.22) +
  draw_image(basin2flower, x = 0.44, y = 0.58, width = 0.22, height = 0.22) +
  draw_image(basin3flower, x = 0.62, y = 0.43, width = 0.22, height = 0.22) +
  draw_image(basin4flower, x = 0.44, y = 0.18, width = 0.22, height = 0.22)
