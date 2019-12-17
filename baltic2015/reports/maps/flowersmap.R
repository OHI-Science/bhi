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

## dataframe of basin flowerplots info
## filenames without png extension and xy placement positions for plotting
basinsinfo <- data.frame(
  ## left align
  flowerplot517base_bothnian_bay = c(x = 0.81, y = 0.96, x0 = 0.65, y0 = 0.94, h = 0, v = 0.5),
  flowerplot515base_bothnian_sea = c(x = 0.67, y = 0.73, x0 = 0.50, y0 = 0.7, h = 0, v = 0.5),
  flowerplot513base_gulf_of_finland = c(x = 0.90, y = 0.68, x0 = 0.76, y0 = 0.52, h = 0, v = 0.25),
  flowerplot509base_eastern_gotland_basin = c(x = 0.75, y = 0.26, x0 = 0.53, y0 = 0.22, h = 0, v = 0.5),
  flowerplot511base_gulf_of_riga = c(x = 0.87, y = 0.41, x0 = 0.69, y0 = 0.33, h = 0, v = 0.25),
  flowerplot508base_gdansk_basin = c(x = 0.63, y = 0.11, x0 = 0.525, y0 = 0.055, h = 0, v = 0.25),
  ## right align
  flowerplot510base_western_gotland_basin = c(x = 0.41, y = 0.43, x0 = 0.46, y0 = 0.33, h = 0.5, v = -0.25),
  flowerplot507base_bornholm_basin = c(x = 0.34, y = 0.27, x0 = 0.41, y0 = 0.13, h = 0.5, v = -0.25),
  flowerplot501base_kattegat = c(x = 0.15, y = 0.28, x0 = 0.22, y0 = 0.24, h = 0.5, v = -0.25),
  flowerplot506base_arkona_basin = c(x = 0.08, y = 0.05, x0 = 0.29, y0 = 0.07, h = 0.5, v = -0.25)
)
plotlines_df <- basinsinfo %>%
  filter(rownames(basinsinfo) %in% c("x", "y", "x0", "y0")) %>%
  cbind(var = c("x", "y", "x0", "y0")) %>%
  gather("basin", "value", -var) %>%
  mutate(var = as.character(var)) %>%
  spread(var, value)

## combine everything into one figure using cowplot!
## start with base map and add main flowerplot,
## and add plotlines for other flowerplots
flowermap <- ggdraw(balticmap) +

  ## arrows from small plots pointing to basins
  draw_plot(
    ggplot(data = plotlines_df) +
      geom_curve(
        aes(x = x, xend = x0, y = y, yend = y0),
        data = plotlines_df,
        arrow = arrow(length = unit(0.14, "cm")),
        curvature = 0.35,
        size = 0.2,
        color = "#687c92"
      ) +
      theme(
        legend.position = "none",
        axis.text = element_blank()
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      labs(x = NULL, y = NULL),
    x = 0.08, y = 0.03,
    width = 0.88, height = 0.97
  ) +

  ## main flowerplot in a box with labels
  draw_image(
    file.path(flowerplotloc, "flower_curvetxt_baltic_sea.png") %>%
      magick::image_read() %>%
      magick::image_background("#f3f5f7") %>%
      magick::image_border(color = "grey20", geometry = "5x5"),
    x = 0.055, y = 0.60,
    width = 0.385, height = 0.385
  ) +
  draw_image(
    file.path(flowerplotloc, "flowerplot0base_baltic_sea.png") %>%
      magick::image_read() %>%
      magick::image_transparent(color = "white"),
    x = 0.055, y = 0.60,
    width = 0.385, height = 0.385
  )

## now loop through to add all other basin plots
for(basin in names(basinsinfo)){
  p <- basinsinfo[c("x", "y", "h", "v"), basin]
  flowermap <- flowermap +
    draw_image(
      file.path(flowerplotloc, paste0(basin, ".png")) %>%
        magick::image_read() %>%
        magick::image_trim() %>%
        magick::image_transparent(color = "white"),
      x = p[1]*0.8+0.1, y = p[2]*0.85+0.05,
      width = 0.15, height = 0.15,
      hjust = p[3], vjust = p[4]
    )
}
ggsave(
  "flowermap.png", device = "png",
  path = here("baltic2015", "reports", "maps"),
  dpi = 320, width = 26, height = 22, units = c("cm")
)
