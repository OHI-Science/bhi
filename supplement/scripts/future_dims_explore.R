## libraries and directories
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(rnaturalearth)

wd <- "/Users/eleanorecampbell/Desktop/GitHub"
dir_sp <- "/Volumes/BHI_share/Shapefiles"

## read in datasets: scores and basins-lookup for aggregating bhi rgn scores to subbasin
scores2015 <- readr::read_csv(file.path(wd, "bhi-1.0-archive", "baltic2015", "scores.csv"))
basin_ids <- readr::read_delim(file.path(wd, "bhi-prep", "ref", "lookup_tabs",
                                         "bhi_basin_country_lookup.csv"), delim = ";")

## wrangle and plot dimensions
scores_basin_trendscaled_futurevpresent <- scores2015 %>%
  left_join(select(basin_ids, BHI_ID, Subbasin, Area_km2),
            by = c("region_id" = "BHI_ID")) %>%
  group_by(goal, dimension, Subbasin) %>%
  summarise(basin_mean = weighted.mean(score, Area_km2)) %>%
  spread(key = dimension, value = basin_mean) %>%
  mutate(future_greater = ifelse(future > status, TRUE, FALSE)) %>%
  gather(key = dimension, value = score, -goal, -Subbasin, -future_greater) %>%
  mutate(trend_rescale = ifelse(dimension == "trend", score * 100, score)) %>%
  mutate(trend_rescale = ifelse(trend_rescale > 100, score, trend_rescale)) # View(scores_basin_trendscaled_futurevpresent)

basins_to_investigate <- c("Gulf of Riga", "Eastern Gotland Basin", "Gdansk Basin",  "Gulf of Finland")
basins_control_compare <- c("Bornholm Basin", "Bothnian Bay")
goals_to_investigate <- c("BD", "EUT")

plot_data <- scores_basin_trendscaled_futurevpresent %>%
  filter(goal %in% goals_to_investigate,
         !is.na(Subbasin),
         Subbasin %in% c(basins_to_investigate, basins_control_compare))

dims <- c("likely future", "pressures", "resilience", "status", "present state", "trend")
dims_pal = c("#f4e2bc", "#c4472f", "#80b477", "#a7b0bc", "#bad5e6", "#d68459") # color palette?

ggplot(plot_data, aes(x = dimension, y = trend_rescale, fill = factor(dimension))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  facet_grid(cols = vars(Subbasin), rows = vars(goal)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = dims_pal)


## correlation between trend and difference between pressure and resilience?
## seems more negative r - p should mean --> more negative T...

scores_wide <- scores_basin_trendscaled_futurevpresent %>%
  select(-trend_rescale) %>%
  group_by(goal, dimension, Subbasin, future_greater) %>%
  spread(key = dimension, value = score) %>%
  mutate(pres_res_diff = resilience - pressures)

plot(scores_wide$trend, scores_wide$pres_res_diff,
     col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5),
     cex = 0.8, pch = 20)

cor.test(scores_wide$trend, scores_wide$pres_res_diff)

## mapping scores by subbasin:
## carbon storage, artisanal opportunity, lasting special places, eutrophication
shp_name <- "HELCOM_subbasins_holasbasins"
helcom_rgns <- sf::st_read(file.path(dir_sp, shp_name), shp_name) %>%
  mutate(Name = stringr::str_replace(Name, pattern = "Å", replacement = "A"))

mapping_goals <- c("CS", "AO", "LSP", "EUT")

mapping_data <- scores_basin_trendscaled_futurevpresent %>%
  filter(dimension == "score") %>%
  select(score, goal, Subbasin) %>%
  spread(key = goal, value = score) %>%
  select(Subbasin, mapping_goals) %>%
  filter(!is.na(Subbasin))

mapping_data_sp <- helcom_rgns %>%
  left_join(mapping_data, by = c("Name" = "Subbasin")) %>%
  st_transform(crs = 4326)

simplepolys <- rmapshaper::ms_simplify(input = mapping_data_sp) %>% st_as_sf()

baltic <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_crop(xmin = -5, xmax = 35, ymin = 48, ymax = 70)

ggplot(data = baltic) +
  ## baltic countries
  geom_sf(fill = "#f0e7d6", alpha = 0.7) +
  ## overlay goal scores by subbasin with custom continuous color palette
  geom_sf(data = simplepolys, aes(fill = LSP, color = NULL)) +
  scale_fill_viridis_c(alpha = 0.9) +
  ## some formatting and map elements
  coord_sf(xlim = c(5, 30), ylim = c(51, 67), expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(0.5), size = 0.5),
        panel.background = element_rect(fill = "aliceblue")) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.3) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true", pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering)




