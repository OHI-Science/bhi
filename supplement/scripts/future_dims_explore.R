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
  filter(!is.na(Subbasin)) %>%
  group_by(goal, dimension, Subbasin) %>%
  summarise(basin_mean = weighted.mean(score, Area_km2, na.rm = TRUE)) %>% ## NaNs also should be removed..?
  spread(key = dimension, value = basin_mean) %>%
  mutate(future_greater = ifelse(future > status, TRUE, FALSE)) %>%
  gather(key = dimension, value = score, -goal, -Subbasin, -future_greater) %>%
  mutate(trend_rescale = ifelse(dimension == "trend", score * 100, score)) %>%
  mutate(trend_rescale = ifelse(trend_rescale > 100, score, trend_rescale))

View(scores_basin_trendscaled_futurevpresent)
hist(filter(scores_basin_trendscaled_futurevpresent, dimension == "trend", !is.na(score))$score)
hist(filter(scores_basin_trendscaled_futurevpresent, dimension == "trend", !is.na(score))$trend_rescale)
summary(filter(scores_basin_trendscaled_futurevpresent, dimension == "trend", !is.na(score))$trend_rescale)

basins_to_investigate <- c("Gulf of Riga", "Eastern Gotland Basin", "Gdansk Basin",  "Gulf of Finland")
basins_control_compare <- c("Bornholm Basin") # "Bothnian Bay"
goals_to_investigate <- c("BD", "EUT")

plot_data <- scores_basin_trendscaled_futurevpresent %>%
  filter(goal %in% goals_to_investigate,
         !is.na(Subbasin),
         Subbasin %in% c(basins_to_investigate, basins_control_compare))
plot_data$dims_reordered <- factor(
  plot_data$dimension,
  levels = c("status", "future", "trend", "pressures", "resilience", "score"))

dims <- c("present state", "likely future", "trend", "pressures", "resilience", "status")
# dims_pal = c("#8e332185", "#80b477", "#d68459", "#ead19c", "#b4ccde", "#8f79b1") # color palette?
dims_pal <- c("#a0bbd0e8", "#ead19cf0", "#de8b5fe8", "#b13a23db", "#63945ade", "#9483afed")

# ggplot(plot_data, aes(x = dims_reordered, y = trend_rescale, fill = dims_reordered)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_grid(cols = vars(Subbasin), rows = vars(goal)) +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_fill_manual(values = dims_pal)

ggplot(plot_data, aes(x = dims_reordered, y = trend_rescale, fill = dims_reordered)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian(ylim = c(-70, 100)) +
  facet_grid(cols = vars(Subbasin), rows = vars(goal)) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_light() +
  theme(axis.text.x = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        legend.direction = "horizontal",
        legend.position = c(0.3, 0.06),
        legend.background = element_rect(size = 0.6, colour = "grey"),
        axis.ticks.x = element_blank()) +
  labs(fill = element_blank(), x = element_blank(), y = element_blank()) +
  scale_fill_manual(
    values = dims_pal,
    labels  = c("Present Status", "Likely Future Status",
                "Trend", "Pressures", "Resilience", "Score"))


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

map_goal <- function(goal_code, dimension_name = "score"){
}

ggplot(data = baltic) +
  ## baltic countries
  geom_sf(fill = "#f0e7d6", alpha = 0.5, color = "#ae9f82", size = 0.15) +
  ## overlay goal scores by subbasin with custom continuous color palette
  geom_sf(data = simplepolys, aes(fill = CS), size = 0.15) +
  ## some formatting and map elements
  scale_fill_gradientn(colours = c("#8c031a", "#cc0033", "#fbff8f", "#ffff99", "#009999", "#0278a7"),
                       breaks = c(15, 40, 60, 75, 90, 99), limits = c(0, 100),
                       na.value = "#eaedf1") +
  # NA white, red from  0-50, green only 95+
  coord_sf(xlim = c(5, 32), ylim = c(53, 66), expand = FALSE) +
  guides(fill = guide_colorbar(barheight = unit(2.5, "in"),
                               frame.colour = "black",
                               ticks.colour = "black",
                               ticks.linewidth = 0.5,
                               frame.linewidth = 1)) +
  ggspatial::annotation_scale(location = "br", width_hint = 0.3) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true", pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  geom_sf_text(data = simplepolys, aes(label = Name)) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        panel.grid.major = element_line(color = gray(0.5), size = 0.5),
        legend.position = c(0.05, 0.85), legend.title = element_blank(),
        legend.spacing.x = unit(0.2, "cm"))


## double check for LSP -- (MPA area 10% of EEZ water for each  country, penalized by whether there is management plan- 0:no plan, 0.4:partial or 1)
## double ckech MPA area % calculation (no wrong calculation, should be by country not by subbasin!)
## Alternative 4 in teh bhi archive lsp_prep docuemtn
