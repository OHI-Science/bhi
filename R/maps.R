## Libraries
source(file.path(here::here(), "R", "common.R"))
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)


## Functions


subbasin_goal_map <- function(){
}

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
simplepolys_x2 <- rmapshaper::ms_simplify(input = simplepolys) %>% st_as_sf()


baltic <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_crop(xmin = -5, xmax = 35, ymin = 48, ymax = 70)

map_goal <- function(goal_code, dimension_name = "score"){
}

## used width:730 when saved plots..
ggplot(data = baltic) +
  ## baltic countries
  geom_sf(fill = "#f0e7d6", alpha = 0.6, color = "#b2996c", size = 0.1) + # f0e7d6
  ## overlay goal scores by subbasin with custom continuous color palette
  geom_sf(data = simplepolys, aes(fill = EUT), color = "#acb9b6", size = 0.1) +
  ## some formatting and map elements
  scale_fill_gradientn(colours = c("#8c031a", "#cc0033", "#fff78a", "#f6ffb3", "#009999", "#0278a7"),
                       breaks = c(15, 40, 60, 75, 90, 99), limits = c(0, 100),
                       na.value = "#fcfcfd") +
  coord_sf(xlim = c(5, 32), ylim = c(53, 66), expand = FALSE) +
  guides(fill = guide_colorbar(barheight = unit(2.5, "in"), # legend formatting
                               frame.colour = "black",
                               ticks.colour = "black",
                               ticks.linewidth = 0.5)) +
  labs(x = element_blank(), y = element_blank()) + # no xy axes labels
  ggspatial::annotation_scale(location = "br", width_hint = 0.3) + # scalebar
  ggspatial::annotation_north_arrow( # north arrow
    location = "br", which_north = "true", pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  ## subbasin labels either with ggrepel or regular geom_text
  ggrepel::geom_text_repel(data = simplepolys,
                           aes(label = Name, geometry = geometry),
                           stat = "sf_coordinates",
                           size = 3.25) +
  # ggrepel::geom_text(data = simplepolys, aes(label = Name, ), size = 2.6, nudge_y = 0.2) +
  theme(panel.background = element_rect(fill = "#ddedfd"),
        panel.grid.major = element_line(color = gray(0.5), size = 0.5),
        legend.position = c(0.08, 0.75), # for no legend use "none"
        legend.title = element_blank(),
        legend.background = element_rect(fill = "aliceblue"),
        legend.spacing.x = unit(0.25, "cm"))
