library(sf)
library(dplyr)
library(ggplot2)

## the two shapefiles on the BHI_share, mac Mini
helcombasin_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/HELCOM_subbasins_holasbasins", "HELCOM_subbasins_holasbasins")
bhirgn_shp <- sf::st_read("/Volumes/BHI_share/Shapefiles/BHI_shapefile", "BHI_shapefile")

## wrangle shapefiles,
## check area columns already in attribute table,
## create subbasin columns with a few corrections so can join tabless

## helcom basins shapefile
helcombasin_shp <- helcombasin_shp %>%
  cbind(chk_area_km2 = as.numeric(st_area(helcombasin_shp))/(10^6)) %>%
  mutate(subbasin = ifelse(Name == "Åland Sea", "Aland Sea", as.character(Name))) %>%
  select(subbasin, area_helcom = chk_area_km2)
st_geometry(helcombasin_shp) <- NULL
head(helcombasin_shp)

## BHI regions shapefile
bhirgn_shp <- bhirgn_shp %>%
  cbind(chk_area_km2 = as.numeric(st_area(bhirgn_shp))/(10^6)) %>%
  mutate(subbasin = ifelse(Subbasin == "Bothian Sea", "Bothnian Sea", as.character(Subbasin)))
st_geometry(bhirgn_shp) <- NULL
head(bhirgn_shp)


## compare BHI region areas with areas used for baltic2015 score calculations:
baltic2015_rgninfo <- read_csv(here("baltic2015", "regions_lookup_complete_wide.csv"))
compare_rgn_areas <- left_join(
  select(baltic2015_rgninfo, region_id, baltic2015_area = area_km2_rgn),
  select(bhirgn_shp, region_id = BHI_ID, shp_area = chk_area_km2),
  by = "region_id") %>%
  mutate(
    percentdiffs = 100*(baltic2015_area - shp_area)/(0.5*(baltic2015_area + shp_area)),
    rounddiffs = round(percentdiffs, 3)
  )
summary(compare_rgn_areas$rounddiffs)


## comparing basin areas:

## aggregate BHI regions to subbasin areas
bhirgn_shp <- bhirgn_shp %>%
  group_by(subbasin) %>%
  summarize(area_bhi = sum(chk_area_km2))

## join tables, calculate percent differences, plot
compare_areas <- bhirgn_shp %>%
  left_join(helcombasin_shp, by = "subbasin") %>%
  mutate(percentdiff = (area_helcom - area_bhi)/(0.5*(area_helcom + area_bhi))*100)

ggplot(compare_areas) + geom_histogram(aes(percentdiff))
View(compare_areas)

## opening the files in QGIS or similar you can see the missmatches,
## even once the basin shp is reprojected to EPSG:4326 - WGS84 to match the BHI shp.
## BHI_shapefile seems to be higher resolution, with less angular lines around islands etc
## adding a baselayer, one can see some errors in the basin shp
## e.g. Björkö in the Quark and some other islands near Turku are straightup missing...
## these missing islands may help explain why Aland has the second largest percent difference
## Kattegat has the largest percent diff, probably because the basins shp includes Limfjord and the bhi shp does not

## save area/comparison datasets
compare_areas %>%
  write_csv(file.path("~/Desktop", "compare_shapefiles_df.csv"))

compare_areas %>%
  select(subbasin, area_km2 = area_bhi) %>%
  write_csv(file.path("~/Desktop", "area_baltic_basins.csv"))
