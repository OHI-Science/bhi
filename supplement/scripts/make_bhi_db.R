library(DBI)
library(dbplyr)
library(dplyr)
library(RSQLite)

bhi_db_file <- "/Users/eleanorecampbell/Desktop/bhi-config.sqlite"
bhi_conf_db <- src_sqlite(bhi_db_file, create = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), bhi_db_file)


dir <- "~/Desktop/GitHub/bhi/spatial"


helcom_ids <- readr::read_csv(file.path(dir, "bhi_basin_country_lookup.csv")) %>%
  select(BHI_ID, HELCOM_ID, rgn_nam) %>%
  rename(region_id = BHI_ID, helcom_id = HELCOM_ID, eez = rgn_nam) %>%
  select(eez, helcom_id, region_id) %>%
  mutate(subbasin_id = as.numeric(str_extract(helcom_id, pattern = "[0-9]+")) + 500)
copy_to(con, helcom_ids, "helcom_ids", temporary = FALSE, indexes = list("region_id", "helcom_id", "subbasin_id"), overwrite = TRUE)


basin_areas <- region_areas %>%
  left_join(helcom_ids, by = "region_id") %>%
  select(-region_id, -eez) %>%
  group_by(helcom_id) %>%
  summarize(basin_area = sum(area_km2))

basins <- readr::read_csv(file.path(dir, "bhi_basin_country_lookup.csv")) %>%
  select(HELCOM_ID, Subbasin) %>%
  rename(helcom_id = HELCOM_ID, subbasin = Subbasin) %>%
  distinct() %>%
  left_join(readr::read_csv(file.path(dir, "subbasins_ordered.csv")), by = "subbasin") %>%
  left_join(basin_areas, by = "helcom_id") %>%
  rename(area_km2 = basin_area) %>%
  select(helcom_id, subbasin, area_km2, order) %>%
  mutate(subbasin_id = as.numeric(str_extract(helcom_id, pattern = "[0-9]+")) + 500) %>%
  select(helcom_id, subbasin_id, subbasin, area_km2, order)
copy_to(con, basins, "basins",  temporary = FALSE, indexes = list("helcom_id"), overwrite = TRUE)


eezs <- readr::read_csv(file.path(dir, "regions_lookup_complete_wide.csv")) %>%
  select(eez_id, eez_name) %>%
  rename(eez = eez_name) %>%
  distinct() %>%
  left_join(readr::read_csv(file.path(dir, "eez_lookup.csv")) %>%
              rename(eez_id_short = eez, eez_code = rgn_key) %>%
              rename(eez = country_name), by = "eez") %>%
  select(eez, eez_id, eez_code)
copy_to(con, eezs, "eezs", temporary = FALSE, indexes = list("eez", "eez_id"))


ices_ids <- readr::read_csv(file.path(dir, "bhi_ices_ids.csv")) %>%
  rename(region_id = BHI_ID, ices_id = ICES_ID, ices_numeric = ICES_numeric) %>%
  select(ices_id, ices_numeric, region_id)
copy_to(con, ices_ids, "ices_ids", temporary = FALSE, indexes = list("ices_id", "region_id"))


scores2015 <- readr::read_csv(paste0(here::here(), "-1.0-archive/baltic2015/scores.csv")) %>%
  select(region_id, goal, dimension, score)
copy_to(con, scores2015, "scores2015", temporary = FALSE, indexes = list("region_id", "goal", "dimension"))


order_rgns <- data.frame(order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                   11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                                   21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                                   31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42),
                         region_id = c(41, 42, 39, 40, 37, 38, 36, 35, 33, 32, 34, 30, 29, 31,
                                       28, 27, 25, 26, 20, 24, 23, 22, 19, 18, 21, 14, 15, 17, 16,
                                       13, 11,  12, 9, 10, 8, 7, 6, 5, 4, 3, 2, 1))

regions <- tbl(bhi_db_con, "helcom_ids") %>%
  left_join(tbl(bhi_db_con, "basins"), by = "helcom_id") %>%
  select(subbasin, eez, region_id) %>%
  collect() %>%
  mutate(region_name = paste0(subbasin, ", ", eez)) %>%
  select(region_id, region_name, subbasin, eez) %>%
  left_join(order_rgns, by = "region_id") %>%
  collect() %>%
  left_join(tbl(bhi_db_con, "region_areas") %>% collect(), by = "region_id") %>%
  select(region_id, subbasin, eez,  region_name, area_km2, order)

copy_to(con, regions, "regions", temporary = FALSE, indexes = list("region_id"), overwrite = TRUE)


dim_descriptions <-  readr::read_csv(file.path(here::here(), "supplement/tables/dimension_descriptions.csv")) %>%
  select(dimension, description)

copy_to(bhi_db_con, dim_descriptions, "dim_descriptions", temporary = FALSE, indexes = list("dimension"))


press_datasets_w_yr <- c(
  "prep/pressures/atmos_contaminants/data_database/pcddf.csv",
  "prep/pressures/wgi_social/data_database/wgi_combined_scores_by_country.csv",
  "prep/pressures/climate_change/salinity_climatechange/sal_data_database/hind_sal_deep.csv",
  "prep/pressures/climate_change/salinity_climatechange/sal_data_database/hind_sal_surf.csv",
  "prep/pressures/climate_change/temperature_climatechange/temp_data_database/hind_sst.csv",
  "prep/pressures/nutrient_load/nutrient_data_database/N_basin_load.csv",
  "prep/pressures/nutrient_load/nutrient_data_database/P_basin_load.csv",
  "prep/pressures/illegal_oil_discharge/oil_data_raw.csv",
  "prep/pressures/open_sea_anoxia/anoxia_pressure_scores_all_rgns_yrs.csv",
  "visualize/con_dioxin_time_data.csv",
  "visualize/con_pcb_time_data.csv",
  "visualize/eut_time_data.csv"
)
press_datasets_w_yr <- file.path("/Users/eleanorecampbell/Desktop/GitHub/bhi-1.0-archive/baltic2015", press_datasets_w_yr)

for(i in press_datasets_w_yr){
  tmp <- read.csv(i)
  assign(basename(str_remove(i, ".csv")), tmp)
  print(paste("loaded", basename(str_remove(i, ".csv"))))
}

pcddf <- pcddf %>%
  # mutate(metric = str_replace_all(paste(unit, substance), " ", "")) %>%
  select(-basin_abb, -data_type, -substance, -unit) %>%
  rename(subbasin = basin_loading) %>%
  # gather(key = year, value = value, c(-subbasin, -metric))
  gather(key = year, value = value, c(-subbasin)) %>%
  left_join(tbl(bhi_db_con, "basins") %>% select(subbasin, subbasin_id) %>% collect(), by = "subbasin") %>%
  filter(!is.na(subbaasin)) # has misssing basins, need to split  eg baltic proper...


for_now_press_dat <- eut_time_data %>%
  select(region_id = rgn_id, year,  eut_time_data = value) %>%
  left_join(con_pcb_time_data %>% select(region_id = rgn_id, year,  con_pcb_time_data = value), by = c("region_id",  "year")) %>%
  left_join(con_dioxin_time_data %>% select(region_id = rgn_id, year,  con_dioxin_time_data = value), by = c("region_id",  "year")) %>%
  left_join(anoxia_pressure_scores_all_rgns_yrs %>% select(region_id = rgn_id, year,  anoxia_press = pressure_score), by = c("region_id",  "year")) %>%
  left_join(
    N_basin_load %>%
      left_join(tbl(bhi_db_con, "basins") %>%
                  select(basin = subbasin, region_id = subbasin_id) %>%
                  collect(),
                by = "basin") %>%
      filter(!is.na(region_id)) %>%
      select(region_id, year, N_basin_tonnes = tonnes),
    by = c("region_id",  "year"))

copy_to(bhi_db_con, for_now_press_dat, "for_now_press_data", temporary = FALSE)


short_defs <- tibble(
  goal = collect(tbl(bhi_db_con, "plot_conf"))$goal,
  short_def = c(
    "Seafood sustainably harvested primarily in human consumption",
    "Harvest of sustainably caught wild seafood",
    "Production of sustainable cultured seafood",
    "Opportunity to engage in coastal non-recreational fishing",
    "Sustainable harvest of natural products, such as shells, algae, and fish oil used for reasons other than food provision",
    "Conservation status of natural habitats affording long-lasting carbon storage",
    "Opportunity to enjoy coastal areas for recreation and tourism",
    "Coastal and ocean-dependent livelihoods and productive coastal economies",
    "Jobs and wages from marine-related sectors",
    "Revenues from marine-related sectors",
    "Protection of coastal and marine features that contribute to sense of cultural identity",
    "Cultural, spiritual, or aesthetic connection to the environment afforded by iconic species",
    "Geographic locations that hold particular value for aesthetic, spiritual, cultural, recreational or existence reasons, and assesses how well they are protected",
    "Levels of pollution in marine areas",
    "The degree to which marine areas are unpolluted by nutrients",
    "The degree to which marine areas are unpolluted by trash",
    "The degree to which marine areas are unpolluted by contaminants",
    "The existence value of biodiversity measured through the conservation status of marine-associated species"
  )
)

goals_defs <- tbl(bhi_db_con, "plot_conf") %>%
  select(goal, parent, name, description) %>%
  collect() %>%
  left_join(short_defs, by = "goal")

copy_to(bhi_db_con, goals_defs, "goals", temporary = FALSE)



