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


