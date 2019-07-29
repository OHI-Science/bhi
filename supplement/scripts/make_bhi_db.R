library(DBI)
library(dbplyr)
library(dplyr)
library(RSQLite)

bhi_db_file <- "/Users/eleanorecampbell/Desktop/bhi-config.sqlite"
bhi_conf_db <- src_sqlite(bhi_db_file, create = TRUE)
con <- DBI::dbConnect(RSQLite::SQLite(), bhi_db_file)


dir <- "~/Desktop/GitHub/bhi/spatial"

region_areas <- readr::read_csv(file.path(dir,  "regions_lookup_complete_wide.csv")) %>%
  select(region_id, area_km2_rgn) %>%
  rename(area_km2 = area_km2_rgn)
copy_to(con, region_areas, "region_areas",temporary = FALSE, indexes = list("region_id", "area_km2"))


helcom_ids <- readr::read_csv(file.path(dir, "bhi_basin_country_lookup.csv")) %>%
  select(BHI_ID, HELCOM_ID, rgn_nam) %>%
  rename(region_id = BHI_ID, helcom_id = HELCOM_ID, eez = rgn_nam) %>%
  select(eez, helcom_id, region_id)
copy_to(con, helcom_ids, "helcom_ids", temporary = FALSE, indexes = list("region_id", "helcom_id"))


basin_areas <- region_areas %>%
  left_join(helcom_ids, by = "region_id") %>%
  select(-region_id, -eez) %>%
  group_by(helcom_id) %>%
  summarize(basin_area = sum(area_km2))

helcom_basins <- readr::read_csv(file.path(dir, "bhi_basin_country_lookup.csv")) %>%
  select(HELCOM_ID, Subbasin) %>%
  rename(helcom_id = HELCOM_ID, subbasin = Subbasin) %>%
  distinct() %>%
  left_join(readr::read_csv(file.path(dir, "subbasins_ordered.csv")), by = "subbasin") %>%
  left_join(basin_areas, by = "helcom_id") %>%
  rename(area_km2 = basin_area) %>%
  select(helcom_id, subbasin, area_km2, order)
copy_to(con, helcom_basins, "helcom_basins",  temporary = FALSE, indexes = list("helcom_id"), overwrite = TRUE)


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
