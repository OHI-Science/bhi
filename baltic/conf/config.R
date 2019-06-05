
## region data layers and spatial config ----
## a list of possible id fields used in datalayers (most will use rgn_id, but not always)
layers_id_fields <- c("rgn_id", "cntry_key", "fao_id", "fao_saup_id", "country_id", "saup_id", "fao_ohi_id")

## the official list of regions, and corresponding names
## note: this is a csv file in the layers folder
layer_region_labels <- "rgn_labels"

## the official ocean areas of each region, used to weight each subregions contribution to the region score
## note: this is a csv file in the layers folder
layer_region_areas <- "rgn_area"

## spatial configuration, to be used by shiny app and future ohicore mapping functions that use leaflet
geojson <- "spatial/regions_gcs_simple2x.geojson"


## pressures & resilience matrices ----

## for goals with elements
## these describe how to weight contribution of each element in calculation of pressure and resilience dimensions
resilience_element <- NULL # for BHI no goals have elements currently
pressures_element <- NULL # for BHI no goals have elements currently

# resilience_element = list("NP"  = "np_harvest_product_weight",
#                           "CS"  = "element_wts_cs_km2_x_storage",      # populated in CS() in functions.R
#                           "CP"  = "element_wts_cp_km2_x_protection",   # populated in CP() in functions.R
#                           "HAB" = "element_wts_hab_pres_abs")          # populated in HAB() in functions.R
#
# pressures_element  = list('NP'  = 'np_harvest_product_weight',
#                           'CS'  = 'element_wts_cs_km2_x_storage',      # populated in CS() in functions.R
#                           'CP'  = 'element_wts_cp_km2_x_protection',   # populated in CS() in functions.R
#                           'LIV' = 'le_sector_weight',
#                           'ECO' = 'le_sector_weight',
#                           'HAB' = 'element_wts_hab_pres_abs')          # populated in CS() in functions.R


## constants for dimensions' relative importance ----
## used by ohicore::Calculate functions to calculate pressures/resilience/trend/likely future state

## relative importance of social vs ecological pressures
## (pressure = gamma * ecological + (1-gamma) * social)
pressures_gamma = 0.5

## relative importance of social vs ecological resiliences
## (resilience = gamma * ecological + (1-gamma) * social)
resilience_gamma = 0.5

## used to calculate likely future state
goal_discount = 1.0

## relative importance of trend vs. pressure/resilience on likely future state
## if goal_beta = 0.67, trend is twice as important as pressure/resilience
goal_beta = 0.67
default_trend = 0


## descriptions and metadata ----

## extra descriptions not covered by goals.description or layers.description
index_description <- "The overall Index represents the weighted average of all goal scores."

loc <- file.path(dir_bhi, "supplement", "tables")
dims_tab <- read_csv(file.path(loc, "dimension_descriptions.csv"))

dimension_descriptions <- c(
  "score" = dims_tab$description[1],
  "status" = dims_tab$description[2],
  "future" = dims_tab$description[3],
  "trend" = dims_tab$description[4],
  "pressures" = dims_tab$description[5],
  "resilience" = dims_tab$description[6]
  )
