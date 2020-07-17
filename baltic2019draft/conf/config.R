
## region data layers and spatial config ----
## a list of possible id fields used in datalayers; most often will use region_id, but not always
layers_id_fields <- c("region_id", "rgn_id", "cntry_key", "fao_id", "fao_saup_id", "country_id", "saup_id", "fao_ohi_id")

## the official list of regions, and corresponding names
## note: this is a csv file in the layers folder, also saved for reference in the conf folder
layer_region_labels <- "rgns_complete"

## the official ocean areas of each region, used to weight each subregions contribution to the region score
## note: this is a csv file in the layers folder
layer_region_areas <- "rgns_complete"


## pressures & resilience matrices ----

## for goals with elements
## these describe how to weight contribution of each element in calculation of pressure and resilience dimensions
resilience_element <- NULL # for BHI no goals have elements currently
pressures_element <- NULL # for BHI no goals have elements currently


## constants for dimensions' relative importance ----
## used by ohicore::Calculate functions to calculate pressures/resilience/trend/likely future state

## relative importance of social vs ecological pressures
## (pressure = gamma * ecological + (1-gamma) * social)
pressures_gamma <- 0.5

## relative importance of social vs ecological resiliences
## (resilience = gamma * ecological + (1-gamma) * social)
resilience_gamma <- 0.5

## used to calculate likely future state
goal_discount <- 1.0

## relative importance of trend vs. pressure/resilience on likely future state
## if goal_beta = 0.67, trend is twice as important as pressure/resilience
goal_beta <- 0.67
default_trend <- 0


## descriptions and metadata ----

## extra descriptions not covered by goals.description or layers.description
index_description <- "The overall Index represents the weighted average of all goal scores."
dims_tab <- tbl(bhi_db_con, "dim_descriptions") %>% collect()
dimension_descriptions <- dims_tab$description
names(dimension_descriptions) <- dims_tab$dimension
