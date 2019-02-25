### not a piece of the repo for assessments!
### can delete after reconfiguring bhi from single to multi-year framework...
### need some functions in common.R

### APPROACH OVERVIEW

### PART 1
### use layers object based on bhi-archive/layers and conf based on bhi/conf current
### update scenario_data_years if/as needed, updating conf after the changes are complete
### Finish stepping through goal model subscript
### PART 2
### update layers.csv and replace layers in bhi/layers repo with bhi-archive versions
### use layers object and conf objects both based on bhi/conf current
### do calculateAll to make sure will work...



## SET UP / PREAMBLE
base_path <- "/Users/eleanorecampbell/Desktop/GitHub/" # set based on own local file organization...
source(file.path(base_path, "bhi/R/common.R"))
source(file.path(base_path, "bhi/R/checking.R"))

### layers from archive
archive_filepath <- file.path(base_path, "bhi-1.0-archive/baltic2015")
setwd(archive_filepath)
layers <- ohicore::Layers("layers.csv", "layers") # layers object created from archive
names(layers$data)

bhi_layers <- read_csv(file.path(base_path, "bhi/baltic/layers.csv"))
bhi_archive_layers <- read_csv(file.path(archive_filepath, "layers.csv"))

### conf from current bhi repo
setwd(file.path(base_path, "bhi/baltic"))
conf <- ohicore::Conf("conf")
current_conf_scen_data_yrs <- conf$scenario_data_years
current_conf_scen_data_yrs$layer_name %>% unique()

############################################################################
######## REPEAT BELOW STEPS AS NECESSARY TO INCORPORATE ALL NECESSARY LAYERS

names(layers$data) %>% grep(pattern = "fp_.*", value = TRUE) # can subset, eg here by 'fp' prefix


## CHOOSE LAYER TO LOOK AT FROM ARCHIVED LAYERS
consider_layer <- layers$data$fp_wildcaught_weight
head(consider_layer) # View(consider_layer)
consider_lyr_nm <- "fp_wildcaught_weight"

consider_goal_prefix <- "fp"

if("year" %in% names(consider_layer)){
  consider_layer$year %>% unique() %>% sort() # years we can have in scenario_data_years.csv as data years
} else {"no year column"}

## IF NO YEAR COLUMN GO TO LAYER PREP AND ADD YEARS
## adding years esp for trend into data prep process will be more carefully reviewed when doing actual data prep for v2019...
names(bhi_layers)

bhi_archive_layers %>%
  filter(layer == consider_lyr_nm) %>%
  select(layer, filename)

bhi_layers %>%
  filter(layer == consider_lyr_nm) %>%
  select(layer, filename)

## REPLACE TEMPLATE W THE ARCHIVED LAYERS ASSOCIATE W GOAL MODEL BEING TRANSFERRED OVER

## view archived layer, if needs year add year and save in bhi/layers, otherwise just copy over
fn <- str_match(bhi_archive_layers$filename, paste0(consider_lyr_nm, ".*.csv")) %>%
  lapply(function(x){x[!is.na(x)]}) %>%
  unlist()

layer_archive_version <- read_csv(file.path(archive_filepath, "layers", fn))
head(layer_archive_version)
yr_col_added <- layer_archive_version %>%
  mutate(year = 2014) # temporary for setting up bhi repo!
## will review how years are added when doing actual data prep!!!

## save version with year column to the bhi repo
write_csv(yr_col_added, file.path(base_path, "bhi/baltic/layers", fn))

datayears_tab <- read_csv(file.path(base_path, "bhi/baltic/layers", fn))
head(datayears_tab)

## UPDATE SCENARIO_DATA_YEARS FOR SELECTED LAYER
## if layer is not in scenario_data_years csv, need to go update scenario_data_years.csv in baltic/conf
current_conf_scen_data_yrs %>%
  filter(layer_name == consider_lyr_nm) # if 0 rows, need to add with scenario_data_include()

View(current_conf_scen_data_yrs)

tmp <- datayears_tab %>%
  group_by(rgn_id) %>%
  summarise(n = n(),
            min_yr = min(year),
            max_yr = max(year)) # View(tmp)
datayrs <- 2014

update_scen_data_yrs <- scenario_data_include(current_conf_scen_data_yrs, # %>%
                                                # filter(is.na(str_match(layer_name, "fp_"))),
                                              2015:2018,
                                              new_lyrs = consider_lyr_nm,
                                              rename_lyrs = "") %>%
  mutate(data_year = ifelse(is.na(data_year), 2014, data_year)) # assign data year as 2014 for now...
write_csv(update_scen_data_yrs, "./conf/scenario_data_years.csv")

# align_scenario_data_yrs <- scenario_data_align(current_conf_scen_data_yrs,
#                                                "mar_sustainability_score",
#                                                data_yrs = 2012,
#                                                2007:2015)
# write_csv(align_scenario_data_yrs, "./conf/scenario_data_years.csv")


######## REPEAT ABOVE STEPS AS NECESSARY TO INCORPORATE ALL NECESSARY LAYERS
############################################################################
############################################################################


## UPDATE LAYERS.CSV AS NEEDED
## layers_csv_edit function defined in common.R, can run line-by-line there
updated_layers <- layers_csv_edit(bhi_layers,
                                  bhi_archive_layers,
                                  consider_goal_prefix)
updated_layers[[2]] # View(updated_layers[[1]])
write_csv(updated_layers[[1]], file.path(base_path, "bhi/baltic/layers.csv"))


### then re-create conf object based on the current bhi/conf folder, so scenario_data_year field updates
conf <- ohicore::Conf("conf")
ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)
scenario_yrs <- 2015

### go back to goal functions subscript to continue running model line-by-line
### copy goal subscript to functions.R main script

############################################################################

## FULL RUN-THROUGH AFTER COPYING OVER GOAL MODEL
## full run-through with with layers and conf both from bhi repo
layers <- ohicore::Layers("layers.csv", "layers")

scorelist <- lapply(scenario_yrs, function(yr){
  layers$data$scenario_year <- yr # layers$data$scenario_year <- scenario_yrs[1]
  scores_scenario_year <- ohicore::CalculateAll(conf, layers) %>%
    dplyr::mutate(year = yr)}) %>% dplyr::bind_rows()

v2015scores <- read_csv(file.path(archive_filepath, "scores.csv")) %>% mutate(year = 2015)
compare_scores(scorelist, 2015, v2015scores, 2015)
