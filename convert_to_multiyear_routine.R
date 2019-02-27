### not a piece of the repo for assessments! can delete after reconfiguring bhi from single to multi-year framework...

### APPROACH OVERVIEW
### create hybrid pressure + resilience matrices, between current and one we want to end up with
### update (1) scenario_data_years if/as needed (2) layers.csv
### replace relevant layers in bhi/layers repo with bhi-archive versions, with years appended
### update conf after the changes are complete and finish stepping through goal model script
### use layers object and conf objects both based on bhi/conf current in full run through


### SET UP OR PREAMBLE
base_path <- "/Users/eleanorecampbell/Desktop/GitHub" # set based on own local file organization...
source(file.path(base_path, "bhi/R/common.R"))
source(file.path(base_path, "bhi/R/checking.R"))
source(file.path(base_path, "bhi/R/reconfiguring.R"))

### layers from archive
archive_filepath <- file.path(base_path, "bhi-1.0-archive/baltic2015")
setwd(archive_filepath)
layers <- ohicore::Layers("layers.csv", "layers") # layers object created from archive
bhi_archive_layers <- read_csv(file.path(archive_filepath, "layers.csv"))

setwd(file.path(base_path, "bhi/baltic"))
consider_goal_prefix <- "fis"

### INTERM PRESSURE AND RESILIENCE MATRICES
### want to replace pressure and resilience weightings of layers for goal being transferred
### need all pressure layers in pressure_matrix.csv to be in layers.csv and layers folder

### pressure and resilience matrices
goal_press <- read_csv(file.path(archive_filepath, "conf/pressures_matrix.csv")) %>%
  filter(goal == str_to_upper(consider_goal_prefix)) %>%
  select(-element, -element_name) # View(goal_pressure)

goal_res <- read_csv(file.path(archive_filepath, "conf/resilience_matrix.csv")) %>%
  filter(goal == str_to_upper(consider_goal_prefix)) %>%
  select(-element) # View(goal_resilience)

### pressure and resilience layers to copy over and add to layers.csv
bhi_layers_most_recent <- read_csv(file.path(base_path, "bhi/baltic/layers.csv"))

press_res_lyrs <- c(
  goal_press %>%
    select_if(!is.na(.)) %>%
    select(-goal) %>%
    names(),
  goal_res %>%
    select_if(!is.na(.)) %>%
    select(-goal) %>%
    names())

### create the 'interm' pressure + resilience matrices
int_pressure_mat <- read_csv(file.path(base_path, "bhi/baltic/conf/pressures_matrix.csv")) %>%
  filter(goal != str_to_upper(consider_goal_prefix)) %>%
  full_join(goal_press, by = c("goal", intersect(names(goal_press), colnames(.)))) %>%
  select_if(function(x) {!all(is.na(x))})

int_resilience_mat <- read_csv(
  file.path(base_path, "bhi/baltic/conf/resilience_matrix.csv"),
  col_types = cols(.default = "c")) %>%
  filter(goal != str_to_upper(consider_goal_prefix)) %>%
  full_join(goal_res, by = c("goal", intersect(names(goal_res), colnames(.)))) %>%
  select_if(function(x) {!all(is.na(x))})

############################################################################
############################################################################
############################################################################

############################################################################
######## REPEAT BELOW STEPS AS NECESSARY TO INCORPORATE ALL NECESSARY LAYERS

bhi_layers_most_recent <- read_csv(file.path(base_path, "bhi/baltic/layers.csv"))
add_press_res_lyrs <- setdiff(
  press_res_lyrs,
  bhi_layers_most_recent$layer)

### names of data layers for goal status, from layers object created from archive
names(layers$data) %>%
  grep(pattern = paste0(consider_goal_prefix, "_.*"),
       value = TRUE) %>%
  c(add_press_res_lyrs) # include pressure and resilience layers

### CHOOSE LAYER
consider_layer <- layers$data$res_reg_mspd # View(consider_layer)
consider_lyr_nm <- "res_reg_mspd"
if("year" %in% names(consider_layer)){
  consider_layer$year %>% unique() %>% sort() # years we can have in scenario_data_years.csv as data years
} else {"no year column"}

### REPLACE TEMPLATE W THE ARCHIVED LAYERS ASSOCIATE W GOAL MODEL BEING TRANSFERRED OVER
### check filename to use, and whether it already exists in bhi repo
### view archived layer, if needs year add year and save in bhi/layers, otherwise just copy over
fn <- str_match(bhi_archive_layers$filename, paste0(consider_lyr_nm, ".*.csv")) %>%
  lapply(function(x){x[!is.na(x)]}) %>%
  unlist()

bhi_archive_layers %>%
  filter(layer == consider_lyr_nm) %>%
  select(layer, filename)

bhi_layers_most_recent %>%
  filter(layer == consider_lyr_nm) %>%
  select(layer, filename)

### IF NEED TO ADD BECAUSE LACKS YEAR COLUMN...
### temporarily adding years into data prep process here
### especially for trend, will be more carefully reviewed when doing actual data prep for v2019
### for purposes of transitioning repo to multi-year framework before new data prep, add dummy year col
### will review how years are actually included when doing actual data prep for new assessment!!!!!
layer_archive_version <- read_csv(file.path(archive_filepath, "layers", fn))
head(layer_archive_version)
yr_col_added <- layer_archive_version %>%
  mutate(year = 2014) # temporary for setting up bhi repo!
head(yr_col_added) # head(read_csv(file.path(base_path, "bhi/baltic/layers", fn)))
write_csv(yr_col_added, file.path(base_path, "bhi/baltic/layers", fn))

### UPDATING SCENARIO_DATA_YEARS FOR SELECTED LAYER
### each time a layer is transitioned recreate scenario_data_years by recreating conf
conf <- ohicore::Conf("conf")
current_conf_scen_data_yrs <- conf$scenario_data_years
current_conf_scen_data_yrs %>% filter(layer_name == consider_lyr_nm)

### layers to remove from or add to scenario_data_years table
r <- ifelse(str_sub(consider_lyr_nm, 1,
                    str_length(consider_goal_prefix)) == consider_goal_prefix,
            paste0(consider_goal_prefix, "_.*"),
            consider_lyr_nm)

rm_lyrs <- setdiff(
  current_conf_scen_data_yrs$layer_name %>%
    grep(pattern = r, value = TRUE) %>% unique(),
  names(layers$data) %>%
    grep(pattern = r, value = TRUE))

incl_new_lyrs <- setdiff(
  names(layers$data) %>%
    grep(pattern = r,
         value = TRUE),
  current_conf_scen_data_yrs$layer_name %>%
    grep(pattern = r,
         value = TRUE) %>% unique())

### use scenario_data_align function if already have data years included in layer!!!
# see_datayears_tab <- read_csv(file.path(base_path, "bhi/baltic/layers", fn))
# head(see_datayears_tab)
# tmp <- see_datayears_tab %>%
#   group_by(rgn_id) %>%
#   summarise(n = n(),
#             min_yr = min(year),
#             max_yr = max(year)) # View(tmp)

### use scenario_data_include function to add the layer into scenario_data_years.csv
if(length(rm_lyrs) != 0){
  base_scen_data_yrs <- current_conf_scen_data_yrs %>%
    filter(is.na(str_match(layer_name, r)))
} else {base_scen_data_yrs <- current_conf_scen_data_yrs}
update_scen_data_yrs <- scenario_data_include(
  base_scen_data_yrs,
  2015:2018,
  new_lyrs = incl_new_lyrs) %>%
  mutate(data_year = ifelse(is.na(data_year), 2014, data_year)) # assign 2014 just for now

update_scen_data_yrs %>%
  filter(!is.na(str_match(
    layer_name, r))) # View(update_scen_data_yrs)
tmp <- update_scen_data_yrs

write_csv(update_scen_data_yrs, "./conf/scenario_data_years.csv")


### UPDATE LAYERS.CSV AS NEEDED
### layers_csv_edit function defined in common.R, can run line-by-line there
### if gsub("\\.\\*", "", r) is the goal prefix, run once, otherwise per layer
updated_layers <- layers_csv_edit(
  bhi_layers_most_recent,
  bhi_archive_layers,
  gsub("\\.\\*", "", r))
updated_layers[[2]] # View(updated_layers[[1]])

track_layers_added <- read_csv(
  file.path(base_path, "bhi/track_layers_added.csv")) %>%
  rbind(updated_layers[[2]] %>%
          mutate(day_added = as.Date("2019-02-18")) %>%
          mutate(dummy_data_yr = TRUE) %>% # FALSE if had real data years already
          mutate(added_w_goal = str_to_upper(consider_goal_prefix))) # View(track_layers_added)

tail(track_layers_added)
length(track_layers_added$layer %>% unique())
length(track_layers_added$layer)

write_csv(updated_layers[[1]], file.path(base_path, "bhi/baltic/layers.csv"))
write_csv(track_layers_added, file.path(base_path, "bhi/track_layers_added.csv"))

######## REPEAT ABOVE STEPS AS NECESSARY TO INCORPORATE ALL NECESSARY LAYERS
############################################################################

### overwrite pressure and resilience matrices before moving on to the next goal or subgoal
write_csv(int_pressure_mat, file.path(base_path, "bhi/baltic/conf/pressures_matrix.csv"))
write_csv(int_resilience_mat, file.path(base_path, "bhi/baltic/conf/resilience_matrix.csv"))

# res_cat_add <- read_csv(file.path(archive_filepath, "conf/resilience_categories.csv"))
# res_cat <- read_csv(file.path(base_path, "bhi/baltic/conf/resilience_categories.csv"))
# res_cat_overlap <- intersect(res_cat_add$layer, res_cat$layer)
# res_cat_update <- res_cat %>%
#   filter(!layer %in% res_cat_overlap) %>%
#   rbind(res_cat_add)
# write_csv(res_cat_update, file.path(base_path, "bhi/baltic/conf/resilience_categories.csv"))


### go back to goal functions subscript to continue running model line-by-line
### copy goal subscript to functions.R main script

############################################################################
### FULL RUN-THROUGH AFTER COPYING OVER GOAL MODEL
### full run-through with with layers and conf both from bhi repo
### then re-create conf object based on the current bhi/conf folder, so scenario_data_year field updates
layers <- ohicore::Layers("layers.csv", "layers")
conf <- ohicore::Conf("conf")
ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)
scenario_yrs <- 2015

scorelist <- lapply(scenario_yrs, function(yr){
  layers$data$scenario_year <- yr # layers$data$scenario_year <- scenario_yrs[1]
  scores_scenario_year <- ohicore::CalculateAll(conf, layers) %>%
    dplyr::mutate(year = yr)}) %>% dplyr::bind_rows()

v2015scores <- read_csv(file.path(archive_filepath, "scores.csv")) %>% mutate(year = 2015)
tmp <- compare_scores(scorelist, 2015, v2015scores, 2015, goal = c("AO", "FIS", "MAR", "FP")) ## what's causing differences???
ggplotly(tmp[[1]])
tmp[[2]]
