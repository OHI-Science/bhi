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


getwd()

### using layers object created by for testing...
archive_filepath <- "/Users/eleanorecampbell/Desktop/GitHub/bhi-1.0-archive/baltic2015"
setwd(archive_filepath)
layers <- ohicore::Layers("layers.csv", "layers")

layers_csv_archived <- read_csv(file.path(archive_filepath, "layers.csv"))
layers_csv <- read_csv(file.path("/Users/eleanorecampbell/Desktop/GitHub/bhi/baltic/layers.csv"))

consider_layer <- layers$data$fis_bbmsy
head(consider_layer) # View(consider_layer)
if("year" %in% names(consider_layer)){
  consider_layer$year %>% unique() %>% sort() # years that we'll want to have in scenario_data_years.csv
}


### if layer is not in scenario_data_years csv, need to go update scenario_data_years.csv in baltic/conf
setwd("/Users/eleanorecampbell/Desktop/GitHub/bhi/baltic")
conf <- ohicore::Conf("conf")
source("../R/common.R")

current_conf_scen_data_yrs <- conf$scenario_data_years
current_conf_scen_data_yrs$layer_name %>% unique() # check which layers are in the current scenario_data_years.csv

update_scen_data_yrs <- scenario_data_include(current_conf_scen_data_yrs,
                                              2015:2018,
                                              new_lyrs = "",
                                              rename_lyrs = "") %>%
  mutate(data_year = ifelse(is.na(data_year), 2013, data_year)) # assign data year as 2013 for now...

layers_csv %>% filter(layer == "fis_landings") %>% select(layer, filename)
datayears_tab <- read_csv(file.path(archive_filepath, "layers", "fis_landings_bhi2015.csv"))
head(datayears_tab)
tmp <- datayears_tab %>%
  group_by(stock) %>%
  summarise(n = n(),
            min_yr = min(year),
            max_yr = max(year),
            miss_yrs = ifelse(max_yr-min_yr != n, setdiff(min_yr:max_yr, year), 0)) # tmp

datayrs <- 1994:2014
align_scenario_data_yrs <- scenario_data_align(update_scen_data_yrs, "fis_bbmsy", data_yrs = datayrs, 2015) %>%
  scenario_data_align("fis_ffmsy", data_yrs = datayrs, 2015) %>%
  scenario_data_align("fis_landings", data_yrs = datayrs, 2015) %>%
  filter(layer_name != "fis_meancatch")


write_csv(align_scenario_data_yrs, "./conf/scenario_data_years.csv")

### then re-create conf object based on the current bhi/conf folder, so scenario_data_year field updates
conf <- ohicore::Conf("conf")
ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)


### go back to goal functions subscript to continue running model line-by-line
### then, update layers.csv
bhi_layers <- read_csv("/Users/eleanorecampbell/Desktop/GitHub/bhi/baltic/layers.csv") # layers_csv
bhi_archive_layers <- read_csv("/Users/eleanorecampbell/Desktop/GitHub/bhi-1.0-archive/baltic2015/layers.csv") # layers_csv_archived

names(bhi_layers)
setdiff(names(bhi_archive_layers), names(bhi_layers))

update_layers <- bhi_archive_layers %>%
  filter(grepl(sprintf("^%s_.*", "fis"), layer)) %>%
  rbind(bhi_layers %>%
          filter(!grepl(sprintf("^%s_.*", "fis"), layer)) %>%
          mutate(clip_n_ship_disag = NA,
                 clip_n_ship_disag_description = NA,
                 rgns_in = NA))


# updated_layers_csv <- layers_csv_edit(tab_to_update, update_using_tab, prefix)
# updated_layers <- layers_csv_edit(bhi_layers, bhi_archive_layers, "fis")
# updated_layers_csv[2]
# 1 fis_bbmsy    fis_bbmsy_bhi2015.csv
# 2 fis_ffmsy    fis_ffmsy_bhi2015.csv
# 3 fis_landings fis_landings_bhi2015.csv

write_csv(updated_layers[[1]], "/Users/eleanorecampbell/Desktop/GitHub/bhi/baltic/layers.csv")

### go into layers folder to replace layers associated with the goal with ones from archived bhi
### copy goal subscript to functions.R main script

### full run-through with with layers and conf both from bhi repo
scenario_yrs <- 2015
conf <- ohicore::Conf("conf")
ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)
layers <- ohicore::Layers("layers.csv", "layers")

scorelist <- lapply(scenario_yrs, function(yr){
  layers$data$scenario_year <- yr
  scores_scenario_year <- ohicore::CalculateAll(conf, layers) %>%
    dplyr::mutate(year = yr)}) %>% dplyr::bind_rows()
