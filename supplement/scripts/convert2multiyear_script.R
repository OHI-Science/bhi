## SET UP / PREAMBLE ----

base_path <- "/Users/eleanorecampbell/Desktop/GitHub"

source(file.path(base_path, "bhi", "R", "common.R"))
source(file.path(base_path, "bhi", "R", "checking.R"))
source(file.path(base_path, "bhi", "R", "reconfiguring.R"))
library(ohicore)

source(file.path(base_path, "bhi", "R", "convert_repo.R"))
archive_filepath <- file.path(base_path, "bhi-1.0-archive", "baltic2015") # archive version
new_repo <- file.path(base_path, "bhi", "baltic") # define path and set directory to new repo
temp_dir <- file.path(base_path, "bhi", "baltic", "temp") # temp directory to sink and save error-checking tables to



## CONVERSION & RESULTS ----
result <- convert_repo(new_repo = new_repo, archive_filepath = archive_filepath,
                       original_funs_dir = file.path(new_repo, "testing", "alt_functions", "v2015"),
                       new_funs_dir = file.path(new_repo, "testing", "alt_functions", "multiyear_v2015"),
                       scenario_yrs = 2015:2018, dummy_data_yr = 2014)


## FULL RUN-THROUGH AFTER COPYING OVER GOAL MODEL ----

## full run-through with with layers and conf both from bhi repo
## then re-create conf object based on the current bhi/conf folder, so scenario_data_year field updates

layers <- ohicore::Layers("layers.csv", "layers")
conf <- ohicore::Conf("conf")
ohicore::CheckLayers("layers.csv", "layers", flds_id=conf$config$layers_id_fields)
scenario_yrs <- 2015

scorelist <- lapply(scenario_yrs, function(yr){
  layers$data$scenario_year <- yr # layers$data$scenario_year <- scenario_yrs[1]
  scores_scenario_year <- ohicore::CalculateAll(conf, layers) %>%
    dplyr::mutate(year = yr)}) %>% dplyr::bind_rows()

v2015scores <- read_csv(file.path(archive_filepath, "scores.csv")) %>% mutate(year = 2015)
tmp <- compare_scores(scorelist, 2015,
                      v2015scores, 2015,
                      dim = c("status", "pressures", "resilience"),
                      goal = c("FP", "FIS", "MAR")) ## what's causing differences???
ggplotly(tmp[[1]])
tmp[[2]]
