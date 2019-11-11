## calculate_scores.R

## This script calculates OHI scores with the `ohicore` package.
## - configure_toolbox.r ensures your files are properly configured for `ohicore`.
## - The `ohicore` function CalculateAll() calculates OHI scores.

## set working directory for all OHI calculations
setwd(here::here('baltic2015'))

##configure toolbox to check configuration
library(ohicore)   # run install_ohicore.R
library(tidyverse) # install.packages('tidyverse')
library(stringr)   # install.packages('stringr')
library(zoo)       # install.packages('zoo')

## load scenario configuration
conf = ohicore::Conf('conf')

## check that scenario layers files in the \layers folder match layers.csv registration. Layers files are not modified.
ohicore::CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

## load scenario layers for ohicore to access. Layers files are not modified.
layers = ohicore::Layers('layers.csv', 'layers')

## select corresponding data year to use for pressures and resilience
scenario_years <- 2014
layers$data$scenario_year <- scenario_years

## calculate scenario scores
scores <- CalculateAll(conf, layers)
write_csv(scores, 'scores.csv', na='')
