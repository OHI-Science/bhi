## Libraries
library(dplyr)
library(readr)
library(stringr)
library(here)

## Directories
dir_bhi <- here::here()
dir_sp <- file.path(dir_bhi, "spatial")
dir_R <- file.path(dir_bhi, "R")
dir_baltic <- file.path(dir_bhi, "baltic")

dir_B <- c("Darwin" = "/Volumes/BHI_share", # "Windows" = ?
           "Linux" = "/home/shares/ohi")[[ Sys.info()[["sysname"]] ]]

if (Sys.info()[["sysname"]] != "Linux" & !file.exists(dir_B)){ # warning if BHI internal directory doesn't exist
  warning(paste("The BHI directory dir_B set in R/common.R does not exist.",
                 sprintf("Do you need to mount the BHI server: %s?", dir_B)))
}
dir_B <- file.path(dir_B, "BHI 2.0") # CHANGE MAIN AUX BHI DIRECTORY HERE!

setwd(dir_baltic) # CHANGE DEFAULT WORKING DIRECTORY (WILL THIS BREAK STUFF?!)
