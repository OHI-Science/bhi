## load tidyverse libraries, and others, if not already loaded
sessionInfo() # check what packages are loaded/attached
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

## sourcing data.R because 'compare_yrs_data' function and others are written there
source(file.path(getwd(), "R", "data.R")) # assumes getwd() gives the folder where bhi-prep.Rproj is located
args(compare_yrs_data) # reminder of what arguements to include for the function and their defaults

## read in datasets to compare
data1_full <- readr::read_csv(file.path(
  "/Volumes/BHI_share/BHI 1.0", "Goals and Metadata",
  "9-Clean Waters", "contaminants",
  "downloaded from ICES", "ContaminantsBiota_PCB",
  "ContaminantsBiota20158241335bpsj33sqwrom5dcuk5cxoqvi.csv"
))
data2_full <- readr::read_csv(file.path(
    "/Volumes/BHI_share/BHI 2.0", "Goals", "CW", "CON",
    "ContaminantsBiota_PCBs", "ContaminantsBiota_PCBs.csv"
)) # parsing errors seem to all be from column specifications selected by default...


## look at some basic info about the data
names(data1_full)
names(data2_full) # check column names of each
setdiff(names(data1_full), names(data2_full))
setdiff(names(data2_full), names(data1_full)) # no differences

unique(data1_full$PARAM)
unique(data2_full$PARAM)
intersect(data1_full$PARAM, data2_full$PARAM)
setdiff(unique(data1_full$PARAM), unique(data2_full$PARAM)) # params in data1 (original) not in data2 (new), none
setdiff(unique(data2_full$PARAM), unique(data1_full$PARAM)) # but new data has also some additional ones

keys <- "tblParamID"
length(data1_full$tblParamID)
length(unique(data1_full$tblParamID)) # length and unique length are same, ie each row has distinct tblParamID


## define p to simplify filtering, change value in square brackets to test different params
intersect(data1_full$PARAM, data2_full$PARAM)
p <- intersect(data1_full$PARAM, data2_full$PARAM)[5]

## subset/filter data to compare
data1 <- filter(data1_full, PARAM == p) %>%
  select("DATE", "MYEAR", "Latitude", "Longitude", "PARAM", "PARGROUP",
         "Species", "MATRX", "tblParamID", "MUNIT", "Value")

data2 <- filter(data2_full, PARAM == p) %>%
  select("DATE", "MYEAR", "Latitude", "Longitude", "PARAM", "PARGROUP",
         "Species", "MATRX", "tblParamID", "MUNIT", "Value")

summary(data1)
summary(data2) # looking at summary of subsetted data, otherwise a lot of vars to look at

## join tables by 'tblParamID' for this data, plot is the first result returned by the function
comparison_plot <- compare_yrs_data(data1, data2, compare = "Value", keys = "tblParamID")
comparison_plot # axes' ranges if very different can indicate outliers, eg for param CB105

## go back and repeat for different params...
