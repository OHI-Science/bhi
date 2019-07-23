# Index of Common Global Function Parameters

The objective of this document is to provide a reference for commonly used parameters
Theoretically, or ideally, this will result in the same parameter names being used consistently throughout the BHI repositories
Further, the relevant roxygen documentation can be automatically filled in using the text of this document and the `R/management.R/` function

### A

**assess_year** -- assessment year i.e. the year for/during which the assessment is being conducted; set in `common.R`


### B
### C
### D

**data_yrs** -- the years of data for the specified layer, i.e. all the years in the layer data file

**dim** -- the dimension the object/plot should represent, typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'

**dir_assess** -- file path to the current assessment directory, an immediate subdirectory of the project root folder; set in `common.R`


### E
### F
### G

**goal_code** -- the two or three letter code indicating which goal/subgoal to create the plot for


### H
### I
### J
### K
### L

**layers_csv** -- layers.csv from the assessment directory, as a dataframe

**layers_metadata** -- layers_metadata.csv from the assessment directory, as a dataframe


### M

**mapping_data_sp** -- an sf object associating goal scores with spatial polygons/geometries


### N

**northarrow** -- boolean indicating whether or not to include a northarrow


### O
### P

**params_index** -- a character string giving the file path to this document

**plot_year** -- year by which to filter region scores_csv input dataframe


### Q
### R

**rgn_scores** -- scores_csv dataframe i.e. output of ohicore::CalculateAll, but typically filtered to specific region or set of regions


### S

**save** -- either a directory in which to save the plot/object, or if TRUE the function will save object to a default location

**scalebar** -- boolean indicating whether or not to include a scalebar

**scores_csv** -- scores dataframe with goal, dimension, region_id, year and score columns, e.g. output of ohicore::CalculateAll typically from calculate_scores.R

**scen_data_years** -- scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv

**scen_yrs** -- the scenario years to be included in the updated scenario_data_years.csv


### T

**temp_filepath** -- path to subfolder where ephemeral, non-critical files are temporarily put; defaults to `temp` subfolder of assessment directory


### U
### V

**version_year** -- the assessment year with a preceeding "v", specified as a string


### W
### X
### Y
### Z
