# Index of Common Global Function Parameters

The objective of this document is to provide a reference for commonly used parameters
Theoretically, or ideally, this will result in the same parameter names being used consistently throughout the BHI repositories
Further, the relevant roxygen documentation can be automatically filled in using the text of this document and the `R/management.R/` function

### A

**assess_year** -- assessment year i.e. the year for/during which the assessment is being conducted; set in `setup.R`


### B

**basins_or_rgns** -- one of 'subbasins' or 'regions' to indicate which spatial units should be represented


### C

**color_pal** -- a user-defined color palette, mapping to score values in the plot


### D

**data_yrs** -- the years of data for the specified layer, i.e. all the years in the layer data file

**dim** -- the dimension the object/plot should represent, typically 'score' but could be any one of the scores.csv 'dimension' column elements e.g. 'trend' or 'pressure'

**dims** -- 

**dir_assess** -- file path to the current assessment directory, an immediate subdirectory of the project root folder; set in `setup.R`
`assessment_path`

**dir_prep** -- file path to the prep folder within bhi-prep repo

**dir_R** -- file path to the directory of scripts containing functions


### E
### F

**functionsR_path** -- location of functions.R

**functionsR_text** -- list of funcitons.R lines e.g. read from file using scan function


### G

**goal_code** -- the two or three letter code indicating which goal/subgoal to create the plot for

**goals** -- a character vector of one or more goal_codes to use in visualizing or filtering data for analysis


### H
### I
### J
### K
### L

**layers_csv** -- layers.csv from the assessment directory, as a dataframe

**lyr_file** --  file path to where layer data file is located, including the name of the csv file itself to be incorporated into bhi layers object

**lyr_metadata** -- layers_metadata.csv from the assessment directory, as a dataframe

**lyr_metadata_path** -- file path to layers_metadata.csv that contains info corresponding to lyr_file; should have layer, name, description, units, and targets fields

**lyr_name** -- single character string with name of the layer (not the filename) to be associated with the layer data file

**lyr_obj** -- a layers object created with ohicore::Layers, for the repo assessment folder to reconfigure, not the archived version


### M

**mapping_data_sp** -- sf object associating scores with spatial polygons, i.e. having goal score and geometries information 


### N

**northarrow** -- boolean indicating whether or not to include a northarrow on map


### O
### P

**params_index** -- a character string giving the file path to this document

**plot_year** -- year by which to filter scores_csv or other input dataframe


### Q
### R

**rgn_scores** -- scores_csv dataframe i.e. output of ohicore::CalculateAll, but filtered to specific region or set of regions

**rgns** -- vector of bhi region ids


### S

**save_obj** -- either a directory in which to save the plot/object, or if TRUE the function will save object to a default location

**scalebar** -- boolean indicating whether or not to include a scalebar

**scenario_yrs** -- the years for which 'scenarios' are to be calculated; includes typically the assessment year and any back-calculated years

**scen_data_years** -- scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv

**scores_csv** -- scores dataframe with goal, dimension, region_id, year and score columns, e.g. output of ohicore::CalculateAll typically from calculate_scores.R

**scores_path** -- the directory where to save the scores.csv, by default in the assessment (dir_assess) folder

**script_name** -- name of a script typically from within repository `R` folder

**script_function** -- specific functions contained in a script typically from within repository `R` folder


### T

**temp_filepath** -- path to subfolder where ephemeral, non-critical files are temporarily put; defaults to `temp` subfolder of assessment directory


### U
### V

**version_year** -- the assessment year with a preceeding "v", specified as a string


### W
### X
### Y

**years** -- 


### Z
