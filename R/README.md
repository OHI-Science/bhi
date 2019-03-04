# `R`

<br/>

### `checking.R`

**compare_scores** <br/> 
compare scores between two scores.csv tables for two specified years <br/> 
@param scores1 first scores table (dataframe) to use <br/> 
@param year1 year of data within first scores table to use in comparison <br/> 
@param scores2 second scores table <br/> 
@param year2 year within second scores table <br/> 
@param dim a string or vector of strings specifying dimension(s) to investigate <br/> 
@param goal a string or vector of strings specifying goal(s) to investigate <br/> 
@return returns a list with two objects: 1. a faceted plot and 2. a summary table <br/> 
<br/>

### `common.R`

**calculate_scores** <br/> 
calculate BHI scores for each scenario year and save to a single csv file <br/> 
@param assessment_path the bhi repository 'baltic' subfolder, wherever that is on your local computer <br/> 
@param scenario_yrs which years scores are to be calculated for <br/> 
@param scores_path the directory where to save the scores.csv, by default in the 'baltic' (assessment) folder <br/> 
@return <br/> 
<br/>

### `database.R`

<br/>

### `reconfiguring.R`

<br/>

### `spatial.R`

**create_rgn_lookup** <br/> 
create region lookup table <br/> 
@param dir_bhi <br/> 
@param layers_object <br/> 
@param conf_object <br/> 
@return <br/> 
<br/>

### `testing.R`

**configure_functions** <br/> 
configure functions.R for testing <br/> 
compiles files listed in a local 'test_functions.csv' into the single \code{functions.R} that ohicore needs <br/> 
@param assessment_path filepath specifying the assessment main folder containing 'conf' and 'testing' folders <br/> 
@return revises functions.R at existing location in 'conf' folder; no output <br/> 
<br/>
**configure_layers** <br/> 
configure layers for testing <br/> 
copies the versions of layers we want to use for testing (as specified in the 'alt_layers_full_table.csv') into `bhi-prep/prep/layers` <br/> 
the `copy_layers_for_assessment` function in `bhi/R/common.R` is used afterwards to copy layers to the layers folder in the assessment repo (bhi/baltic) <br/> 
compares the specified layers to the layers named in \code{functions.R} to confirm all layers needed given the models, will be copied to 'layers' <br/> 
automatically writes the name of copied file into 'filename' column of 'layers.csv' <br/> 
@param assessment_path filepath specifying the assessment main folder containing 'conf' and 'testing' folders <br/> 
@param prep_path bhi-prep filepath specifying the main folder containing goal/pressure/resilience subfolders <br/> 
@return a table comparing layers required by \code{functions.R} to those specified in alt_layers_full_table.csv; revises 'layers.csv' and contents of 'layers' folder <br/> 
<br/>

### `visualization.R`

<br/>
