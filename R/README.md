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

**scenario_data_include** <br/> 
update scenario years and/or layers' names in scenario_data_years table <br/> 
@param scen_data_years scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv <br/> 
@param scen_yrs the scenario years to be included in the updated scenario_data_years.csv <br/> 
@param new_lyrs names (character vector) of any layers not yet in table, for which to create scenario-data year information <br/> 
@param rename_lyrs a list with two elements: 'layer_name' vector of layers to be renamed and 'to' vector of new names with placement (index) matching current layer name <br/> 
@return <br/> 
<br/>
**scenario_data_align** <br/> 
aligning scenario and data years for a given layer <br/> 
maps years within the layer dataset to a "scenario year" for a given layer <br/> 
because of time lags in or aperiodic data collection... <br/> 
@param scen_data_years scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv <br/> 
@param layer_name name of the layer for which to align scenario and data years <br/> 
@param data_yrs the years of data for the specified layer, i.e. all the years in the layer data file <br/> 
@param scen_yrs the scenario years to be included in the updated scenario_data_years.csv <br/> 
@return <br/> 
<br/>
**copy_layers_for_assessment** <br/> 
copy layers from 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder <br/> 
@param assessment_path file path to assessment folder within bhi repo <br/> 
@param repo_location url pointing to the bhi repo on github <br/> 
@return <br/> 
<br/>
**update_alt_layers_tab** <br/> 
update `alt_layers_full_table.csv` to include additional versions of layers created <br/> 
@param assessment_path file path to assessment folder within bhi repo <br/> 
@param prep_path file path to the prep folder within bhi-prep repo <br/> 
@param assess_year assessment year i.e. the year for/during which the assessment is being conducted <br/> 
@return <br/> 
<br/>
**layers_edit** <br/> 
update rows in layers.csv for a given layer <br/> 
registers a specified file is to the given layer, after checking registration against layers object created by ohicore::Layers <br/> 
written originally mostly for setting up bhi multiyear assessments repo from the archived repo... <br/> 
@param layers_object a layers object created with ohicore::Layers for the bhi repo assessment folder we aim to reconfigure <br/> 
@param lyr_file file path to where layer data file is located, including the name of the csv file itself to be incorporated into bhi layers object <br/> 
@param lyr_name single character string with name of the layer (not the filename) to be associated with the <br/> 
@param assessment_path file path to assessment folder within bhi repo <br/> 
@param update_with a dataframe or tibble with at least one row, with the information to be added to layers.csv for the layer <br/> 
@param write if TRUE then the function will automatically overwrite layers.csv and write lyr_file to the 'layers' folder <br/> 
@return <br/> 
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

### `management.R`

**bhiRfun_readme** <br/> 
compile readme information associated with functions defined in a script <br/> 
written to generate readme content for functions in bhi/R scripts, but could be used elsewhere... <br/> 
@param bhiR_dir file path to the directory containing the script of interest <br/> 
@param script_name the name of the script with functions you want readme documentation for <br/> 
@return text for readme content is returned in the console, but output is also configured as a character vector <br/> 
<br/>
**readme_content** <br/> 
function for creating metadata/readme content <br/> 
@param folder_filepath file path to folder where README will be located and which contains objects to document <br/> 
@param file name of the file to generate basic content for <br/> 
@param file_type <br/> 
@return <br/> 
<br/>
**readme_outline** <br/> 
generate basic readme outline <br/> 
look within a folder and create structure dependent on content and file tree <br/> 
if has subfolders... <br/> 
result not to be the end all be all, just a starting point or rough outline to start from <br/> 
will still have to actually open and manually edit some fields, but readme_content function will help with that <br/> 
describe_objects could be: file, table, folder, function, script <br/> 
could use `sink` function to write readme outline output directly to a specified readme file <br/> 
@param folder_filepath file path to folder where README will be located and which contains objects to document <br/> 
@param type_objects character string with type of thing to document in the readme: folders, functions, files, tables, or scripts <br/> 
@return text for readme outline is printed to the console, and can be copied from there or sunk to a file <br/> 
<br/>
**readme_to_df** <br/> 
from readme markdown file to dataframe <br/> 
@param folder_filepath file path to folder where README will be located and which contains objects to document <br/> 
@param write a boolean variable indicating whether to write the dataframe to csv file in the folder_filepath location <br/> 
@return dataframe created from a readme markdown file with structure outlined by `readme_outline` function above <br/> 
<br/>
**readme_status** <br/> 
check up-to-date-ness status of readme <br/> 
check whether readme actually reflects the current state of the directory and files it is written for <br/> 
compare lines within readme and outline that would be generated via the `readme_outline` function above <br/> 
the comparison is done via the `readme_to_csv` function, also above... <br/> 
@param folder_filepath file path to folder where README will be located and which contains objects to document <br/> 
@param type_objects character string with type of thing to document in the readme: folders, functions, files, tables, or scripts <br/> 
@param temp_filepath file path to subfolder of assessment folder called 'temp' where ephemeral, non-critical files are temporarily put <br/> 
@return no returned value, just printed messages about status of readme in comparison to expected structure <br/> 
<br/>

### `visualization.R`

<br/>
