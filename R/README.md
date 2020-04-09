# `R`

<br/>

### `calculating.R`

**calculate_scores** <br/> 
calculate BHI scores for each scenario year and save to a single csv file <br/> 
@param dir_assess filepath to the current assessment directory, an immediate subdirectory of the project root folder; set in `setup.R` <br/> 
@param scenario_yrs which years scores are to be calculated for <br/> 
@param scores_path the directory where to save the scores.csv, by default in the assessment (dir_assess) folder <br/> 
@return OHI scores <br/> 
<br/>
**semicolon_to_comma** <br/> 
revise csv file from semicolon to comma delimiters <br/> 
@param csv_filepath the filepath to the csv file to edit, including the filename <br/> 
@param remove_na_cols boolean indicating whether to remove all columns with only NA values <br/> 
@param overwrite boolean indicating whether to immediately overwrite the csv file with updated version <br/> 
@return if overwritten, returns head of updated file read with read_csv, else original table with NA cols removed <br/> 
<br/>

### `checking.R`

**compare_scores** <br/> 
compare scores between two scores.csv tables for two specified years <br/> 
@param scores1 first scores table (dataframe) to use in comparison <br/> 
@param year1 year of data within first scores table to use <br/> 
@param scores2 second scores table <br/> 
@param year2 year within second scores table <br/> 
@param dim a string or vector of strings specifying dimension(s) to investigate <br/> 
@param goals a string or vector of strings specifying goal(s) to investigate <br/> 
@return returns a list with two objects: 1. a faceted plot and 2. a summary table <br/> 
<br/>
**change_plot** <br/> 
change plot <br/> 
This function compares BHI scores from the current analysis and a previous commit <br/> 
Written originally by Melanie Frazier for the ohi-global assessment <br/> 
@param repo repository name, e.g. 'bhi' or  'bhi-1.0-archive' <br/> 
@param assessmt name of assessment subfolder that contains the 'scores.csv' file of interest <br/> 
@param commit 7 digit sha number identifying the commit. Otherwise, it is compared to the previous commit <br/> 
@return list of two objects: first is the interactive html image, second is a dataframe recording the differences <br/> 
<br/>

### `common.R`

**funsR_goals_list** <br/> 
list functions.R goals <br/> 
@param functionsR_path location of functions.R to extract goal from <br/> 
@param functionsR_text list of funcitons.R lines read eg using scan (if functionsR_path is not provided) <br/> 
@return goal codes character vector for goals having functions defined in functions.R <br/> 
<br/>
**goal_layers** <br/> 
extract from functions.R layers associated with a specific goal function <br/> 
@param functionsR_path location of functions.R to extract goal from <br/> 
@param goal_code the two or three letter code indicating the goal or subgoal <br/> 
@return character vector naming layers <br/> 
<br/>
**bhiprep_github_layers** <br/> 
prepared layers in bhi-prep github <br/> 
@param github_api_url github api url from which to extract list of layers at location prep/layers/filename <br/> 
@return dataframe with names of the prepared layers within bhi-prep layers folder on github <br/> 
<br/>
**compare_tables** <br/> 
compare columns and/or rows between two tables <br/> 
@param tab1 the table to compare to (what we hope table 2 looks like) <br/> 
@param tab2 the table to -within some specified columns- check for missing/extra rows with respect to some 'key' variable <br/> 
@param key_row_var the variable for groupings of interest e.g. by which you would join or maybe gather the data <br/> 
@param check_cols the columns to look for differences within; essentially taking tab1 %>% select(check_cols) to compare tab2 against <br/> 
@param check_for_nas character vector of column names to check for NAs <br/> 
@return a boolean indicating if differences were found, list of checks results including scan for NAs, <br/> 
<br/>

### `reconfiguring.R`

**configure_functions** <br/> 
configure functions.R for testing <br/> 
compiles files listed in a local 'test_functions.csv' into the single \code{functions.R} that ohicore needs <br/> 
@param dir_assess filepath specifying the assessment main folder containing 'conf' and 'testing' folders <br/> 
@param test_funs_list a list of filepaths from (not including) 'testing/alt_functions' to the functions we want to use in analysis <br/> 
@return revises functions.R at existing location in 'conf' folder; no direct output <br/> 
<br/>
**copy_lyrs_from_bhiprep** <br/> 
copy prepared layers from bhi-prep to bhi repo <br/> 
copies layers from prep 'bhi-prep/prep/layers' to the assessment 'bhi/baltic/layers' folder, from github repo url <br/> 
@param dir_assess file path to assessment folder within bhi repo <br/> 
@param copy_layers <br/> 
@param repo_loc url pointing to the bhi repo on github <br/> 
@return <br/> 
<br/>
**scenario_data_include** <br/> 
update scenario years and/or layers' names in scenario_data_years table <br/> 
@param scen_data_years scenario_data_years dataframe or tibble; the object read in from scenario_data_years.csv <br/> 
@param scenario_yrs the scenario years to be included in the updated scenario_data_years.csv <br/> 
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
@param scenario_yrs the scenario years to be included in the updated scenario_data_years.csv <br/> 
@param approach <br/> 
@return <br/> 
<br/>
**make_config_files** <br/> 
make assessment configuration files <br/> 
@param bhi_db_con connection to bhi config database <br/> 
@param dir_assess file path to assessment folder within bhi repo <br/> 
@return create configuration tables from database versions and save to conf folder in specified assessment directory <br/> 
<br/>
**layerscsv_edit** <br/> 
update rows in layers.csv for a given layer <br/> 
registers a specified file to the given layer, <br/> 
after checking registration against layers object created by ohicore::Layers <br/> 
written originally/mostly for setting up bhi multiyear assessments repo from the archived repo... <br/> 
@param layers_obj a layers object created with ohicore::Layers, <br/> 
for the repo assessment folder to reconfigure, not the archived version <br/> 
@param lyr_file file path to where layer data file is located, <br/> 
including the name of the csv file itself to be incorporated into bhi layers object <br/> 
@param lyr_name single character string with name of the layer (not the filename) to be associated with the layer data file <br/> 
@param dir_assess file path to assessment folder within bhi repo <br/> 
@param lyr_meta file path to lyr_metadata.csv that contains info corresponding to lyr_file; <br/> 
should have layer, name, description, units, and targets fields <br/> 
@param update_w a dataframe or tibble with at least one row, <br/> 
with the information to be added to layers.csv for the layer <br/> 
@param write if TRUE then the function will automatically overwrite layers.csv and write lyr_file to the 'layers' folder <br/> 
@return updated layers.csv table is first output, the second output is the contents of the layer file specified by lyr_file arg <br/> 
<br/>

### `management.R`

**funR_readme** <br/> 
compile readme information associated with functions defined in a script <br/> 
written to generate readme content for functions in bhi/R scripts, but could be used elsewhere... <br/> 
@param dir_R file path to the directory containing the script of interest <br/> 
@param script_name the name of the script with functions you want readme documentation for <br/> 
@return text for readme content is returned in the console, but output is also configured as a character vector <br/> 
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
**readme_to_dataframe** <br/> 
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
**update_goalsRmd_links** <br/> 
update links to bhi-prep docs in goals.Rmd <br/> 
@param dir project root of bhi-prep <br/> 
@param version_year the assessment year with a preceeding "v", specified as a string <br/> 
@return no immediate output; effect of the function is updated links to prep files in supplement/web/goals.Rmd <br/> 
<br/>
**fill_oxygendocs_param** <br/> 
fill in roxygen param documentation from a common params_index <br/> 
the objective of this function is to facilitate consistent use of parameter names throughout the BHI repositories <br/> 
relevant roxygen documentation can be automatically filled in using this function and the supplemental 'params_index.md' document <br/> 
@param script_name the name of the script with functions you want to create documentation for <br/> 
@param params_index a character string giving the file path to this document <br/> 
@param script_function specific functions to create documentation for, if not for entire script <br/> 
@return no returned value, roxygen documentation updated with parameters described based on params_index.md <br/> 
<br/>
