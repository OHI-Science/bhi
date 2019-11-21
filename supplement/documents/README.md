# Supplemental Documents


The README documents sprinkled throughout the repo aim to provide guidance on folder structure and contents while navigating or attempting to locate something specific. However, they do not provide thorough explanation of how those items are or can be integrated and combined during the assessment.

The documents contained in this folder are intended to give more detailed information on how to use the BHI repo to run assessments. They break the process into sub-tasks including:

* `updating_data.md`: gathering data especially through automated routines, and evaluating new datasets
* `exploring_layers.md`: exploring the data layers and conducting basic checks prior to score calculation  
* `gapfilling.md`: filling data gaps and tracking the occurance and patterns of gapfilling  
* `investigating_uncertainty.md`: investigating uncertainties  
* `calculating_scores.md`: more detail on mechanics and routine of calculating the index scores  
* `creating_reports.md`: generating reports including the production of standardized figures

Additional documents provide instruction or advice on:

* `using_database.md`: interacting with (reading from, adding to or updating data within) the BHI postgresql database
* `scenarios.md`: how to test different models or data layers, to simulate different scenarios  
* `maintaining_repo.md`: the tedious but important task of maintaining meta information  
* `finalizing_assessment.md`: assorted tasks and book-keeping for wrapping up an assessment  
* `maintaining_shiny.md`: app structure and details about hosting and database connections   
* `param_index.md`: list of common/standard parameters used, part of an effort to reduce redundancy/improve clarity   

The intended audiences of these documents are (1) future BHI team memebers, and (2) anyone that forks the github repo in order to explore the analysis and/or conduct additional analyses within the BHI framework.

These documents will be written over the course of the `BHI2.0` assessment, and continue to evolve as the code and communication aspects evolve. Eventually these may become pkgdown vignettes...
