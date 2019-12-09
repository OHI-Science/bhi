### Recalculating 2014 Scores with Revised Data & Functions

This folder uses the previous BHI assessment materials (data, functions, configuration), with a few details tweaked.

### `wFis2019data.Rmd`
Some data for the **fisheries goal** was corrected/completed after the first BHI assessment was completed. The `wFis2019data.Rmd` script incorporates these new fisheries data into a recalcualtion of the 2014 scores

### `woCONpenalty.Rmd`
The **contaminants goal** includes a penalty based on proportions of hazardous substances not monitored. The `woCONpenalty.Rmd` script recalculates the 2014 scores without applying this penalty

### `compare`
The `compare` folder contains scripts with different versions of the functions. In particular, the FIS function is slightly modified in `functionsv2019` from `functionsv2015`, and the `functions2019_cwNew.R` script has some tweaks to the CW functions was used for testing the effect of excluding the CON penalty. The original layers.csv is also saved here as `layers2015.csv` which serves a basis for the analysis.
