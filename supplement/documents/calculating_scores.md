# Calculating BHI Scores

### Introduction

### Configuration

### Key ohicore Functions

---

### Registering Layers

#### `layers.csv`

<br/>

Each row of `layers.csv` table corresponds to a single layer used in calculating the Baltic Health Index scores. In some cases layers will feed into a few different goals or 'dimensions' via different models; this is what multiple targets in the 'target' column indicates. This table contains some columns which are to be manually edited, and some that are updated by `ohicore` functions during the configuration prior to score calculation. These two categories are separated into two sections below, with descriptions of what each is. When a new layer (i.e. a separate, new dataset) is incorporated into the index, a row should be added for it in `layers.csv`, with... (what to set as ohicore fields defaults?)

The table can be updated using the `layers_edit` function found in the `reconfiguring.R` script. There is some overlap with `layers_metadata.csv` --layer, filename, targets, name, description columns.

<br/>

#### Columns to update manually:

**layer** <br/>
The aliases of the layers used throughout the bhi code, by functions.R and the ohicore functions when calculating scores. These are snake case (lower case and underscores) with the first piece being the name of the two or three character code for the goal it corresponds to, rgn if it is a spatial input, or another prefix... (ideally these would be more consistent...).

**filename** <br/>
This is the name of the actual file containing data used for a given layer. Since many layers will go through multiple iterations or versions, filenames are used to distinguish which we will use in the analysis.

**targets** <br/>
Which goals or dimensions that the given layer is used to calculate. This serves to connect models in functions.R with their input datasets.

**name** <br/>
The full, descriptive name of the dataset.

**description** <br/>
Expands on the information summarized in the 'name' variable to explain more fully about the original source, ranges, and in some cases methods of collection or creation of the data.

**fld_value** <br/>
**units** <br/>
**clip_n_ship_disag** <br/>
**clip_n_ship_disag_description** <br/>
**path_in** <br/>
**rgns_in** <br/>
**layer_gl** <br/>

<br/>

#### To be updated by `ohicore::CheckLayer`:

For more insight about these fields see the [source code](https://github.com/OHI-Science/ohicore/blob/master/R/CheckLayers.R) for the `ohicore::CheckLayers` function.

**fld_id_num** <br/>
**fld_id_chr** <br/>
**fld_category** <br/>
**fld_year** <br/>
**fld_val_num** <br/>
**fld_val_chr** <br/>
**file_exists** <br/>
**year_min** <br/>
**year_max** <br/>
**val_min** <br/>
**val_max** <br/>
**val_0to1** <br/>
**flds_unused** <br/>
**flds_missing** <br/>
**rows_duplicated** <br/>
**num_ids_unique** <br/>
**data_na** <br/>

---

### The calculate_scores.Rmd Routine

### Mechanisms & Mechanics

### Additional References
