# Organization and Contents of `layers.csv`

---


### Notes on the `layers.csv` file

Each row of `layers.csv` table corresponds to a single layer used in calculating the Baltic Health Index scores. In some cases layers will feed into a few different goals or 'dimensions' via different models; this is what multiple strings separated by spaces within 'targets' column indicates. This table contains some columns which are to be manually edited, and some that are updated by `ohicore` functions during the configuration prior to score calculation. These two categories are separated into two sections below, with descriptions of what each is. When a new layer (i.e. a separate, new dataset) is incorporated into the index, a row should be added for it in `layers.csv`, with... (what to set as ohicore fields defaults?)


### Columns to update manually

**layer**

The aliases of the layers used throughout the bhi code, by functions.R and the ohicore functions when calculating scores. These are snake case (lower case and underscores) with the first piece being the name of the two or three character code for the goal it corresponds to, rgn if it is a spatial input, or another prefix... (ideally these would be more consistent...).

**filename**

This is the name of the actual file containing data used for a given layer. Since many layers will go through multiple iterations or versions, filenames are used to distinguish which we will use in the analysis.

**targets**

Which goals or dimensions that the given layer is used to calculate. This serves to connect models in functions.R with their input datasets.

**name**

The full, descriptive name of the dataset.

**description**

Expands on the information summarized in the 'name' variable to explain more fully about the original source, ranges, and in some cases methods of collection or creation of the data.

**fld_value**
**units**
**clip_n_ship_disag**
**clip_n_ship_disag_description**
**path_in**
**rgns_in**
**layer_gl**


---

### Columns to be updated by `ohicore` CheckLayer() function

For more insight about these fields see the [source code](https://github.com/OHI-Science/ohicore/blob/master/R/CheckLayers.R) for the CheckLayers function.

**fld_id_num**
**fld_id_chr**
**fld_category**
**fld_year**
**fld_val_num**
**fld_val_chr**
**file_exists**
**year_min**
**year_max**
**val_min**
**val_max**
**val_0to1**
**flds_unused**
**flds_missing**
**rows_duplicated**
**num_ids_unique**
**data_na**


