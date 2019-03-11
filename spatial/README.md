# `spatial` 

<br/>

**`bhi_basin_country_lookup.csv`** 

Created on: <br/>
Last modified on: <br/>
Used by or referenced in functions/scripts: spatial.R create_rgn_lookup <br/>

* rgn_nam: character <br/>
* rgn_key: character <br/>
* Subbasin: character <br/>
* HELCOM_ID: character <br/>
* BHI_ID: integer <br/>
* Area_km2: numeric <br/>
* rgn_nam: country common names (one of: Sweden, Denmark, Germany, Poland, Russia, Lithuania, Latvia, Estonia, Finland) <br/>
* rgn_key: three-character codes for each of the Baltic countries (one of: SWE, DNK, DEU, POL, RUS, LTU, LVA, EST, FIN) <br/>
* Subbasin: names of baltic sea subbasins of which there are seventeen <br/>
* HELCOM_ID: 'SEA-' followed by a three digit code between 001 and 017 <br/>
* BHI_ID:  integers 1 through 42 <br/>
* Area_km2: areas in square kilometers to four decimal decimal places <br/>

<br/>

**`eez_lookup.csv`**

Created on: <br/>
Last modified on: <br/>
Used by functions/scripts: spatial.R create_rgn_lookup, <br/>

Columns/variables/features: 

* eez: integer <br/>
* rgn_key: character <br/>
* eez: one of 1, 2, 3, 4, 5, 6, 7, 8, 9 <br/>
* rgn_key: one of SWE, DNK, DEU, POL, RUS, LTU, LVA, EST, FIN  <br/>

<br/>

**`rgn_labels.csv`**

Created on: <br/>
Last modified on: <br/>
Used by or referenced in functions/scripts: spatial.R create_rgn_lookup <br/>

* rgn_id: integer <br/>
* type: character <br/>
* label: character <br/>
* rgn_id: integers 1 through 42 <br/>
* type: 'eez' for all rows <br/>
* label: three letter country code and subbasin name separated by ' - ' <br/>

<br/>

**`regions_list.csv`**

Created on: <br/>
Last modified on: <br/>
Used by or referenced in functions/scripts: <br/>

* rgn_id: integer <br/>
* rgn_name: character <br/>
* area_km2: numeric <br/>
* rgn_id: integers 1 through 42 <br/>
* rgn_name: three letter country code and subbasin name separated by ' - ' <br/>
* area_km2: areas in square kilometers to five decimal decimal places <br/>

<br/>

**`regions_gcs.geojson`**

Created on: <br/>
Last modified on: <br/>
Used by functions/scripts: spatial.R create_rgn_lookup <br/>

<br/>
