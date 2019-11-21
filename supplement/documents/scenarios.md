# Exploring Scenarios & Alternative Model Specifications

### Introduction

### Structure and Organization

Multiple versions of different layers may be calculated in a given assesssment year, for different scenarios. These will be prepared in the same data prep file of the `bhi-prep` repository, with differences between the layers explored and explained there and summarized in a markdown document in the  `bhi` repo scenario folder. All of these different versions of a layer will be saved to the assessment year 'layers' folder (e.g. `bhi-prep/layers/v2019`).

When BHI scores are calculated using the `ohicore` functions, they draw information from scenario rather than data prep assessment year folders; all configuration information is included in the scenario folder, including:

- `conf` folder (`functions.R`, `resilience_categories`, `resilience_matrix`, `pressure_categories`, `pressure_matrix`)
- layers, registered in `layers.csv` and documented in `layers_metadata.csv`

Versions of layers to be used are registered in the `layers.csv` file, and then based on this layers metadata is automatically updated and the correct layer files are copied over to the scenario layers folder from the `bhi-prep` repository.


### Identifying and Characterizing Scenarios

### Creating and Using Simulated Data

### Structure & Contents of the testing Folder

### Testing Alternative Data Layers or Model Specifications

### Additional References
