# Using the BHI Database

### Introduction

### Summary of Contents of the Database

### Drivers & Configuring within RStudio

helpful database functions in dplyr: copy_to,
functions in DBI: dbConnect,
from dbplot: creates ggplot object using data within database

#### Extra Info

https://db.rstudio.com/best-practices/drivers/
https://db.rstudio.com/databases/postgresql/
https://db.rstudio.com/odbc/
Connecting to database w/ method supporting DBI package makes using dplyr as a front-end possible
odbc connects via open database connectivity protocol

Setting up OBCD drivers (MacOS instructions):
first install homebrew if not installed (https://brew.sh/)
then via terminal, install the unixODBC library and PostgreSQL ODBC ODBC Drivers:
brew install unixodbc
brew install psqlodbc

edit odbcinst.ini (driver options) and odbc.ini (connection options) files
to find exact location use: odbcinst -j
or rather than depending on DSNs use config package:
'allows connection code in R to reference external file that defines values based on the environment'



### Querying Database

### Loading Data into RStudio

### Adding or Updating Data in Database

### Harmonization Concept & Data Parcels for GIS

### Additional References
