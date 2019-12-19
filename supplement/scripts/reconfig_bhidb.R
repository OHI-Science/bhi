## connect to bhi database ----
## connection with mysql https://www.r-bloggers.com/accessing-mysql-through-r/
## will need to go through each of the original BHI MySQL databases...
databases <- c("BHI", "BHI_level_0", "BHI_level_1", "metafiles")
db <- databases[2]
dbcon <- DBI::dbConnect(
  RMySQL::MySQL(),
  user = user,
  password = password, # don't save password!
  dbname = db,
  host = "bhi.stockholmresilience.su.se"
)

## a function to clean tables ----
library(stringr)
library(dplyr)

clean_tab <- function(con = dbcon, tab){

  ## DATA
  df <- tbl(con, tab) %>%
    collect() %>%
    mutate_if(
      is.character,
      function(x){Encoding(x) = "latin1"; textclean::replace_non_ascii(x)}
    )

  ## COLNAMES
  colnm <- DBI::dbListFields(con, tab)
  ## fixed column names:
  ## replace spaces with underscores, remove dot, square brackets, non-ascii chars, etc...
  editcolnm <- colnames(df) %>%
    str_replace_all(" |\\.", "_") %>%
    str_replace_all("/", "_per_") %>%
    str_remove_all("\\. |\\[|\\]|_$") %>%
    textclean::replace_non_ascii() %>%
    str_replace(pattern = " mu ", "u") %>%
    str_to_lower()

  coltypes <- c()
  for(col in colnm){
    coltypes <- c(coltypes, typeof(df[[col]]))
  }
  colnames(df) <- editcolnm

  ## GEOSPATIAL STUFF
  ## where relevant combine lat long into a single geospatial column
  ## check lat long exist and whether decimal deg or min/sec etc
  chksp <- FALSE
  latlon <- data.frame(
    lon = c("longitude", "long", "lon"),
    lat = c("latitude", "lat", "lat")
  )
  for(i in 1:nrow(latlon)){
    lon <- editcolnm[grep(as.character(latlon[i, "lon"]), editcolnm)]
    if(any(grep(as.character(latlon[i, "lon"]), editcolnm))){
      break
    }
  }
  for(j in 1:nrow(latlon)){
    lat <- editcolnm[grep(as.character(latlon[i, "lat"]), editcolnm)]
    if(any(grep(as.character(latlon[i, "lat"]), editcolnm))){
      break
    }
  }
  if(i == j & length(lon) != 0 & length(lat) != 0){
    if(is.numeric(df[[lon]]) & is.numeric(df[[lat]])){
      chkna <- nrow(df %>% filter(is.na(!!sym(lon))) %>% filter(is.na(!!sym(lat)))) == 0
      chklat <- max(df[[lat]], na.rm = TRUE) < 85 & min(df[[lat]], na.rm = TRUE) > -85
      chklon <- max(df[[lon]], na.rm = TRUE) < 185 & min(df[[lon]], na.rm = TRUE) > -180
      chksp <- chklon & chklat & chkna
    }
  }
  if(chksp){
    df <- sf::st_as_sf(x = df, coords = c(lon, lat), remove = FALSE)
  }

  return(
    list(
      data = df,
      colnames = data.frame(original = colnm, revised = editcolnm, typ = coltypes) %>%
        mutate(
          tabname = tab,
          newtabname = ifelse(
            str_detect(db, "[0-9]"),
            paste0("level", str_extract(db, "[0.9]"), "_", tab), tab
          )
        )
    )
  )
}

## check before migrating BHI-database to srcdb-PostgreSQL ----
## loop through tables, cleaning and checking
chkresults <- list()
for(tab in DBI::dbListTables(dbcon)){
  tabresult <- clean_tab(dbcon, tab)
  chkresults[["colnames"]][[tab]] <- tabresult[["colnames"]]
}
## IMPORTANT save this info about how changed column names and data types
trackcols <- purrr::map(magrittr::extract(chkresults, "colnames"), bind_rows)$colnames
readr::write_csv(trackcols, file.path("/Users/eleanorecampbell/Desktop", "trackDBmigrate.csv"))


## migrate data to PostGIS database ----
## first needs an ssh-tunnel to be set up to the srcdb-server where the database is located with e.g.
## ssh -L 127.0.0.1:1111:127.0.0.1:5432 [user]@srcdb.stockholmresilience.su.se
## connecting w postgres https://www.r-bloggers.com/r-and-postgresql-using-rpostgresql-and-sqldf/
library(RPostgreSQL)
# library(rpostgis)
postgresdbcon <- DBI::dbConnect(
  DBI::dbDriver("PostgreSQL"),
  dbname = "marine",
  user = user,
  password = password, # don't save password here!
  host = "localhost",
  port = 1111
)
rpostgis::pgPostGIS(postgresdbcon) # check postgis

## uploading data tables to the PostGIS database
for(tab in listtables){
  tabresult <- clean_tab(dbcon, tab)
  dbWriteTable(
    conn = testcon,
    name = c("bhi", "testSPATIAL"),
    value = tabresult[["data"]],
    row.names = FALSE,
    binary = TRUE
  )
}

## disconnect after transfers completed
dbDisconnect(dbcon)
dbDisconnect(postgresdbcon)
