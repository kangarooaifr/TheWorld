

# ------------------------------------------------------------------------------
# Import UIC Railway Stations
# ------------------------------------------------------------------------------
# 
# This is a helper function to import data
# It should not be used directly by the app
# 

import_railway_stations <- function(){
  
  # -- inside function (not run upon source code)
  library(readr)
  
  # -- param
  filename <- "shinyapp/resources/railway_stations.csv"
  
  # -- columns
  cols_keep <- c("id", "name", "slug", "uic", "latitude", "longitude", "parent_station_id", "country", "time_zone", "is_airport", "iata_airport_code")
  cols_rename <- c("internal_id", "name", "slug", "uic", "lat", "lng", "parent_id", "country", "time_zone", "is_airport", "iata")
  
  # -- read file
  railway_stations <- read_delim(filename, 
                                 delim = ";",
                                 escape_double = FALSE, 
                                 trim_ws = TRUE)
  
  # -- slice columns
  railway_stations <- railway_stations[cols_keep]
  colnames(railway_stations) <- cols_rename
  
  # -- generate ids
  railway_stations$id <- ktools::seq_timestamp(n = dim(railway_stations)[1])
  
  # -- return
  railway_stations
  
}