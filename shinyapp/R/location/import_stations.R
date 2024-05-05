

# ------------------------------------------------------------------------------
# Import Stations (Bus & Rail)
# ------------------------------------------------------------------------------
# 
# This is a helper function to import data
# It should not be used directly by the app
# 

import_stations <- function(){
  
  # -- inside function (not run upon source code)
  library(readr)
  
  # -- param
  filename <- "shinyapp/resources/stations.csv"
  
  # -- columns
  cols_keep <- c("id", "name", "slug", "uic", "latitude", "longitude", "parent_station_id", "country", "time_zone", "is_airport", "iata_airport_code")
  cols_rename <- c("internal_id", "name", "slug", "uic", "lat", "lng", "parent_id", "country", "time_zone", "is_airport", "iata")
  
  # -- read file
  stations <- read_delim(filename, 
                                 delim = ";",
                                 escape_double = FALSE, 
                                 trim_ws = TRUE)
  
  # -- slice columns
  stations <- stations[cols_keep]
  colnames(stations) <- cols_rename
  
  # -- generate ids
  # Warning! Performance pb ~13min
  stations$id <- ktools::seq_timestamp(n = dim(stations)[1])
  
  # -- return
  stations
  
}


import_bus_stations <- function(stations){
  
  # -- list of identified keywords
  keywords <- c("autobus", "autobuses", "autobusni", "avtobusna", "autobusowy",
                "busstation", "bussterminal", "busterminal",
                "busbf", "fernbus", "busbahnhof",
                "routiere")
  
  # -- get indexes
  idx <- grep(paste(keywords, collapse = "|"), stations$slug)
  
  # -- init & tag
  stations$is_road <- NA
  stations[idx, ]$is_road <- TRUE
  stations$is_rail <- NA
  stations[idx, ]$is_rail <- FALSE
  
  # -- return
  stations

}

