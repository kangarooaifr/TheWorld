

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- get namespace
    ns <- session$ns
    
    
    # --------------------------------------------------------------------------
    # Data manager
    # --------------------------------------------------------------------------

    # -- launch kitems sub module
    locations <- kitems::kitemsManager_Server(id = "location", path = path$data)
    
    
    # --------------------------------------------------------------------------
    # Load resources & connector: airports
    # --------------------------------------------------------------------------
    
    # -- File name
    filename_airports <- "airports.csv"
    
    # -- colClasses
    colClasses_airports <- c(id = "numeric",
                             name = "character",
                             city = "character",
                             country = "character",
                             iata = "character",
                             icao = "character",
                             latitude = "numeric",
                             longitude = "numeric",
                             altitude = "numeric")
    
    # -- load data
    airports <- kfiles::read_data(file = filename_airports,
                                    path = path$resources, 
                                    colClasses = colClasses_airports,
                                    create = FALSE)
    
    # -- rename columns to fit with convention & expose connector
    names(airports)[names(airports) == 'latitude'] <- 'lat'
    names(airports)[names(airports) == 'longitude'] <- 'lng'
    
    # -- filter out heliports & entries without iata code (value = "\\N")
    airports <- airports[airports$iata != '\\N', ]
    airports <- airports[!grepl('Heli', airports$name), ]
    cat(MODULE, "Filter airports without iata code & heliports, output =", dim(airports), "\n")
    
    
    # --------------------------------------------------------------------------
    # Load resources & connector: seaports
    # --------------------------------------------------------------------------
    # There is no specific file for seaports at this moment
    # it is taken from the standard locations with type = Port
    
    # -- expose connector
    seaports <- reactive(locations$items()[locations$items()$type == 'Port', ])
    
    
    # --------------------------------------------------------------------------
    # Load resources & connector: stations
    # --------------------------------------------------------------------------
    # stations.csv file is created out of helper function 
    # import_stations()
    
    # -- File name
    filename_stations <- "stations.csv"
    
    # -- colClasses
    colClasses_stations <- c(id = "numeric",
                             internal_id ="numeric",
                             name = "character",
                             slug = "character",
                             uic = "numeric",
                             lat = "numeric",
                             lng = "numeric",
                             parent_id = "numeric",
                             country = "character",
                             time_zone = "character",
                             is_airport = "logical",
                             iata = "character",
                             is_road = "logical",
                             is_rail = "logical")
    
    # -- read data
    stations <- kfiles::read_data(file = filename_stations,
                                  path = path$resources, 
                                  colClasses = colClasses_stations,
                                  create = FALSE)
    
    # -- filter out locations without gps coordinate (lng)
    cat(MODULE, "Filter stations without lng / lat:", sum(is.na(stations$lng)), "\n")
    stations <-  stations[!is.na(stations$lng), ]
    
    # -- expose railway stations
    railway_stations <- stations[!stations$is_rail %in% FALSE, ]
    
    # -- expose bus stations
    bus_stations <- stations[stations$is_road %in% TRUE, ]

    
    # --------------------------------------------------------------------------
    # Module server return value
    # --------------------------------------------------------------------------
    
    # -- composite object
    c(locations, list('airports' = airports,
                      'seaports' = seaports,
                      'railway_stations' = railway_stations,
                      'bus_stations' = bus_stations))

  })
}
