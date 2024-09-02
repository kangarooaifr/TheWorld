

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, locationId, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- get namespace
    ns <- session$ns
    
    # -- items name
    r_trigger_update <- kitems::trigger_update_name(id = locationId)
    r_trigger_delete <- kitems::trigger_delete_name(id = locationId)
    
    
    # --------------------------------------------------------------------------
    # Data manager
    # --------------------------------------------------------------------------

    # -- launch kitems sub module
    locations <- kitems::kitemsManager_Server(id = locationId, r, path$data)
    
    
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
    raw_airports <- kfiles::read_data(file = filename_airports,
                                    path = path$resources, 
                                    colClasses = colClasses_airports,
                                    create = FALSE)
    
    # -- rename columns to fit with convention & expose connector
    names(raw_airports)[names(raw_airports) == 'latitude'] <- 'lat'
    names(raw_airports)[names(raw_airports) == 'longitude'] <- 'lng'
    
    # -- filter out heliports & entries without iata code (value = "\\N")
    raw_airports <- raw_airports[raw_airports$iata != '\\N', ]
    raw_airports <- raw_airports[!grepl('Heli', raw_airports$name), ]
    cat(MODULE, "Filter airports without iata code & heliports, output =", dim(raw_airports), "\n")
    
    # -- store & delete temp object
    r$airports <- raw_airports
    rm(raw_airports)
    
    
    # --------------------------------------------------------------------------
    # Load resources & connector: seaports
    # --------------------------------------------------------------------------
    # There is no specific file for seaports at this moment
    # it is taken from the standard locations with type = Port
    
    # -- expose connector
    r$seaports <- reactive(locations()[locations()$type == 'Port', ])
    
    
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
    r$railway_stations <- stations[!stations$is_rail %in% FALSE, ]
    
    # -- expose bus stations
    r$bus_stations <- stations[stations$is_road %in% TRUE, ]

    
    # --------------------------------------------------------------------------
    # Actions (click from marker popup)
    # --------------------------------------------------------------------------
    
    # -- Observe: action_update
    observeEvent(input$action_update, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_update, split = "_"))[2]
      cat(MODULE, "[EVENT] Marker popup click: update id =", id, "\n")
      
      # -- get location to update
      location <- locations()[locations()$id == id, ]
      
      # -- build choices
      choices <- list(type = unique(locations()$type),
                      country = r$countries_iso$country.en,
                      state = unique(locations()$state),
                      city = unique(locations()$city))
      
      # -- display form
      showModal(location_modal(location, choices = choices, ns = ns))
      
    })
    
    
    # -- Event: btn confirm_update_location
    observeEvent(input$confirm_update_location, {
      
      # -- close dialog
      removeModal()
      
      # -- extract location id
      id <- unlist(strsplit(input$action_update, split = "_"))[2]
      
      # -- get location to update
      location <- locations()[locations()$id == id, ]
      
      # -- update values
      location$name = input$name
      location$type = input$type
      location$country = input$country
      location$state = input$state
      location$zip.code = input$zip.code
      location$city = input$city
      location$address = input$address
      location$comment = input$comment
      location$been.there = input$been.there
      location$wish.list = input$wish.list

      # -- call trigger
      r[[r_trigger_update]](location)
      
    })
    
    
    # -- Observe: action_delete
    observeEvent(input$action_delete, {
    
      # -- extract id
      id <- unlist(strsplit(input$action_delete, split = "_"))[2]
      cat(MODULE, "[EVENT] Marker popup click: delete id =", id, "\n")
      
      # -- call trigger
      r[[r_trigger_delete]](id)
      
    })
    
    
    # -- Observe: action_beenthere
    observeEvent(input$action_beenthere, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_beenthere, split = "_"))[2]
      cat(MODULE, "[EVENT] Marker popup click: been-there id =", id, "\n")
      
      # -- update item
      item <- locations()[locations()$id == id, ]
      item$been.there <- TRUE
      item$wish.list <- FALSE
      
      # -- call trigger
      r[[r_trigger_update]](item)
      
    })
    
  
    # --------------------------------------------------------------------------
    # Module server return value
    # --------------------------------------------------------------------------
    
    # -- the items reference from the sub module (not its value!)
    locations

  })
}
