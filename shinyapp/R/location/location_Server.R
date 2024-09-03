

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
    locations <- kitems::kitemsManager_Server(id = "location", r, path$data)
    
    
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
      location <- locations$items()[locations$items()$id == id, ]
      
      # -- build choices
      choices <- list(type = unique(locations$items()$type),
                      country = r$countries_iso$country.en,
                      state = unique(locations$items()$state),
                      city = unique(locations$items()$city))
      
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
      location <- locations$items()[locations$items()$id == id, ]
      
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

      # -- update location
      kitems::item_update(locations$items, location, name = locations$id)
      
    })
    
    
    # -- Observe: action_delete
    observeEvent(input$action_delete, {
    
      # -- extract id
      id <- unlist(strsplit(input$action_delete, split = "_"))[2]
      cat(MODULE, "[EVENT] Marker popup click: delete id =", id, "\n")
      
      # -- delete item
      kitems::item_delete(locations, id, name = "location")
      
    })
    
    
    # -- Observe: action_beenthere
    observeEvent(input$action_beenthere, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_beenthere, split = "_"))[2]
      cat(MODULE, "[EVENT] Marker popup click: been-there id =", id, "\n")
      
      # -- update item
      location <- locations$items()[locations$items()$id == id, ]
      location$been.there <- TRUE
      location$wish.list <- FALSE
      
      # -- update location
      kitems::item_update(locations$items, location, name = locations$id)
      
      
    })
    
  
    # --------------------------------------------------------------------------
    # Module server return value
    # --------------------------------------------------------------------------
    
    # -- composite object
    c(locations, list('airports' = airports))

  })
}
