

# ------------------------------------------------------------------------------
# Shiny module: dependencies
# ------------------------------------------------------------------------------

# -- Library
library(geosphere)


# ------------------------------------------------------------------------------
# Module Server logic
# ------------------------------------------------------------------------------

transport_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -------------------------------------
    # Load resources (airports)
    # -------------------------------------
    
    # -- File name
    filename <- "airports.csv"
    
    
    # -- colClasses
    colClasses_airports <- c(Name = "character",
                             City = "character",
                             Country = "character",
                             IATA = "character",
                             ICAO = "character",
                             Latitude = "numeric",
                             Longitude = "numeric",
                             Altitude = "numeric")
    
    
    # -- load airports
    airports <- kfiles::read_data(file = filename,
                                  path = path$resources, 
                                  colClasses = colClasses_airports,
                                  create = FALSE)
    
    
    # -------------------------------------
    # Data manager (transports)
    # -------------------------------------
    
    # -- module id
    kitems_id <- "transport"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------

    # -- add markers to map (hidden)
    observeEvent(input$print_transports, {
      
      cat("Updating transport segments \n")
      
      # -- Helper: add transport route to map
      addtransport <- function(from, to){
        
        cat("[transportS]    - transport from", from, "to", to, "\n")
        
        # -- compute great circle route
        route <- gcIntermediate(p1 = get_iata_lnglat(airports = r$airports(), 
                                                     iata.code = from), 
                                p2 = get_iata_lnglat(airports = r$airports(), 
                                                     iata.code = to), 
                                n = 100, 
                                addStartEnd = TRUE)
        
        # -- add to proxy map
        r$proxymap %>%
          
          # add fight route
          addPolylines(data = route, group = "transports", color = "purple", weight = 2, popup = "test")
        
      }
      
      # apply helper to transports df
      cat("[transportS] -- Looping over transport list... \n")
      apply(r$transports(), MARGIN = 1, function(x) addtransport(x['from'], x['to']))
        
    })
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$submit_transports, {
      
      # checkbox marked
      if(input$submit_transports){
        
        cat("Show group: transports \n")
        
        # proxy map
        r$proxymap %>%
          
          # Show group
          showGroup('transports')
        
      }else{
        
        cat("Hide group: transports \n")
        
        # proxy map
        r$proxymap %>%
          
          # clear group
          hideGroup('transports')
      }
      
    })
    
  })
}

