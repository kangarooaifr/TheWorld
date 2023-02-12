
# -------------------------------------
# Module Server logic
# -------------------------------------

flights_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    
    # -- load airports
    r$airports <- reactiveVal(read.data(path$resource, file_list$airports, colClasses_airports))
    
    # -- load data
    r$flights <- reactiveVal(read.data(path$data, file_list$flights, colClasses_flights))
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------

    # -- add markers to map (hidden)
    observeEvent(input$print_flights, {
      
      cat("Updating flight segments \n")
      
      # -- Helper: add flight route to map
      addFlight <- function(from, to){
        
        cat("[FLIGHTS]    - flight from", from, "to", to, "\n")
        
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
          addPolylines(data = route, group = "flights", color = "purple", weight = 2, popup = "test")
        
      }
      
      # apply helper to flights df
      cat("[FLIGHTS] -- Looping over flight list... \n")
      apply(r$flights(), MARGIN = 1, function(x) addFlight(x['from'], x['to']))
        
    })
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$submit_flights, {
      
      # checkbox marked
      if(input$submit_flights){
        
        cat("Show group: flights \n")
        
        # proxy map
        r$proxymap %>%
          
          # Show group
          showGroup('flights')
        
      }else{
        
        cat("Hide group: flights \n")
        
        # proxy map
        r$proxymap %>%
          
          # clear group
          hideGroup('flights')
      }
      
    })
    
  })
}

