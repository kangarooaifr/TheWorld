
# --------------------------------------------------------------------------------
# Shiny module: flights
# --------------------------------------------------------------------------------

# -- Library
library(geosphere)

# -- Source dependencies


# -------------------------------------
# UI items section
# -------------------------------------

# -- Where gone checkbox
flights_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # hide / show checkbox
    checkboxInput(ns("submit_flights"), label = "flights", value = FALSE, width = NULL)
    
  )
  
}


# -------------------------------------
# Server logic
# -------------------------------------

flights_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- prepare
    filename <- "airports.csv"
    cols <- c(Name = "character",
              City = "character",
              Country = "character",
              IATA = "character",
              ICAO = "character",
              Latitude = "numeric",
              Longitude = "numeric",
              Altitude = "numeric")
    
    # -- load airports
    r$airports <- reactiveVal(read.data(path$resource, filename, cols))
    
    # -- prepare
    filename <- "flights.csv"
    cols <- c(id = "numeric",
              from = "character",
              to = "character",
              date = "character",
              airline = "character",
              flight.number = "character",
              departure.time = "character",
              arrival.time = "character")
    
    # -- load data
    r$flights <- reactiveVal(read.data(path$data, filename, cols))

    # -- add markers to map (hidden)
    observeEvent(r$flights(), {
      
      airports <<- r$airports()
      flights <<- r$flights()
      
      cat("Updating flight segments \n")
      
      # -- Helper: get airport coordinates
      getLonLat <- function(x){
        
        lng = airports[airports['IATA'] == x, ]$Longitude
        lat = airports[airports['IATA'] == x, ]$Latitude
        
        c(lng, lat)
        
      }
      
      # -- Helper: add flight route to map
      addFlight <- function(from, to){
        
        route <- gcIntermediate(getLonLat(from), getLonLat(to), n = 100, addStartEnd = TRUE)
        
        # get proxy map
        r$proxymap %>%
          
          # add fight route
          addPolylines(data = route, group = "flights", color = "blue", weight = 3, popup = "test")
        
      }
      
      # apply helper to flights df
      apply(flights, MARGIN = 1, function(x) addFlight(x['from'], x['to']))
        
    })
    
    
    # -------------------------------------
    # Outputs
    # -------------------------------------
    
    
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

