

# ------------------------------------------------------------------------------
# Server logic of the Shiny web application
# ------------------------------------------------------------------------------

# -- Define server logic
shinyServer(
  function(input, output){
  
    # --------------------------------------------------------------------------
    # Communication object
    # --------------------------------------------------------------------------
    
    # -- declare object
    r <- reactiveValues()
    
    
    # --------------------------------------------------------------------------
    # Selected tab
    # --------------------------------------------------------------------------
    
    # -- Observe
    # r$activity <- reactive({
    #   
    #   cat("[EVENT] Selected activity =", input$selected_tab, "\n")
    #   input$selected_tab
    #   
    # })
    
    
    # --------------------------------------------------------------------------
    # Kitems menu
    # --------------------------------------------------------------------------
    
    # -- kitems: generate dynamic sidebar
    output$menu <- renderMenu(kitems::dynamic_sidebar(r))

    
    # --------------------------------------------------------------------------
    # Modules
    # --------------------------------------------------------------------------
    
    # -- the maps
    map_Server(id = "world", r = r, verbose = TRUE)
    map_Server(id = "trip", r = r, verbose = TRUE)
    
    # -- locations
    location_Server(id = "locationmngr", r = r, path = path)
    
    # -- countries
    country_Server(id = "country", r = r, path = path)
    
    # -- tracks
    track_Server(id = "track", r = r, path = path)
    
    # -- transports
    route_Server(id = "routemngr", r = r, path = path)
    
    
    # --------------------------------------------------------------------------
    # Activity modules
    # --------------------------------------------------------------------------

    # -- worldmap
    worldmap_Server(id = "worldmap", r = r, mapId = "world", locationId = "location", location_ns = "locationmngr")
        
    # -- trips
    trip_Server(id = "tripmngr", r = r, path = path, mapId = "trip", locationId = "location", location_ns = "locationmngr", routeId = "route")

    
    # --------------------------------------------------------------------------
    # Application server ready
    # --------------------------------------------------------------------------
    
    cat("--------------------------------------------------------------------\n")
    cat("Application server ready! \n")
    cat("--------------------------------------------------------------------\n")
    
  }
)
