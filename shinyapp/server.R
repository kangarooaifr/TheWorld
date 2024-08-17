

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
    
    # -- the map
    map_Server(id = "world", r = r, verbose = TRUE)
    map_Server(id = "trip", r = r, verbose = TRUE)
    
    # -- locations
    location_Server(id = "locationmngr", r = r, path = path,
                    map_proxy = 'world_proxy', map_click = 'world_click')
    
    # -- countries
    #country_Server(id = "country", r = r, path = path, map_proxy = 'map_proxy', filter_country = 'map_country')
    
    # -- transports
    #route_Server(id = "routemngr", r = r, path = path, map_proxy = 'map_proxy')
    
    # -- tracks
    #track_Server(id = "track", r = r, path = path, map_proxy = 'map_proxy')
    
    # -- trips
    #trip_Server(id = "tripmngr", r = r, path = path, map_flyto = 'map_flyto')
    
    # -- worldmap
    worldmap_Server(id = "worldmap", r = r, location_id = "location", map_id = "world")
    
    
    # --------------------------------------------------------------------------
    # Application server ready
    # --------------------------------------------------------------------------
    
    cat("--------------------------------------------------------------------\n")
    cat("Application server ready! \n")
    cat("--------------------------------------------------------------------\n")
    
  }
)
