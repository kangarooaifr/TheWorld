

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
    
    # -- declare connectors
    r$activity <- NULL
    
    
    # --------------------------------------------------------------------------
    # Selected tab
    # --------------------------------------------------------------------------
    
    # -- Observe
    r$activity <- reactive({
      
      cat("[EVENT] Selected activity =", input$selected_tab, "\n")
      input$selected_tab
      
    })
    
    
    # --------------------------------------------------------------------------
    # Kitems menu
    # --------------------------------------------------------------------------
    
    # -- kitems: generate dynamic sidebar
    output$menu <- renderMenu(kitems::dynamic_sidebar(r))

    
    # --------------------------------------------------------------------------
    # Modules
    # --------------------------------------------------------------------------
    
    # -- the map
    map_Server(id = "map", r = r, verbose = TRUE)
    
    # -- locations
    location_Server(id = "locationmngr", r = r, path = path, 
                    map_proxy = 'map_proxy', map_click = 'map_click', map_bounds = 'map_bounds', map_zoom = 'map_zoom',
                    map_crop = 'map_crop',
                    filter_country_choices = 'map_country_choices', filter_country = 'map_country')
    
    # -- countries
    country_Server(id = "country", r = r, path = path, map_proxy = 'map_proxy', filter_country = 'map_country')
    
    # -- transports
    route_Server(id = "routemngr", r = r, path = path, map_proxy = 'map_proxy')
    
    # -- tracks
    track_Server(id = "track", r = r, path = path, map_proxy = 'map_proxy')
    
    # -- trips
    trip_Server(id = "tripmngr", r = r, path = path, map_flyto = 'map_flyto')
    
    
    # --------------------------------------------------------------------------
    # Application server ready
    # --------------------------------------------------------------------------
    
    cat("--------------------------------------------------------------------\n")
    cat("Application server ready! \n")
    cat("--------------------------------------------------------------------\n")
    
  }
)
