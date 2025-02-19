

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
    # Names
    # --------------------------------------------------------------------------
    
    locationMngrId <- "locationmngr"
    routeId <- "route"
    
    
    # --------------------------------------------------------------------------
    # Settings
    # --------------------------------------------------------------------------
    
    # -- call module
    settings_Server(id = "setting", path)
    
    
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
    
    # -------------------------------------
    # save for later
    # To dynamically switch from a tabItem to another:
    # updateTabItems(session, "inTabset", selected = "widgets")
    # -------------------------------------
    
    
    # --------------------------------------------------------------------------
    # Kitems menu
    # --------------------------------------------------------------------------
    
    # -- kitems: generate dynamic sidebar
    output$menu <- renderMenu(kitems::dynamic_sidebar(names = list("location", 
                                                                   "route",
                                                                   "trip",
                                                                   "step",
                                                                   "transport",
                                                                   "accommodation")))

    
    # --------------------------------------------------------------------------
    # Modules
    # --------------------------------------------------------------------------
    
    # -- the maps
    worldMap <- map_Server(id = "world", r = r, verbose = TRUE)
    tripMap <- map_Server(id = "trip", r = r, verbose = TRUE)
    
    # -- locations
    locations <- location_Server(id = locationMngrId, r, path)
    
    # -- countries
    countries <- country_Server(id = "country", path)
    
    # -- tracks
    tracks <- track_Server(id = "track", path)
    
    # -- transports
    routes <- route_Server(id = "routemngr", routeId, r, path)
    
    
    # --------------------------------------------------------------------------
    # Activity modules
    # --------------------------------------------------------------------------

    # -- worldmap
    worldmap_Server(id = "worldmap", map = worldMap, locations, countries, tracks)
        
    # -- trips
    trip_Server(id = "tripmngr", map = tripMap, locations, location_ns = locationMngrId, routes, r, path)

    
    # --------------------------------------------------------------------------
    # Application server ready
    # --------------------------------------------------------------------------
    
    
    cat("--------------------------------------------------------------------\n")
    cat("Application server ready! \n")
    cat("--------------------------------------------------------------------\n")
    
  }
)
