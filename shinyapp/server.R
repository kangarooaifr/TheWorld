

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
                                                                   "accommodation")))

    
    # --------------------------------------------------------------------------
    # Modules
    # --------------------------------------------------------------------------
    
    # -- locations
    locations <- location_Server(id = locationMngrId, r, path)
    
    # -- countries
    countries <- country_Server(id = "country", path)
    
    # -- tracks
    tracks <- track_Server(id = "track", path)
    
    
    # --------------------------------------------------------------------------
    # Activity modules
    # --------------------------------------------------------------------------

    # -- worldmap
    worldmap_Server(id = "worldmap", locations, countries, tracks)
        
    # -- trips
    tripManager_Server(id = "tripmngr", locations, countries, location_ns = locationMngrId, r, path)

    
    # --------------------------------------------------------------------------
    # Application server ready
    # --------------------------------------------------------------------------
    
    
    cat("--------------------------------------------------------------------\n")
    cat("Application server ready! \n")
    cat("--------------------------------------------------------------------\n")
    
  }
)
