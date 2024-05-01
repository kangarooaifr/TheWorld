

# Shiny: server logic of the Shiny web application

# -- Define server logic

shinyServer(
  function(input, output){
  
    # declare r communication object
    r <- reactiveValues()
    
    # -- declare connectors
    r$activity <- NULL
    
    # -- observe: selected tab (tabsetPanel)
    # Can't use eventReactive (idk why)
    observeEvent(input$selected_tab, {
      
      cat("[EVENT] Selected tab =", input$selected_tab, "\n")
      r$activity <- input$selected_tab
      
    })
    
    # -- kitems: generate dynamic sidebar
    output$menu <- renderMenu(kitems::dynamic_sidebar(r))

    # -- the map
    map_Server(id = "map", r = r, path = path)
    
    # -- locations
    location_Server(id = "locationmngr", r = r, path = path)
    
    # -- countries
    country_Server(id = "country", r = r, path = path)
    
    # -- transports
    route_Server(id = "routemngr", r = r, path = path)
    
    # -- tracks
    track_Server(id = "track", r = r, path = path)
    
    # -- trips
    trip_Server(id = "tripmngr", r = r, path = path)
    
  }
)
