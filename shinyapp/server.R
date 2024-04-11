

# Shiny: server logic of the Shiny web application

# -- Define server logic

shinyServer(
  function(input, output){
    
    # *******************************************************************************************************
    # DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
    # *******************************************************************************************************
    

    
    
    # *******************************************************************************************************
    # DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
    # *******************************************************************************************************
    
    # declare r communication object
    r <- reactiveValues()
    
    # ----------------------------------------------------------------------
    # - whereGone:
    # - proxymap:
    # - map_center:
    # - airports:
    # - flights:
    # ----------------------------------------------------------------------
    
    # -- kitems: generate dynamic sidebar
    output$menu <- renderMenu(kitems::dynamic_sidebar(r))
    
    # map
    map_Server(id = "map", r = r, path = path)
    
    # where gone
    # whereGone_Server(id = "wheregone", r = r, path = path)
    
    # flights
    flights_Server(id = "flights", r = r, path = path)
    
    # countries
    #countries_Server(id = "countries", r = r, path = path)
    
    #
    tracks_Server(id = "tracks", r = r, path = path)
    
    
    # -- locations
    location_Server(id = "locationmngr", r = r, path = path)
    
    
  }
)
