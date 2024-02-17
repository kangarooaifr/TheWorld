

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

map_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # The map
    # --------------------------------------------------------------------------
    
    # -- Declare the map output
    output$map <- renderLeaflet({
      
      leaflet() %>%
        
        # -- Add default OpenStreetMap map tiles
        addTiles(group = "OSM") %>%
        
        # -- Set view point
        # TODO: set values as parameters (probably saved as RDS?)
        setView(lng = 2.55, lat = 49, zoom = 5)
        
        # -- Add National Geographic
        # TODO: set as optional parameters when calling module
        #addProviderTiles(providers$Stamen.Watercolor)
      
    })
    
    # -- Declare proxy for the map
    r$proxymap <- leafletProxy('map')
    
    
    # --------------------------------------------------------------------------
    # Event observers
    # --------------------------------------------------------------------------
    
    # -- Observe mouse clicks
    observeEvent(input$map_click, {

      # -- Get the click info
      click <- input$map_click

      # -- print
      cat("Point clicked: lng =", click$lng, "/ lat =", click$lat, "\n")
      
      # -- store
      r$map_click <- input$map_click

    })
    
    
    # -- Observe map center
    observeEvent(input$map_center, {
      
      # -- Get the click info
      center <- input$map_center
      
      # -- print
      cat("Map center: lng =", center$lng, "/ lat =", center$lat, "\n")
      
      # -- store
      r$map_center <- input$map_center
      
    })
    
    
    # -- Observe search
    observeEvent(input$search, {
      
      # -- check
      req(input$search)
      
      # -- print
      cat("search input =", input$search, "\n")
      
      # -- get search result
      # TODO: rework, this function should be part of a package
      res <- mygeocode(input$search)
      
      # -- check & update map
      if (!is.null(res)){
        
        leafletProxy('map') %>%
          flyTo('map', lng = res[1], lat = res[2], zoom = 12)
        
      } else 
        
        showNotification("No result found", type = "error")
      
    })
    
  })
}
