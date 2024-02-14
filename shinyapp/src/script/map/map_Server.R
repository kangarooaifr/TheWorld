
# --------------------------------------------------------------------------------
# Shiny module: map
# --------------------------------------------------------------------------------

library(leaflet)


# -------------------------------------
# Server logic
# -------------------------------------

map_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------
    # Outputs
    # -------------------------------------
    
    # -- Output: map
    output$map <- renderLeaflet({
      leaflet() %>%
        
        # Add default OpenStreetMap map tiles
        addTiles(group = "OSM") %>%
        
        # Set view point
        setView(lng = 2.55, lat = 49, zoom = 5)
        
        # Add National Geographic
        #addProviderTiles(providers$Stamen.Watercolor)
      
    })
    
    # -- Declare proxy map
    r$proxymap <- leafletProxy('map')
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
    
    # -- Observe mouse clicks
    observeEvent(input$map_click, {

      # Get the click info
      click <- input$map_click

      # print
      cat("Point clicked: lng =", click$lng, "/ lat =", click$lat, "\n")
      
      # store
      r$map_click <- input$map_click

    })
    
    
    # -- Observe map center
    observeEvent(input$map_center, {
      
      # Get the click info
      center <- input$map_center
      
      # print
      cat("Map center: lng =", center$lng, "/ lat =", center$lat, "\n")
      
      # cache map center
      r$map_center <- input$map_center
      
    })
    
    
    # -- Observe search
    observeEvent(input$search, {
      
      # check value
      req(input$search)
      
      # trace      
      cat("search input =", input$search, "\n")
      
      # get search result
      res <- mygeocode(input$search)
      
      # update map
      if (!is.null(res)){
        leafletProxy('map') %>%
          flyTo('map', lng = res[1], lat = res[2], zoom = 12)
      }
      else{
        showNotification("No result found", type = "error")
      }
      
    })
    
  })
}

