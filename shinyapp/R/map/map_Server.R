

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

map_Server <- function(id, r, verbose = TRUE) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting map module server... \n")
    
    # -- fly to settings
    fly_duration <<- 1.0
    fly_padding <<- 50
    fly_zoom <<- 12
    
    
    # --------------------------------------------------------------------------
    # Communication objects
    # --------------------------------------------------------------------------
    
    # -- declare map names
    map_proxy <- paste0(id, "_proxy")
    map_click <- paste0(id, "_click")
    map_center <- paste0(id, "_center")
    map_bounds <- paste0(id, "_bounds")
    map_zoom <- paste0(id, "_zoom")
    map_flyto <- paste0(id, "_flyto")
    
    # -- declare connectors
    r[[map_proxy]] <- NULL
    r[[map_click]] <- NULL
    r[[map_center]] <- NULL
    r[[map_bounds]] <- NULL
    r[[map_zoom]] <- NULL
    
    # -- declare triggers
    r[[map_flyto]] <- NULL
    
    
    # --------------------------------------------------------------------------
    # The map
    # --------------------------------------------------------------------------
    
    # -- Declare the map output
    output$map <- renderLeaflet({
      
      cat(MODULE, "Generate leaflet output \n")
      
      leaflet() %>%
        
        # -- Add default OpenStreetMap map tiles
        addTiles(group = "OSM") %>%
        
        # -- Set view point
        # TODO: set values as parameters (probably saved as RDS?)
        setView(lng = 2.55, lat = 49, zoom = 5)
        
        # -- Add National Geographic
        # TODO: set as optional parameters when calling module
        # addProviderTiles(providers$Stamen.Watercolor)
      
    })
    
    # -- Declare proxy for the map
    r[[map_proxy]] <- leafletProxy('map')
    
    
    # --------------------------------------------------------------------------
    # Map observers & connectors
    # --------------------------------------------------------------------------
    
    # -- Connector: mouse click
    r[[map_click]] <- reactive({

      # -- check
      req(input$map_click)
      
      # -- trace
      if(verbose)
        cat(MODULE, "Point clicked: lng =", input$map_click$lng, "/ lat =", input$map_click$lat, "\n")
      
      # -- return
      input$map_click

    })
    
    
    # -- Connector: map center
    r[[map_center]] <- reactive({
      
      # -- check
      req(input$map_center)
      
      # -- trace
      if(verbose)
        cat(MODULE, "Center: lng =", input$map_center$lng, "/ lat =", input$map_center$lat, "\n")
      
      # -- return
      input$map_center
      
    })
    
    
    # -- Connector: map bounds
    r[[map_bounds]] <- reactive({
      
      # -- check
      req(input$map_bounds)
      
      # -- trace
      if(verbose)
        cat(MODULE, "Bounds: north =", input$map_bounds$north, "/ east =", input$map_bounds$east, 
            "/ south =", input$map_bounds$south, "\ west =", input$map_bounds$west, "\n")
      
      # -- return
      input$map_bounds
      
    })
    
    
    # -- Connector: map zoom
    r[[map_zoom]] <- reactive({
      
      # -- check
      req(input$map_zoom)
      
      # -- trace
      if(verbose)
        cat(MODULE, "Zoom: level =", input$map_zoom, "\n")
      
      # -- return
      input$map_zoom
      
    })
    
    
    # --------------------------------------------------------------------------
    # Trigger: map_flyto
    # --------------------------------------------------------------------------
    # r[[map_flyto]] = list(lng, lat)
    
    # -- observe trigger
    observeEvent(r[[map_flyto]], {
      
      # -- check setting
      req(!input$map_freeze)
      
      # -- trace
      if(verbose)
        cat(MODULE, "Trigger map_flyto, applying flyTo \n")
      
      # -- crop view
      r[[map_proxy]] %>%
        flyTo(lng = r[[map_flyto]]$lng, 
              lat = r[[map_flyto]]$lat,
              zoom = fly_zoom,
              options = list(duration = fly_duration, 
                             padding = c(fly_padding, fly_padding)))
      
      # -- unset trigger (otherwise you can't call again with same value)
      r[[map_flyto]] <- NULL
      
    })
    
    
    # --------------------------------------------------------------------------
    # Search
    # --------------------------------------------------------------------------
    
    # -- Observe: search input
    observeEvent(input$search, {
      
      # -- check
      req(input$search)
      
      # -- print
      cat(MODULE, "search input =", input$search, "\n")
      
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
