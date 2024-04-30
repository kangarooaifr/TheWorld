

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

map_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Communication objects
    # --------------------------------------------------------------------------
    
    r$filter_country <- NULL
    r$freeze_map <- NULL
    
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
      cat("[map] Point clicked: lng =", click$lng, "/ lat =", click$lat, "\n")
      
      # -- store
      r$map_click <- input$map_click

    })
    
    
    # -- Observe map center
    observeEvent(input$map_center, {
      
      # -- Get the click info
      center <- input$map_center
      
      # -- print
      cat("[map] Center: lng =", center$lng, "/ lat =", center$lat, "\n")
      
      # -- store
      r$map_click <- NULL
      r$map_center <- input$map_center
      
    })
    
    
    # -- Observer map bounds
    observeEvent(input$map_bounds, {
      bounds <- input$map_bounds
      cat("[map] Bounds: north =", bounds$north, "/ east =", bounds$east, "/ south =", bounds$south, "\ west =", bounds$west, "\n")
      
      # -- store
      r$map_bounds <- bounds
      
    })
    
    
    # -- Observer map zoom
    observeEvent(input$map_zoom, {
      
      zoom <- input$map_zoom
      cat("[map] Zoom =", zoom, "\n")
      
      r$zoom <- zoom
      
    })
    
    
    # --------------------------------------------------------------------------
    # Search
    # --------------------------------------------------------------------------
    
    # -- Observe search
    observeEvent(input$search, {
      
      # -- check
      req(input$search)
      
      # -- print
      cat("[map] search input =", input$search, "\n")
      
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
    
    
    # --------------------------------------------------------------------------
    # Filters
    # --------------------------------------------------------------------------
    
    # -- trigger: set filter choices
    observeEvent(r$filter_country_choices, {
      
      # -- update choices
      updateSelectizeInput(inputId = "filter_country", choices = r$filter_country_choices)
      
    })
    
    
    # -- Observe: update filter
    observeEvent(input$filter_country, {
      
      # -- check filter reset
      if(identical(input$filter_country, ""))
        r$filter_country <- NULL
      else {
        cat("[map] EVENT: Filter country =", input$filter_country, "\n")
        r$filter_country <- input$filter_country}
      
    }, ignoreInit = TRUE)
    
    # -- reset filter
    observeEvent(input$filter_reset, {
      
      cat("[map] EVENT: Reset filter country \n")
      
      # -- update filter
      updateSelectizeInput(inputId = "filter_country", selected = character(0))
      
      
    })
    
    
    # --------------------------------------------------------------------------
    # Freeze map
    # --------------------------------------------------------------------------
    
    # -- update connector
    r$freeze_map <- reactive(input$freeze_map)
    
    
  })
}
