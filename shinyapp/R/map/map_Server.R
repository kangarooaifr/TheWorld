

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
    
    # -- fly to settings
    fly_duration <- 1.0
    fly_padding <- 50
    fly_zoom <- 12
    
    
    # --------------------------------------------------------------------------
    # Communication objects
    # --------------------------------------------------------------------------
    
    # -- declare names
    map_proxy <- paste0(id, "_proxy")
    map_click <- paste0(id, "_click")
    map_center <- paste0(id, "_center")
    map_bounds <- paste0(id, "_bounds")
    map_zoom <- paste0(id, "_zoom")
    
    # -- declare map connectors
    r[[map_proxy]] <- NULL
    r[[map_click]] <- NULL
    r[[map_center]] <- NULL
    r[[map_bounds]] <- NULL
    r[[map_zoom]] <- NULL
    
    # -- other connectors
    r$filter_country_choices <- NULL
    r$filter_country <- NULL
    
    # -- map triggers
    r$map_crop <- NULL
    r$map_flyto <- NULL
    
    
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
    # Trigger: map_crop
    # --------------------------------------------------------------------------
    # r$map_crop = list(lng_min, lat_min, lng_max, lat_max)
    
    # -- observe trigger
    observeEvent(r$map_crop, {
      
      # -- check setting
      req(!input$map_freeze)
      
      # -- crop view
      r[[map_proxy]] %>%
        flyToBounds(lng1 = r$map_crop$lng_min, 
                    lat1 = r$map_crop$lat_min, 
                    lng2 = r$map_crop$lng_max, 
                    lat2 = r$map_crop$lat_max, 
                    options = list(duration = fly_duration, 
                                   padding = c(fly_padding, fly_padding)))
      
    })
    
    
    # --------------------------------------------------------------------------
    # Trigger: map_flyto
    # --------------------------------------------------------------------------
    # r$map_flyto = list(lng, lat)
    
    # -- observe trigger
    observeEvent(r$map_flyto, {
      
      # -- check setting
      req(!input$map_freeze)
      
      # -- crop view
      r[[map_proxy]] %>%
        flyTo(lng = r$map_flyto$lng, 
              lat = r$map_flyto$lat,
              zoom = fly_zoom,
              options = list(duration = fly_duration, 
                             padding = c(fly_padding, fly_padding)))
      
      # -- unset trigger (otherwise you can't call again with same value)
      r$map_flyto <- NULL
      
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
    
    
    # --------------------------------------------------------------------------
    # Filters
    # --------------------------------------------------------------------------
    
    # -- Trigger: set country filter choices
    observeEvent(r$filter_country_choices(), {
      
      # -- update choices
      updateSelectizeInput(inputId = "filter_country", choices = r$filter_country_choices())
      
    })
    
    
    # -- Connector: country filter
    # init & reset = "" / test with if(nchar(r$filter_country) == 0)
    r$filter_country <- reactive(input$filter_country)
    
    
    # -- Observe: reset filter btn
    observeEvent(input$filter_country_reset, {
      
      cat(MODULE, "[TRIGGER] Reset filter country \n")
      
      # -- update filter
      updateSelectizeInput(inputId = "filter_country", selected = character(0))
      
    })
    
  })
}
