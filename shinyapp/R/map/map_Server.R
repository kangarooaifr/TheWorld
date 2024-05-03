

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

map_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- trace level
    verbose <- TRUE
    
    
    # --------------------------------------------------------------------------
    # Communication objects
    # --------------------------------------------------------------------------
    
    # -- declare map connectors
    r$proxymap <- NULL
    r$map_click <- NULL
    r$map_center <- NULL
    r$map_bounds <- NULL
    r$map_zoom <- NULL
    r$map_freeze <- NULL
    
    # -- other connectors
    r$filter_country_choices <- NULL
    r$filter_country <- NULL
    
    
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
    # Map observers & connectors
    # --------------------------------------------------------------------------
    
    # -- Connector: mouse click
    r$map_click <- reactive({

      # -- check
      req(input$map_click)
      
      # -- trace
      if(verbose)
        cat("[map] Point clicked: lng =", input$map_click$lng, "/ lat =", input$map_click$lat, "\n")
      
      # -- return
      input$map_click

    })
    
    
    # -- Connector: map center
    r$map_center <- reactive({
      
      # -- check
      req(input$map_center)
      
      # -- trace
      if(verbose)
        cat("[map] Center: lng =", input$map_center$lng, "/ lat =", input$map_center$lat, "\n")
      
      # -- return
      input$map_center
      
    })
    
    
    # -- Connector: map bounds
    r$map_bounds <- reactive({
      
      # -- check
      req(input$map_bounds)
      
      # -- trace
      if(verbose)
        cat("[map] Bounds: north =", input$map_bounds$north, "/ east =", input$map_bounds$east, 
            "/ south =", input$map_bounds$south, "\ west =", input$map_bounds$west, "\n")
      
      # -- return
      input$map_bounds
      
    })
    
    
    # -- Connector: map zoom
    r$map_zoom <- reactive({
      
      # -- check
      req(input$map_zoom)
      
      # -- trace
      if(verbose)
        cat("[map] Zoom: level =", input$map_zoom, "\n")
      
      # -- return
      input$map_zoom
      
    })
    
    
    # -- Connector: map freeze
    r$map_freeze <- reactive(input$map_freeze)
    
    
    # --------------------------------------------------------------------------
    # Search
    # --------------------------------------------------------------------------
    
    # -- Observe: search input
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
    
    # -- Trigger: set country filter choices
    observeEvent(r$filter_country_choices(), {
      
      # -- update choices
      updateSelectizeInput(inputId = "filter_country", choices = r$filter_country_choices())
      
    })
    
    
    # -- Connector: country filter
    r$filter_country <- reactive({
      
      # -- init & reset = "" / test with if(nchar(r$filter_country) == 0)
      cat("[EVENT] Filter country =", input$filter_country, "\n")
      input$filter_country
      
    })
    
    
    # -- Observe: reset filter btn
    observeEvent(input$filter_country_reset, {
      
      cat("[EVENT] Reset filter country \n")
      
      # -- update filter
      updateSelectizeInput(inputId = "filter_country", selected = character(0))
      
    })
    
  })
}
