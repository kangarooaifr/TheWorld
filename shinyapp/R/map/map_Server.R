

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
    cat(MODULE, "Starting module server... \n")
    
    # -- fly to settings
    setting(name = "fly_duration", type = "numeric", default = 1.0)
    setting(name = "fly_padding", type = "numeric", default = 50)
    setting(name = "fly_zoom", type = "numeric", default = 12)
    
    
    # --------------------------------------------------------------------------
    # Names
    # --------------------------------------------------------------------------
    
    # -- declare names
    # map_proxy <- paste0(id, "_proxy")
    # map_click <- paste0(id, "_click")
    # map_center <- paste0(id, "_center")
    # map_bounds <- paste0(id, "_bounds")
    # map_zoom <- paste0(id, "_zoom")
    map_flyto <- paste0(id, "_flyto")
    
    
    # --------------------------------------------------------------------------
    # Communication objects
    # --------------------------------------------------------------------------
    
    # -- declare connectors
    map_proxy <- NULL
    map_click <- NULL
    map_center <- NULL
    map_bounds <- NULL
    map_zoom <- NULL
    
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
    
    
    # --------------------------------------------------------------------------
    # Map observers & connectors
    # --------------------------------------------------------------------------
    
    # -- Connector: map proxy
    map_proxy <- leafletProxy('map')
    
    
    # -- Connector: mouse click
    map_click <- reactive({

      # -- check
      req(input$map_click)
      
      # -- trace
      if(verbose)
        cat(MODULE, "Point clicked: lng =", input$map_click$lng, "/ lat =", input$map_click$lat, "\n")
      
      # -- return
      input$map_click

    })
    
    
    # -- Connector: map center
    map_center <- reactive({
      
      # -- check
      req(input$map_center)
      
      # -- trace
      if(verbose)
        cat(MODULE, "Center: lng =", input$map_center$lng, "/ lat =", input$map_center$lat, "\n")
      
      # -- return
      input$map_center
      
    })
    
    
    # -- Connector: map bounds
    map_bounds <- reactive({
      
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
    map_zoom <- reactive({
      
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
      map_proxy %>%
        flyTo(lng = r[[map_flyto]]$lng, 
              lat = r[[map_flyto]]$lat,
              zoom = setting("fly_zoom"),
              optionss = list(duration = setting("fly_duration"), 
                             padding = c(setting("fly_padding"), setting("fly_padding"))))
      
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
    
    
    # --------------------------------------------------------------------------
    # LAB
    # --------------------------------------------------------------------------
    
    # -- define trigger
    myTrigger <- reactiveVal(NULL)
    
    # -- observe trigger
    observeEvent(myTrigger(), {
      
      cat("[LAB] -- myTrigger has been updated !!!", myTrigger(), "\n")
      
    })
    
    # -- return
    list(id = id,
         proxy = map_proxy,
         click = map_click,
         center = map_center,
         bounds = map_bounds,
         zoom = map_zoom,
         trigger = myTrigger)
    

  })
}
