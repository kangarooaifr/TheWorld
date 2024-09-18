

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

worldmap_Server <- function(id, map, locations, countries, tracks) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- get namespace
    ns <- session$ns
    
    # -- general settings
    setting(name = "coord_digits", type = "numeric", default = 3)
    
    # -- contextual locations settings
    setting(name = "contextual_locations_level", type = "numeric", default = 8)
    setting(name = "airports_level", type = "numeric", default = 9)
    setting(name = "bus_stations_level", type = "numeric", default = 13)
    setting(name = "railway_stations_level", type = "numeric", default = 10)

    
    # --------------------------------------------------------------------------
    # Init
    # --------------------------------------------------------------------------

    # -- init
    filtered_locations <- reactiveVal()

    # -- fill colors
    color_palette <- colorNumeric(palette = "YlOrBr", domain = c(0, 10))
    
    
    # --------------------------------------------------------------------------
    # Location manager
    # --------------------------------------------------------------------------

    # -- call module
    locationManager_Server(id = paste0(id, "_lm"), map, locations, countries,
                           onSelect = filtered_locations, popups = wm_popups,
                           ns.callback = ns)
    
    
    # --------------------------------------------------------------------------
    # Register observers
    # --------------------------------------------------------------------------
    
    # -- Observe: actionLink
    observeEvent(input$action_beenthere, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_beenthere, split = "_"))[2]
      cat(paste0("[", map$id, "]"), "Marker popup click: been-there id =", id, "\n")
      
      # -- update item
      location <- locations$items()[locations$items()$id == id, ]
      location$been.there <- TRUE
      location$wish.list <- FALSE
      
      # -- update location
      kitems::item_update(locations$items, location, name = locations$id)
      
    })
    
    
    # -- Observe: actionLink
    observeEvent(input$action_wishlist, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_wishlist, split = "_"))[2]
      cat(paste0("[", map$id, "]"), "Marker popup click: wish-list id =", id, "\n")
      
      # -- update item
      location <- locations$items()[locations$items()$id == id, ]
      location$wish.list <- TRUE
      
      # -- update location
      kitems::item_update(locations$items, location, name = locations$id)
      
    })
  
    
    # --------------------------------------------------------------------------
    # Country filter
    # --------------------------------------------------------------------------
    
    # -- Update country filter choices
    observe({

      # -- compute choices
      choices <- sort(unique(locations$items()$country))
      cat(MODULE, "Update country filter choices, nb =", length(choices), "\n")
      
      # -- update choices
      updateSelectizeInput(inputId = "filter_country", choices = choices)

    }) %>% bindEvent(unique(locations$items()$country))
    
    
    # --------------------------------------------------------------------------
    # Select locations
    # --------------------------------------------------------------------------
    
    # -- Level.1: observe location items
    selected_locations <- reactive({
      
      cat(MODULE, "Update locations from items \n")
      
      # -- get locations (depending on selected option)
      switch (input$display_options,
              'been-there' = select_locations(locations, 
                                              pattern = list(type = "city", been.there = TRUE), 
                                              result = "locations"),
              'wish-list'  = select_locations(locations, 
                                              pattern = list(type = "city", wish.list = TRUE), 
                                              result = "locations"),
              select_locations(locations, 
                               pattern = list(type = "city"), 
                               result = "locations"))
      
    })
    
    
    # --------------------------------------------------------------------------
    # Filter locations
    # --------------------------------------------------------------------------
    
    # -- Level.2: observe country filter
    filtered_locations <- reactive({
    
      # -- get data
      x <- selected_locations()
      
      # -- reset (skip) when filter is NULL
      if(is.null(input$filter_country))
        return(x)
      
      cat(MODULE, "Apply country filter, value =", input$filter_country, "\n")
      
      # -- compute value
      x <- x[x$country %in% input$filter_country, ]
      cat("-- output dim =", dim(x)[1], "obs. \n")
    
      # -- return
      x
      
    })
      
  
    # --------------------------------------------------------------------------
    # Country area
    # --------------------------------------------------------------------------
    
    # -- Visited countries
    visited_countries <- reactive(
      unique(select_locations(locations, 
                              pattern = list(been.there = TRUE), 
                              result = "locations")$country))
    
    
    # -- Level.1: select countries
    selected_countries <- reactive({
      
      cat(MODULE, "Select countries \n")
      
      # -- init
      x <- visited_countries()
      
      # -- apply filter
      if(!is.null(input$filter_country))
        x <- x[x %in% input$filter_country]
      
      # -- switch to country code
      # WARNING! the column name is switched to X3digits.code upon reading the file
      x <- countries$iso[countries$iso$country.en %in% x, 'X3digits.code']
      
    })
    
    
    # -- Level.2: select geojson
    selected_geojson <- reactive({
      
      # -- need to wait for async data to be ready!
      req(countries$geojson())
      cat(MODULE, "Select geojson data \n")
      
      # -- selected geojson to be displayed
      countries$geojson()[countries$geojson()@data$ISO_A3 %in% selected_countries(), ]
      
    })
    
    
    # -- Level.3: display data
    observe({
      
      cat(MODULE, "Update map (polygons) \n")
      
      # -- update map
      map$proxy %>%
        
        # -- cleanup
        clearGroup("countries") %>%
        
        # -- add areas
        addPolygons(data = selected_geojson(), weight = 1, color = color_palette(9), group = "countries")
      
      # -- Add in cache
      map_layers_control(map$layer_control, overlayGroups = "countries")
      
      
    }) %>% bindEvent(selected_geojson())
    
    
    # --------------------------------------------------------------------------
    # track
    # --------------------------------------------------------------------------
    
    observe({
      
      # add to leaflet map
      map$proxy %>%
        
        # -- hidden by default
        hideGroup('track') %>%
        
        # -- add on map
        addPolylines(data = tracks, group = 'track')
      
      # -- add in cache
      map_layers_control(map$layer_control, overlayGroups = "track")
      
        
    }) %>% bindEvent(tracks)
    
    
  })
}
