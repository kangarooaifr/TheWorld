

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
    setting(name = "bus_stations_level", type = "numeric", default = 13)
    setting(name = "railway_stations_level", type = "numeric", default = 10)

    
    # --------------------------------------------------------------------------
    # Init
    # --------------------------------------------------------------------------
    
    # -- cache
    cache_contextual <- reactiveVal(NULL)
    
    # -- marker icons
    icons <- location_icons()

    # -- build choices
    choices <- reactive(list(type = unique(locations$items()$type),
                             country = countries$iso$country.en,
                             state = unique(locations$items()$state),
                             city = unique(locations$items()$city)))
    
    
    # --------------------------------------------------------------------------
    # Register observers
    # --------------------------------------------------------------------------
    
    # -- map_click
    map_click <- map_click_observer(map, ns, coord_digits = setting("coord_digits"))
  
    # -- action_add
    action_add <- location_add_observer(map, input, choices, ns)
    
    # -- confirm_add
    confirm_add <- location_confirm_add_observer(map, input, locations)

    # -- action_update
    action_update <- location_update_observer(mapId = map$id, input, locations, choices, ns)
    
    # -- confirm_update
    confirm_update <- location_confirm_update_observer(mapId = map$id, input, locations)
    
    # -- action_delete
    action_delete <- location_delete_observer(mapId = map$id, input, locations)
    
    # -- action_beenthere
    action_beenthere <- action_beenthere_observer(mapId = map$id, input, locations)
  
    
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
    # Display locations
    # --------------------------------------------------------------------------
    
    # -- Event: filtered_locations
    observe({
      
      x <- filtered_locations()
      
      # -- remove markers
      clearGroup(map$proxy, group = 'city')
      
      # -- check dim #
      if(nrow(x) != 0){
        
        # -- add icon & popup columns
        x <- location_icon(x)
        x$popup <- location_popups(x, type = 'selected', activity = 'world_map', ns)
        
        # -- display on map
        add_markers(x, map_proxy = map$proxy, icons = icons)
        
        # -- Add in cache
        map_layers_control(map$layer_control, overlayGroups = unique(x$type))
        
        # -- crop map around markers
        map_crop(map_proxy = map$proxy, 
                 lng1 = min(x$lng), 
                 lat1 = min(x$lat), 
                 lng2 = max(x$lng),
                 lat2 = max(x$lat), 
                 fly_duration = setting(name = "fly_duration"),
                 fly_padding = setting(name = "fly_padding"))}
       
    }) %>% bindEvent(filtered_locations())
      
  
    # --------------------------------------------------------------------------
    # Contextual locations (observe map_bounds)
    # --------------------------------------------------------------------------
    
    observe({

      # -- check zoom level
      if(map$zoom() >= setting("contextual_locations_level")){

        # -- init
        railway_stations <- NULL
        bus_stations <- NULL

        # -- check setting
        if(map$zoom() >= setting("railway_stations_level"))
          railway_stations <- locations$railway_stations

        # -- check setting
        if(map$zoom() >= setting("bus_stations_level"))
          bus_stations <- locations$bus_stations

        # -- get contextual locations
        x <- contextual_locations(locations =  locations$items(),
                                  airports = locations$airports,
                                  railway_stations = railway_stations,
                                  bus_stations = bus_stations,
                                  bounds = map$bounds())
        
        # -- Remove locations already in filtered_locations
        x <- x[!x$id %in% filtered_locations()$id, ]
        
        # -- Extract locations to remove & add
        locations_to_remove <- cache_contextual()[!cache_contextual()$id %in% x$id, ]$id
        locations_to_add <- x[!x$id %in% cache_contextual()$id, ]
        
        # -- Extract groups to remove & add
        groups_to_add <- unique(locations_to_add$type)
        groups_in_cache <- unique(cache_contextual()[!cache_contextual()$id %in% x$id, ]$type)
        groups_to_remove <- groups_in_cache[!groups_in_cache %in% x$type]
        
        # -- store in cache (df)
        cache_contextual(x[c("id", "type")])
        
        # -- remove locations
        if(!identical(locations_to_remove, numeric(0)) & !is.null(locations_to_remove)){
          
          cat(MODULE, "Remove markers from map, nb =", length(locations_to_remove), "\n")
          removeMarker(map$proxy, layerId = as.character(locations_to_remove))
          
          # -- Remove from cache
          map_layers_control(map$layer_control, overlayGroups = groups_to_remove, remove = TRUE)}
        
        # -- check dim
        if(nrow(locations_to_add) > 0){
          
          cat(MODULE, "Add markers on map, nb =", nrow(locations_to_add), "\n")
          
          # -- add icon & popup columns
          locations_to_add <- location_icon(locations_to_add)
          locations_to_add$popup <- location_popups(locations_to_add, type = 'selected', activity = 'world_map', ns)
          
          # -- display on map
          add_markers(locations_to_add, map_proxy = map$proxy, icons = icons)
          
          # -- Add in cache
          map_layers_control(map$layer_control, overlayGroups = groups_to_add)}

      } else {
        
        if(!is.null(cache_contextual()))
          if(nrow(cache_contextual()) > 0){
            
            # -- Clear markers
            cat(MODULE, "Clear contextual locations \n")
            removeMarker(map$proxy, layerId = as.character(cache_contextual()$id))
            
            # -- Remove from cache ('city' should be kept)
            groups <- unique(cache_contextual()$type)
            groups <- groups[!groups %in% filtered_locations()$type]
            map_layers_control(map$layer_control, overlayGroups = groups, remove = TRUE)
            
            # -- Reset cache
            cache_contextual(NULL)}
        
      }

    }) %>% bindEvent(map$bounds())

    
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
        addPolygons(data = selected_geojson(), weight = 1, color = "red", group = "countries")
      
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
