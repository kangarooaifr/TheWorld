

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

worldmap_Server <- function(id, map, locations, location_ns, r) {
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

    
    # --------------------------------------------------------------------------
    # Register observer (map_click)
    # --------------------------------------------------------------------------
    
    obs <- map_click_observer(map, onclick_id = ns("add_location"), coord_digits = setting("coord_digits"))
    
    
    # --------------------------------------------------------------------------
    # Add location
    # --------------------------------------------------------------------------
    
    # -- Event: popup link (add_location)
    observeEvent(input$add_location, {
      
      # -- get lng, lat
      lng <- map$click()[['lng']]
      lat <- map$click()[['lat']]
      
      # -- build choices
      choices <- list(type = unique(locations$items()$type),
                      country = r$countries_iso$country.en,
                      state = unique(locations$items()$state),
                      city = unique(locations$items()$city))
      
      # -- display form
      showModal(location_modal(location = NULL, lng, lat, choices, ns))
      
    })
    
    
    # -- Event: btn confirm_add_location
    observeEvent(input$confirm_add_location, {
      
      # -- secure against empty locations #161
      req(input$name, input$type, input$country, input$city)
      
      # -- close dialog
      removeModal()
      
      # -- clear popup
      map$proxy %>% 
        clearPopups()
      
      # -- build values
      input_values <- data.frame(id = NA,
                                 name = input$name,
                                 type = input$type,
                                 lng = map$click()[['lng']],
                                 lat = map$click()[['lat']],
                                 country = input$country,
                                 state = input$state,
                                 zip.code = input$zip.code,
                                 city = input$city,
                                 address = input$address,
                                 comment = input$comment,
                                 been.there = input$been.there,
                                 wish.list = input$wish.list)
      
      # -- create item
      item <- kitems::item_create(values = input_values, data.model = locations$data_model())
      
      # -- call trigger
      kitems::item_add(locations$items, item, name = locations$id)
      
    })
    
    
    # --------------------------------------------------------------------------
    # Connector: visited_countries
    # --------------------------------------------------------------------------
    
    # -- expose as reactive
    visited_countries <- reactive(
      unique(locations$items()[locations$items()$been.there, 'country']))
  
    
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
      x <- switch (input$display_options,
                   'been-there' = locations$items()[locations$items()$type == 'city' & locations$items()$been.there, ],
                   'wish-list'  = locations$items()[locations$items()$type == 'city' & locations$items()$wish.list, ],
                   locations$items()[locations$items()$type == 'city', ])
      
      # -- check
      cat("-- output dim =", dim(x)[1], "obs. \n")
    
      # -- return
      x
      
    })
    
    
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
        x$popup <- location_popups(x, type = 'selected', activity = 'world_map', ns = ns, location_ns = location_ns)
        
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
          locations_to_add$popup <- location_popups(locations_to_add, type = 'selected', activity = 'world_map', ns = ns, location_ns = location_ns)
          
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
    
    observeEvent({
      r$geojson_data
      visited_countries()
      input$filter_country}, {
        
        # -- because ignoreNULL = FALSE
        # need to wait for async data to be ready
        req(r$geojson_data)
        
        # -- get visited countries
        selected_countries <- visited_countries()
        
        # -- apply filter
        if(!is.null(input$filter_country))
          selected_countries <- selected_countries[selected_countries %in% input$filter_country]
        
        # -- switch to country code
        # WARNING! the column name is switched to X3digits.code upon reading the file
        selected_countries <- r$countries_iso[r$countries_iso$country.en %in% selected_countries, 'X3digits.code']
        
        # -- selected geojson to be displayed
        selected_geojson <- r$geojson_data[r$geojson_data@data$ISO_A3 %in% selected_countries, ]
        
        # -- update map
        map$proxy %>%
          
          # -- cleanup
          clearGroup("countries") %>%
          
          # -- add areas
          addPolygons(data = selected_geojson, weight = 1, color = "red", group = "countries")
        
        # -- Add in cache
        map_layers_control(map$layer_control, overlayGroups = "countries")
          
          
        # -- update ui
        output$panel_ui <- renderUI({
          
          wellPanel(
            h4("Countries"),
            p("Show visited countries."))})
        
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    # --------------------------------------------------------------------------
    # track
    # --------------------------------------------------------------------------
    
    observe({
      
      # add to leaflet map
      map$proxy %>%
        
        # -- hidden by default
        hideGroup('track') %>%
        
        # -- add on map
        addPolylines(data = r$track, group = 'track')
      
      # -- add in cache
      map_layers_control(map$layer_control, overlayGroups = "track")
      
        
    }) %>% bindEvent(r$track)
    
    
  })
}
