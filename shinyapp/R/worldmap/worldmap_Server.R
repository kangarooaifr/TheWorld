

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

worldmap_Server <- function(id, r, location_id, location_ns, map_id) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    
    # -- settings
    contextual_locations_level <- 8
    bus_stations_level <- 13
    railway_stations_level <- 10
    
    # -- items name
    r_location_items <- kitems::items_name(id = location_id)
    
    # -- cache
    cache_contextual <- reactiveVal(NULL)
    
    # -- declare input names
    filter_country <- paste0(id, "_country")
     
    r[[filter_country]] <- reactive(NULL)
    
    # -- map
    map_proxy <- paste0(map_id, "_proxy")
    map_bounds <- paste0(map_id, "_bounds")
    map_zoom <- paste0(map_id, "_zoom")
    
    
    # -- marker icons
    icons <- location_icons()

        
    # -------------------------------------
    # Connector: visited_countries
    # -------------------------------------
    
    # -- expose as reactive
    visited_countries <- reactive(
      unique(r[[r_location_items]]()[r[[r_location_items]]()$been.there, 'country']))
  
    
    # --------------------------------------------------------------------------
    # Country filter
    # --------------------------------------------------------------------------
    
    # -- Update country filter choices
    observe({

      # -- compute choices
      choices <- sort(unique(r[[r_location_items]]()$country))
      cat(MODULE, "Update country filter choices, nb =", length(choices), "\n")
      
      # -- update choices
      updateSelectizeInput(inputId = "filter_country", choices = choices)

    }) %>% bindEvent(unique(r[[r_location_items]]()$country))
    
    
    # --------------------------------------------------------------------------
    # Select locations
    # --------------------------------------------------------------------------
    
    # -- Level.1: observe location items
    locations <- reactive({
      
      cat(MODULE, "Update locations from items \n")
      
      # -- get locations (depending on selected option)
      x <- switch (input$display_options,
                   'been-there' = r[[r_location_items]]()[r[[r_location_items]]()$type == 'city' & r[[r_location_items]]()$been.there, ],
                   'wish-list'  = r[[r_location_items]]()[r[[r_location_items]]()$type == 'city' & r[[r_location_items]]()$wish.list, ],
                   r[[r_location_items]]()[r[[r_location_items]]()$type == 'city', ])
      
      # -- check
      cat("-- output dim =", dim(x)[1], "obs. \n")
    
      # -- return
      x
      
    })
    
    
    # -- Level.2: observe country filter
    filtered_locations <- reactive({
    
      # -- reset (skip) when filter is NULL
      if(is.null(input$filter_country))
        return(locations())
      
      cat(MODULE, "Apply country filter, value =", input$filter_country, "\n")
      
      # -- compute value
      x <- locations()[locations()$country %in% input$filter_country, ]
      cat("-- output dim =", dim(x)[1], "obs. \n")
    
      # -- return
      x
      
    })
    
    
    # -- display
    observe({
      
      locations <- filtered_locations()
      
      # -- remove markers
      clearGroup(r[[map_proxy]], group = 'city')
      
      # -- check dim #
      if(nrow(locations) != 0){
        
        # -- add icon & popup columns
        locations <- location_icon(locations)
        locations$popup <- location_popups(locations, type = 'selected', activity = 'world_map', ns = ns, location_ns = location_ns)
        
        # -- display on map
        add_markers(locations, map_proxy = r[[map_proxy]], icons = icons)
        
        # -- crop map around markers
        map_crop(map_proxy = r[[map_proxy]], 
                 lng1 = min(locations$lng), 
                 lat1 = min(locations$lat), 
                 lng2 = max(locations$lng),
                 lat2 = max(locations$lat), 
                 fly_duration, 
                 fly_padding)}
      
    }) %>% bindEvent(filtered_locations())
      
  
    # --------------------------------------------------------------------------
    # Contextual locations (observe map_bounds)
    # --------------------------------------------------------------------------
    
    observe({

      # -- check zoom level
      if(r[[map_zoom]]() >= contextual_locations_level){

        # -- init
        railway_stations <- NULL
        bus_stations <- NULL

        # -- check setting
        if(r[[map_zoom]]() >= railway_stations_level)
          railway_stations <- r$railway_stations

        # -- check setting
        if(r[[map_zoom]]() >= bus_stations_level)
          bus_stations <- r$bus_stations

        # -- get contextual locations
        locations <- contextual_locations(locations =  r[[r_location_items]](),
                                          airports = r$airports,
                                          railway_stations = railway_stations,
                                          bus_stations = bus_stations,
                                          bounds = r[[map_bounds]]())

        # -- Remove locations already in filtered_locations
        locations <- locations[!locations$id %in% filtered_locations()$id, ]
        
        # -- Extract locations to remove & add
        locations_to_remove <- cache_contextual()[!cache_contextual() %in% locations$id]
        locations_to_add <- locations[!locations$id %in% cache_contextual(), ]
        
        # -- store in cache
        cache_contextual(locations$id)
        
        # -- remove locations
        if(!identical(locations_to_remove, numeric(0))){
          
          cat(MODULE, "Remove markers from map, nb =", length(locations_to_remove), "\n")
          removeMarker(r[[map_proxy]], layerId = as.character(locations_to_remove))}
        
        # -- check dim
        if(nrow(locations_to_add) > 0){
          
          cat(MODULE, "Add markers on map, nb =", nrow(locations_to_add), "\n")
          
          # -- add icon & popup columns
          locations_to_add <- location_icon(locations_to_add)
          locations_to_add$popup <- location_popups(locations_to_add, type = 'selected', activity = 'world_map', ns = ns, location_ns = location_ns)
          
          # -- display on map
          add_markers(locations_to_add, map_proxy = r[[map_proxy]], icons = icons)}

      } else {
        
        if(length(cache_contextual()) > 0){
          
          cat(MODULE, "Clear contextual locations \n")
          removeMarker(r[[map_proxy]], layerId = as.character(cache_contextual()))
          cache_contextual(NULL)}
        
      }

    }) %>% bindEvent(r[[map_bounds]]())

    
    # -------------------------------------
    # Country area
    # -------------------------------------
    
    observeEvent({
      r$geojson_data
      visited_countries()
      r[[filter_country]]()}, {
        
        # -- because ignoreNULL = FALSE
        # need to wait for async data to be ready
        req(r$geojson_data)
        
        # -- get visited countries
        selected_countries <- visited_countries()
        
        # -- apply filter
        if(!is.null(r[[filter_country]]()))
          selected_countries <- selected_countries[selected_countries %in% r[[filter_country]]()]
        
        # -- switch to country code
        # WARNING! the column name is switched to X3digits.code upon reading the file
        selected_countries <- r$countries_iso[r$countries_iso$country.en %in% selected_countries, 'X3digits.code']
        
        # -- selected geojson to be displayed
        selected_geojson <- r$geojson_data[r$geojson_data@data$ISO_A3 %in% selected_countries, ]
        
        # -- update map
        r[[map_proxy]] %>%
          
          # -- cleanup
          clearGroup("countries") %>%
          
          # -- add areas
          addPolygons(data = selected_geojson, weight = 1, color = "red", group = "countries") %>%
        
          # -- Map overlay checkbox (hide / show groups)
          addLayersControl(overlayGroups = "countries")
        
        # -- update ui
        output$panel_ui <- renderUI({
          
          wellPanel(
            h4("Countries"),
            p("Show visited countries."))})
        
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    # -------------------------------------
    # track
    # -------------------------------------
    
    observe({
      
      # add to leaflet map
      r[[map_proxy]] %>%
        
        # -- hidden by default
        hideGroup('track') %>%
        
        # -- add on map
        addPolylines(data = r$track, group = 'track') %>%
        
        # -- Map overlay checkbox (hide / show groups)
        addLayersControl(overlayGroups = "track")
        
    }) %>% bindEvent(r$track)
    
    
  })
}
