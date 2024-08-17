

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

worldmap_Server <- function(id, r, location_id, map_proxy) {
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
    
    
    # -- declare input names
    filter_country <- paste0(id, "_country")
     
    r[[filter_country]] <- reactive(NULL)
    
    # -- map
    map_bounds <- paste0("world", "_bounds")
    map_zoom <- paste0("world", "_zoom")
    
    
    # -- marker icons
    icons <- location_icons()

        
    # -------------------------------------
    # Connector: visited_countries
    # -------------------------------------
    
    # -- expose as reactive
    # r$visited_countries <- reactive(
    #   unique(r[[r_location_items]]()[r[[r_location_items]]()$been.there, 'country']))
  
    
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

      # -- compute value
      x <- r[[r_location_items]]()[r[[r_location_items]]()$type == 'city', ]
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
      
      # -- add icon & popup columns
      locations <- location_icon(locations)
      locations$popup <- location_popups(locations, type = 'selected', activity = 'world_map', ns = ns)
      
      # -- display on map
      add_markers(locations, map_proxy = r[[map_proxy]], group_id = "cities", icons = icons)
      
      # -- crop map around markers
      map_crop(map_proxy = r[[map_proxy]], 
               lng1 = min(locations$lng), 
               lat1 = min(locations$lat), 
               lng2 = max(locations$lng),
               lat2 = max(locations$lat), 
               fly_duration, 
               fly_padding)
      
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
        
        # -- check dim
        if(nrow(locations) > 0){
          
          # -- add icon & popup columns
          locations <- location_icon(locations)
          locations$popup <- location_popups(locations, type = 'selected', activity = 'world_map', ns = ns)
          
          # -- display on map
          add_markers(locations, map_proxy = r[[map_proxy]], group_id = "context", icons = icons)}

      } else {

        r[[map_proxy]] %>%
          clearGroup("context")

      }

    }) %>% bindEvent(r[[map_bounds]]())

    
  })
}
