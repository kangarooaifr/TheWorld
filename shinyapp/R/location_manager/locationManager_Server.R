

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

locationManager_Server <- function(id, map, locations, countries, onSelect, popups) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- get namespace
    ns <- session$ns
    
    
    # --------------------------------------------------------------------------
    # Init
    # --------------------------------------------------------------------------
    
    # -- marker icons
    icons <- location_icons()
    
    
    # --------------------------------------------------------------------------
    # Map Click Popup
    # --------------------------------------------------------------------------

    # -- observe
    observeEvent(map$click(), {
      
      cat(paste0("[", map$id, "]"), "Map click event received \n")
      
      # -- get values
      lng <- map$click()[['lng']]
      lat <- map$click()[['lat']]
      
      # -- get address info from coordinates
      address <- reverse_geocoding(lng, lat)
      
      # -- display popup
      map$proxy %>% 
        clearPopups() %>%
        addPopups(lng, lat, 
                  paste("Longitude:", round(lng, digits = setting("coord_digits")), br(),
                        "Latitude:", round(lat, digits = setting("coord_digits")), 
                        hr(),
                        "address:", address$display_name,
                        hr(),
                        actionLink(inputId = map$id, 
                                   label =  "add to my locations", 
                                   icon = icon("plus"),
                                   onclick = sprintf(
                                     'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                     ns("action_add")))))
    })
    
    
    # --------------------------------------------------------------------------
    # Add location
    # --------------------------------------------------------------------------
    
    # -- build location choices
    choices <- reactive(list(type = unique(locations$items()$type),
                             country = countries$iso$country.en,
                             state = unique(locations$items()$state),
                             city = unique(locations$items()$city)))
    
    
    # -- Event: actionLink click
    observeEvent(input$action_add, {
      
      cat(paste0("[", map$id, "]"), "Marker popup click / add location \n")
      
      # -- get lng, lat
      lng <- map$click()[['lng']]
      lat <- map$click()[['lat']]
      
      # -- display form
      showModal(location_modal(location = NULL, lng, lat, choices(), ns))
      
    })
    
    
    # -- Event: actionButton click
    observeEvent(input$confirm_add_location, {
      
      # -- secure against empty locations #161
      req(input$name, input$type, input$country, input$city)
      
      # -- close dialog
      cat(paste0("[", map$id, "]"), "Confirm add location \n")
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
    # Update location
    # --------------------------------------------------------------------------
    
    # -- Event: actionLink click
    observeEvent(input$action_update, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_update, split = "_"))[2]
      cat(paste0("[", map$id, "]"), "Marker popup click: update id =", id, "\n")
      
      # -- get location to update
      location <- locations$items()[locations$items()$id == id, ]
      
      # -- display form
      showModal(location_modal(location, choices = choices(), ns = ns))
      
    })
    
    
    # -- Event: actionButton click
    observeEvent(input$confirm_update_location, {
      
      # -- close dialog
      cat(paste0("[", map$id, "]"), "Confirm add location \n")
      removeModal()
      
      # -- extract location id
      id <- unlist(strsplit(input$action_update, split = "_"))[2]
      
      # -- get location to update
      location <- locations$items()[locations$items()$id == id, ]
      
      # -- update values
      location$name = input$name
      location$type = input$type
      location$country = input$country
      location$state = input$state
      location$zip.code = input$zip.code
      location$city = input$city
      location$address = input$address
      location$comment = input$comment
      location$been.there = input$been.there
      location$wish.list = input$wish.list
      
      # -- update location
      kitems::item_update(locations$items, location, name = locations$id)
      
    })
    

    # --------------------------------------------------------------------------
    # Delete location
    # --------------------------------------------------------------------------
    
    # -- Event: actionLink click
    observeEvent(input$action_delete, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_delete, split = "_"))[2]
      cat(paste0("[", map$id, "]"), "Marker popup click: delete id =", id, "\n")
      
      # -- delete item
      kitems::item_delete(locations$items, id, name = "location")
      
    })
    
    
    # --------------------------------------------------------------------------
    # Display locations
    # --------------------------------------------------------------------------
    
    # -- Event: onSelect callback
    observe({
      
      # -- get value from callback function
      x <- onSelect()
      
      # -- remove markers
      clearGroup(map$proxy, group = 'city')
      
      # -- check dim
      if(nrow(x) != 0){
        
        # -- add icon & popup columns
        x <- location_icon(x)
        x$popup <- location_popups(x, ns, activity = popups())
        
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
      
    }) %>% bindEvent(onSelect())
    
    
    # --------------------------------------------------------------------------
    # Contextual locations
    # --------------------------------------------------------------------------
    
    # -- call module server
    contextualLocation_Server(id = paste0(id, "_ctx"), map, locations, exclude = onSelect, icons)
        
    
  })
}
