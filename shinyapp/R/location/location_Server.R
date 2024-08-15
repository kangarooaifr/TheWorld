

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path, map_proxy, map_click, map_bounds, map_zoom) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- ids
    kitems_id <- "location"
    group_id <- "locations"
    
    # -- settings
    coord_digits <- 3
    contextual_locations_level <- 8
    bus_stations_level <- 13
    railway_stations_level <- 10
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_update <- kitems::trigger_update_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    
    
    # -- External connectors
    r$selected_locations <- NULL
    
    # -- Internal caches
    cache_groups <- reactiveVal(NULL)
    
    
    # -------------------------------------
    # Load resources & connector: airports
    # -------------------------------------
    
    # -- File name
    filename_airports <- "airports.csv"
    
    # -- colClasses
    colClasses_airports <- c(id = "numeric",
                             name = "character",
                             city = "character",
                             country = "character",
                             iata = "character",
                             icao = "character",
                             latitude = "numeric",
                             longitude = "numeric",
                             altitude = "numeric")
    
    # -- load data
    raw_airports <- kfiles::read_data(file = filename_airports,
                                    path = path$resources, 
                                    colClasses = colClasses_airports,
                                    create = FALSE)
    
    # -- rename columns to fit with convention & expose connector
    names(raw_airports)[names(raw_airports) == 'latitude'] <- 'lat'
    names(raw_airports)[names(raw_airports) == 'longitude'] <- 'lng'
    
    # -- filter out heliports & entries without iata code (value = "\\N")
    raw_airports <- raw_airports[raw_airports$iata != '\\N', ]
    raw_airports <- raw_airports[!grepl('Heli', raw_airports$name), ]
    cat("[location] Filter airports without iata code & heliports, output =", dim(raw_airports), "\n")
    
    # -- store & delete temp object
    r$airports <- raw_airports
    rm(raw_airports)
    
    
    # -------------------------------------
    # Load resources & connector: seaports
    # -------------------------------------
    # There is no specific file for seaports at this moment
    # it is taken from the standard locations with type = Port
    
    # -- expose connector
    r$seaports <- reactive(r[[r_items]]()[r[[r_items]]()$type == 'Port', ])
    
    
    # --------------------------------------------------------------------------
    # Load resources & connector: stations
    # --------------------------------------------------------------------------
    # stations.csv file is created out of helper function 
    # import_stations()
    
    # -- File name
    filename_stations <- "stations.csv"
    
    # -- colClasses
    colClasses_stations <- c(id = "numeric",
                             internal_id ="numeric",
                             name = "character",
                             slug = "character",
                             uic = "numeric",
                             lat = "numeric",
                             lng = "numeric",
                             parent_id = "numeric",
                             country = "character",
                             time_zone = "character",
                             is_airport = "logical",
                             iata = "character",
                             is_road = "logical",
                             is_rail = "logical")
    
    # -- read data
    stations <- kfiles::read_data(file = filename_stations,
                                  path = path$resources, 
                                  colClasses = colClasses_stations,
                                  create = FALSE)
    
    # -- filter & expose connector
    cat("[location] Filter stations without lng / lat:", sum(is.na(stations$lng)), "\n")
    r$stations <-  stations[!is.na(stations$lng), ]
    
    
    # -------------------------------------
    # [EVENT] Map click
    # -------------------------------------
    
    # -- Event: map click
    observeEvent(r[[map_click]](), {
      
      cat("[location] Map click event received \n")
      
      # -- get values
      lng <- r[[map_click]]()[['lng']]
      lat <- r[[map_click]]()[['lat']]
      
      # -- display popup
      r[[map_proxy]] %>% 
        clearPopups() %>%
        addPopups(lng, lat, 
                  paste("Longitude:", round(lng, digits = coord_digits), br(),
                        "Latitude:", round(lat, digits = coord_digits), 
                        hr(),
                        actionLink(inputId = ns("link_add"), 
                                   label =  "add to my locations", 
                                   icon = icon("plus"),
                                   onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', ns("add_to_locations")))))
    })
    
    
    # -- Event: popup link (add_to_locations)
    observeEvent(input$add_to_locations, {
    
      # -- modal
      showModal(modalDialog(
    
            p("Coordinates:"), 
            
            tags$ul(tags$li("long =", r[[map_click]]()[['lng']]), 
                    tags$li("lat =", r[[map_click]]()[['lat']])),
            
            # -- name
            textInput(inputId = ns("name"), 
                      label = "Name"),
            
            # -- type
            selectizeInput(inputId = ns("type"), 
                           label = "Type", 
                           choices = r[[r_items]]()$type, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = TRUE)),
            
            # -- country          
            selectizeInput(inputId = ns("country"), 
                           label = "Country", 
                           choices = r$countries_iso$country.en, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = FALSE)),
            
            # -- state / region
            selectizeInput(inputId = ns("state"), 
                           label = "State", 
                           choices = r[[r_items]]()$state, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = TRUE)),
            
            # -- zip code
            textInput(inputId = ns("zip.code"), 
                      label = "Zip code"),
            
            # -- city
            selectizeInput(inputId = ns("city"), 
                           label = "City", 
                           choices = r[[r_items]]()$city, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'),
                                          create = TRUE)),
            
            # -- address
            textInput(inputId = ns("address"), 
                      label = "Address"),
            
            # -- comment
            textInput(inputId = ns("comment"), 
                      label = "Comment"),
            
            # -- been.there
            checkboxInput(inputId = ns("been.there"), 
                          label = "Been there", 
                          value = FALSE),
            
            # -- wish.list
            checkboxInput(inputId = ns("wish.list"), 
                          label = "Wish list", 
                          value = FALSE),
            
            # -- title
            title = "Add to my locations",
            
            # -- actions
            footer = tagList(
              modalButton("Cancel"),
              actionButton(inputId = ns("confirm_add_location"), 
                           label = "Create"))))
      
    })
    

    # -- Event: btn confirm_add_location
    observeEvent(input$confirm_add_location, {

      # -- close dialog
      removeModal()
      
      # -- clear popup
      r[[map_proxy]] %>% 
        clearPopups()
      
      # -- build values
      input_values <- data.frame(id = NA,
                                 name = input$name,
                                 type = input$type,
                                 lng = r[[map_click]]()[['lng']],
                                 lat = r[[map_click]]()[['lat']],
                                 country = input$country,
                                 state = input$state,
                                 zip.code = input$zip.code,
                                 city = input$city,
                                 address = input$address,
                                 comment = input$comment,
                                 been.there = input$been.there,
                                 wish.list = input$wish.list)
      
      # -- create item
      item <- kitems::item_create(values = input_values, data.model = r[[r_data_model]]())
    
      # -- call trigger
      r[[r_trigger_add]](item)
      
    })

    
    
    
    
    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$hide_show, 
                 hide_show(proxy = r[[map_proxy]], id = group_id, show = input$hide_show), ignoreInit = TRUE)
    
    
    # -------------------------------------
    # Actions (click from marker popup)
    # -------------------------------------
    
    # -- Observe: action_update
    observeEvent(input$action_update, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_update, split = "_"))[2]
      cat("[EVENT] Marker popup click: update id =", id, "\n")
      
      # -- call trigger
      #r[[]](id)
      
    })
    
    
    # -- Observe: action_delete
    observeEvent(input$action_delete, {
    
      # -- extract id
      id <- unlist(strsplit(input$action_delete, split = "_"))[2]
      cat("[EVENT] Marker popup click: delete id =", id, "\n")
      
      # -- call trigger
      r[[r_trigger_delete]](id)
      
    })
    
    
    # -- Observe: action_beenthere
    observeEvent(input$action_beenthere, {
      
      # -- extract id
      id <- unlist(strsplit(input$action_beenthere, split = "_"))[2]
      cat("[EVENT] Marker popup click: been-there id =", id, "\n")
      
      # -- update item
      item <- r[[r_items]]()[r[[r_items]]()$id == id, ]
      item$been.there <- TRUE
      item$wish.list <- FALSE
      
      # -- call trigger
      r[[r_trigger_update]](item)
      
    })
    
    
    # -- Observe: add_to_trip
    observeEvent(input$add_to_trip, {
    
      # -- extract id
      id <- unlist(strsplit(input$add_to_trip, split = "_"))[2]
      cat("[EVENT] Marker popup click: add_to_trip id =", id, "\n")
      
      # -- get location
      location <- r[[r_items]]()[r[[r_items]]()$id == id, ]
      
      # -- call trigger
      r$trigger_add_step <- location
    
    })
    
    
    # -- Observe: remove_from_trip
    observeEvent(input$remove_from_trip, {
      
      # -- extract id
      id <- unlist(strsplit(input$remove_from_trip, split = "_"))[2]
      cat("[EVENT] Marker popup click: remove_from_trip id =", id, "\n")
      
      # -- call trigger
      r$trigger_remove_step <- id
      
    })

    
    # -------------------------------------
    # save for later
    # To dynamically switch from a tabItem to another:
    # updateTabItems(session, "inTabset", selected = "widgets")
    # -------------------------------------
    
    
    # -------------------------------------
    # Search
    # -------------------------------------
    
    # -- declare trigger
    r$location_search_string <- NULL
    
    
    # -- observe trigger & expose connector
    r$location_search_result <- eventReactive(r$location_search_string, {
      
      # -- check for empty string (otherwise the whole df is returned)
      if(identical(r$location_search_string, ""))
        NULL
      
      else {
        
        cat("[location] Trigger, search string =", r$location_search_string, "\n")
        
        # -- filter items
        result <- r[[r_items]]() %>%
          filter_all(any_vars(grepl(r$location_search_string, .)))
        
        # -- return
        result}
      
    })
    
    
    # -------------------------------------
    # Select
    # -------------------------------------
    
    # -- declare trigger
    r$location_select <- NULL
    
    # -- observe
    observeEvent(r$location_select, {
      
      cat("[TRIGGER] Select location: \n")
      
      # -- get items
      locations <- r[[r_items]]()
      
      # -- apply selection
      locations <- locations[locations$id %in% r$location_select, ]
      
      # -- check output dim
      if(dim(locations)[1] != length(r$location_select)){
        
        # -- check for airports
        airports <- r$airports[r$airports$id %in% r$location_select, ]
        
        # -- make locations from airports
        if(dim(airports)[1] > 0)
          tmp_locations <- airport_to_location(airports)
        
        # -- merge
        locations <- rbind(locations, tmp_locations)}
      
      # -- return
      cat("-- output dim =", dim(locations),"\n")
      r$selected_locations <- locations
      
    })
    
    
    # --------------------------------------------------------------------------
    # Contextual (temporary) locations
    # --------------------------------------------------------------------------
    # Note: 
    # groupOptions() is not compatible with addLayersControl()
    # zoom level driven behavior have to be done 'manually'
    
    # -- observe: bounds
    # Note: when zoom level is changed, bounds is updated (no need to observe)
    observeEvent(r[[map_bounds]](), {
      
      # -- check zoom level
      if(r[[map_zoom]]() >= contextual_locations_level){
        
        cat("[location] Update contextual locations \n")
        
        # -- init
        bounds <- r[[map_bounds]]()
        
        # -- get locations
        # -------------------------------------
        locations <- r[[r_items]]()
        locations <- bounding_box(locations, bounds)
        
        
        # -- get airports
        # -------------------------------------
        airports <- r$airports
        airports <- bounding_box(airports, bounds)
        
        # -- turn airports into locations & merge
        if(dim(airports)[1] > 0){
          
          airports <- airport_to_location(airports)
          locations <- rbind(locations, airports)}
        
        
        # -- get railway_stations
        # -------------------------------------
        # Note: only if zoom level > setting
        if(r[[map_zoom]]() >= railway_stations_level){
          
          railway_stations <- r$stations[!r$stations$is_rail %in% FALSE, ]
          railway_stations <- bounding_box(railway_stations, bounds)
          
          # -- turn railway stations into locations & merge
          if(dim(railway_stations)[1] > 0){
            
            railway_stations <- railway_to_location(railway_stations)
            locations <- rbind(locations, railway_stations)}}
        
        
        # -- get bus_stations
        # -------------------------------------
        # Note: only if zoom level > setting
        if(r[[map_zoom]]() >= bus_stations_level){
          
          bus_stations <- r$stations[r$stations$is_road %in% TRUE, ]
          bus_stations <- bounding_box(bus_stations, bounds)
          
          # -- turn bus stations into locations & merge
          if(dim(bus_stations)[1] > 0){
            
            bus_stations <- bus_to_location(bus_stations)
            locations <- rbind(locations, bus_stations)}}

        
        # -- End get locations
        # -------------------------------------
        
        # -- Remove locations already in r$selected_locations
        locations <- locations[!locations$id %in% r$selected_locations$id, ]
        
        # -- Check & add locations
        if(dim(locations)[1] > 0){
          
          # -- Add icon & popup columns
          locations <- location_icon(locations)
          locations$popup <- location_popups(locations, type = 'contextual', activity = 'world_map', ns)
          
          # -- Get groups
          groups <- unique(locations$type)
          
          # -- add markers
          r[[map_proxy]] %>%
            clearGroup(cache_groups()) %>%
            # -- Add markers
            addAwesomeMarkers(data = locations,
                              lng = ~lng,
                              lat = ~lat,
                              group = ~type,
                              icon = ~icons[icon],
                              label = ~name,
                              popup = ~popup,
                              clusterOptions = NULL) %>%
            
            # -- Map overlay checkbox (hide / show groups)
            addLayersControl(
              overlayGroups = groups)
          
          # -- store groups in cache
          cache_groups(groups)
          
        }
        
      } else {
        
        cat("[location] Clear contuextual locations \n")
        
        # -- clear markers
        r[[map_proxy]] %>%
          clearGroup(cache_groups())
        
      }
      
    }, ignoreInit = TRUE)
    
  })
}
