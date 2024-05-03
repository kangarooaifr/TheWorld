

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- ids
    kitems_id <- "location"
    group_id <- "locations"
    
    # -- settings
    coord_digits <- 3
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_update <- kitems::trigger_update_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    
    
    # -- Internal connectors
    selected_locations <- reactiveVal(NULL)
    
    
    # -- icon set
    icons <- location_icons()
    
    
    # -------------------------------------
    # Load resources & connector: airports
    # -------------------------------------
    
    # -- File name
    filename <- "airports.csv"
    
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
    content_df <- kfiles::read_data(file = filename,
                                    path = path$resources, 
                                    colClasses = colClasses_airports,
                                    create = FALSE)
    
    # -- rename columns to fit with convention & expose connector
    names(content_df)[names(content_df) == 'latitude'] <- 'lat'
    names(content_df)[names(content_df) == 'longitude'] <- 'lng'
    r$airports <- content_df
    
    
    # -------------------------------------
    # Load resources & connector: seaports
    # -------------------------------------
    # There is no specific file for seaports at this moment
    # it is taken from the standard locations with type = Port
    
    # -- expose connector
    r$seaports <- reactive(r[[r_items]]()[r[[r_items]]()$type == 'Port', ])
    
    
    # -------------------------------------
    # Connector: visited_countries
    # -------------------------------------
    
    # -- expose as reactive
    r$visited_countries <- reactive(
      unique(r[[r_items]]()[r[[r_items]]()$been.there, 'country']))
    
    
    # -------------------------------------
    # Filter: country (choices)
    # -------------------------------------
    
    # -- Update filter choices
    r$filter_country_choices <- reactive(sort(unique(r[[r_items]]()$country)))
    
    
    # -------------------------------------
    # [EVENT] Map click
    # -------------------------------------
    
    # -- Event: map click
    observeEvent(r$map_click(), {
      
      cat("[location] Map click event received \n")
      
      # -- get values
      lng <- r$map_click()[['lng']]
      lat <- r$map_click()[['lat']]
      
      # -- display popup
      r$proxymap %>% 
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
            
            tags$ul(tags$li("long =", r$map_click()[['lng']]), 
                    tags$li("lat =", r$map_click()[['lat']])),
            
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
      r$proxymap %>% 
        clearPopups()
      
      # -- build values
      input_values <- data.frame(id = NA,
                                 name = input$name,
                                 type = input$type,
                                 lng = r$map_click()[['lng']],
                                 lat = r$map_click()[['lat']],
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
    # Activity: world map
    # -------------------------------------
    
    # -- Observe: items, activity, filter (country)
    # define selected_locations
    obsA <- observe({
      
      # -- Run only when activity = world_map
      req(r$activity() == "world_map")
      
      # -- init
      cat("[location] World map, select locations: \n")
      locations <- r[[r_items]]()
      
      # -- filter by country
      if(!is.null(r$filter_country())){
        locations <- locations[locations$country %in% r$filter_country(), ]
        cat("-- apply country filter, value =", r$filter_country(), "\n")}

      # -- store
      cat("-- selected locations, output dim =", dim(locations)[1], "obs. \n")
      selected_locations(locations)
      
    })

    
    # -------------------------------------
    
    # -- Observe: display button
    observeEvent(selected_locations(), {
      
      # -- init
      locations <- selected_locations()

      # -- check dim
      if(dim(locations)[1] > 0){
        
        # -- add icon column
        locations <- location_icon(locations)
        
        # -- update map (proxy)
        r$proxymap %>%
          
          # -- cleanup
          clearGroup(group_id) %>%
          
          # -- Add markers
          addAwesomeMarkers(data = locations,
                            lng = ~lng,
                            lat = ~lat,
                            group = group_id,
                            icon = ~icons[icon],
                            label = ~name,
                            popup = ~sprintf(
                              paste0(
                                "Name:", name,
                                br(),
                                "lng = ", lng,
                                br(),
                                "lat = ", lat,
                                br(),
                                actionLink(inputId = "update_%s", 
                                           label =  "Update", 
                                           onclick = sprintf(
                                             'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                             ns("action_update"))),
                                actionLink(inputId = "delete_%s", 
                                           label =  "Delete", 
                                           onclick = sprintf(
                                             'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                             ns("action_delete"))),
                                actionLink(inputId = "been-there_%s", 
                                           label =  "Been there", 
                                           onclick = sprintf(
                                             'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                             ns("action_beenthere")))), id, id, id),
                            #clusterOptions = markerClusterOptions(),
                            clusterOptions = NULL)
        
        # -- call trigger (fit map to bounding box)
        r$map_crop <- list(lng_min = min(locations$lng), 
                           lat_min = min(locations$lat),
                           lng_max = max(locations$lng), 
                           lat_max = max(locations$lat))
        
      }
      
    }, ignoreNULL = FALSE)
    
    
    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$hide_show, 
                 hide_show(proxy = r$proxymap, id = group_id, show = input$hide_show), ignoreInit = TRUE)
    
    
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
      selected_locations(locations)
      
    })
    
    
    # -------------------------------------
    # Temporary locations
    # -------------------------------------
    
    # -- observe: zoom, bounds
    observeEvent({
      r$map_zoom()
      r$map_bounds()}, {
      
        cat("[location] Map zoom / bounds event received \n")
        
        # -------------------------------------
        # Set the detail group to only appear when zoomed in
        #groupOptions("detail", zoomLevels = 7:18)
        # -------------------------------------
        # >>> rework block !!
        
      # -- check zoom value
      if(r$map_zoom() >= 8){
        
        cat("[location] Add temporary locations \n")
        
        # -- init
        airports <- r$airports
        locations <- r[[r_items]]()
        bounds <- r$map_bounds()
        
        # -- filter by bounding box
        airports <- bounding_box(airports, bounds)
        locations <- bounding_box(locations, bounds)
        
        # -- turn airports into locations & merge
        if(dim(airports)[1] > 0){
          
          
          airports <- airport_to_location(airports)
          locations <- rbind(locations, airports)
          
          # -- remove already selected locations
          
          }
        
        # -- check
        if(dim(locations)[1] > 0){
          
          # -- add icon column
          locations <- location_icon(locations)
          
          # -- add markers
          r$proxymap %>%
            clearGroup("temp") %>%
            # -- Add markers
            addAwesomeMarkers(data = locations,
                              lng = ~lng,
                              lat = ~lat,
                              group = "temp",
                              icon = ~icons[icon],
                              label = ~name,
                              popup = ~sprintf(
                                paste0(
                                  "Name:", name,
                                  br(),
                                  "lng = ", lng,
                                  br(),
                                  "lat = ", lat,
                                  br(),
                                  actionLink(inputId = "add_%s", 
                                             label =  "Add to trip", 
                                             onclick = sprintf(
                                               'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                               ns("add_to_trip")))), id),
                              clusterOptions = NULL)}
        
      } else if(r$map_zoom() <= 7){
        
        cat("[location] Clear temporary locations \n")
        
        r$proxymap %>%
          clearGroup("temp")
        
      }
        
    }, ignoreInit = TRUE)
    
  })
}
