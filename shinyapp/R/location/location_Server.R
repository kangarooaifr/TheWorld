

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
    fly_duration <- 1.0
    coord_digits <- 3
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_update <- kitems::trigger_update_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    
    # -- icon set
    icons <- awesomeIconList(been.there = makeAwesomeIcon(icon = 'location-dot', iconColor = 'black', library = 'fa', markerColor = 'beige'),
                             wish.list = makeAwesomeIcon(icon = 'location-dot', iconColor = 'black', library = 'fa', markerColor = 'pink'),
                             bed = makeAwesomeIcon(icon = 'bed', iconColor = 'black', library = 'fa', markerColor = 'beige'),
                             port = makeAwesomeIcon(icon = 'anchor', iconColor = 'black', library = 'fa', markerColor = 'blue'),
                             default = makeAwesomeIcon(icon = 'location-dot', iconColor = 'black', library = 'fa', markerColor = 'grey'))
    
    
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
    
    # -- load & expose connector
    r$airports <- kfiles::read_data(file = filename,
                                    path = path$resources, 
                                    colClasses = colClasses_airports,
                                    create = FALSE)
    
    
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
    # Filter: country >>> to be moved to country module!
    # -------------------------------------
    
    # -- update filter choices
    observeEvent(r[[r_items]](), {
      
      locations <- r[[r_items]]()
      choices <- unique(locations$country)
      
      r$filter_country_choices <- choices
      
    })
    
    
    # -------------------------------------
    # [EVENT] Map click
    # -------------------------------------
    
    # -- Event: map click
    observeEvent(r$map_click, {
      
      # -- get values
      lng <- r$map_click[['lng']]
      lat <- r$map_click[['lat']]
      
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
            
            tags$ul(tags$li("long =", r$map_click[['lng']]), 
                    tags$li("lat =", r$map_click[['lat']])),
            
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
                                 lng = r$map_click[['lng']],
                                 lat = r$map_click[['lat']],
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
    
    # -- Observe: display button
    # dependencies on display options, items and filter
    observeEvent({
      input$display_options
      r[[r_items]]()
      r$activity
      selected_location()
      r$filter_country
    }, {
      
      
      if(r$activity == "world_map"){
        
        # -- option
        cat("[location] Show location, option =", input$display_options, "\n")
        
        # -- get the data & apply option
        locations <- r[[r_items]]()
        if(input$display_options == "been-there")
          locations <- locations[locations$been.there, ]
        else if(input$display_options == "wish-list")
          locations <- locations[locations$wish.list, ]
        
        cat("-- apply type filter output dim =", dim(locations)[1], "obs. \n")
        
        # -- filter by country
        if(!is.null(r$filter_country)){
          locations <- locations[locations$country %in% r$filter_country, ]
          cat("-- apply country filter output dim =", dim(locations)[1], "obs. \n")}
        
      } else {
        
        cat("[location] Show trip locations \n")
        
        locations <- selected_location()
      
      }
        
      # -- check dim
      if(dim(locations)[1] > 0){
        
        # -- get bounding box (args for flyToBounds)
        lng_min <- min(locations$lng)
        lng_max <- max(locations$lng)
        lat_min <- min(locations$lat)
        lat_max <- max(locations$lat)

        # -- prepare marker icon
        locations <- locations %>%
          mutate(icon = case_when(been.there ~ 'been.there',
                                  wish.list ~ 'wish.list',
                                  type == 'Port' ~ 'port',
                                  type == 'Accomodation' ~ 'bed'))
        
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
                                actionLink(inputId = "delete_%s", 
                                           label =  "Delete", 
                                           onclick = sprintf(
                                             'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                             ns("button_click"))),
                                actionLink(inputId = "been-there_%s", 
                                           label =  "Been there", 
                                           onclick = sprintf(
                                             'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                             ns("button_click")))), id, id),
                            #clusterOptions = markerClusterOptions(),
                            clusterOptions = NULL)
        
        # -- update map view
        if(!r$freeze_map()){
          r$proxymap %>%
            flyToBounds(lng1 = lng_min, lat1 = lat_min, lng2 = lng_max, lat2 = lat_max, 
                        options = list(duration = fly_duration, padding = c(50, 50)))}
      }
      
    }, ignoreNULL = FALSE)
    
    
    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$hide_show, 
                 hide_show(proxy = r$proxymap, id = group_id, show = input$hide_show))
    
    
    # -------------------------------------
    
    # -- Observe: button click from marker popup
    observeEvent(input$button_click, {
      
      # -- get id from input value
      action <- unlist(strsplit(input$button_click, split = "_"))[1]
      id <- unlist(strsplit(input$button_click, split = "_"))[2]
      cat("[EVENT] Marker popup click: action =", action, "/ id =", id, "\n")

      # -- action: delete
      if(action == "delete")
        
        # -- call trigger
        r[[r_trigger_delete]](id)
      
      # -- action: switch to been-there
      else if(action == "been-there"){
        
        # -- update item
        item <- r[[r_items]]()[r[[r_items]]()$id == id, ]
        item$been.there <- TRUE
        item$wish.list <- FALSE
        
        # -- call trigger
        r[[r_trigger_update]](item)}
      
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
    selected_location <- eventReactive(r$location_select, {
      
      # -- get items
      locations <- r[[r_items]]()
      
      # -- apply selection
      locations[locations$id %in% r$location_select, ]
      
    })
    
  })
}
