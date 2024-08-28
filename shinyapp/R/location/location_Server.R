

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path, map_proxy, map_click) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- ids
    kitems_id <- "location"

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
    
    # -- filter out locations without gps coordinate (lng)
    cat("[location] Filter stations without lng / lat:", sum(is.na(stations$lng)), "\n")
    stations <-  stations[!is.na(stations$lng), ]
    
    # -- expose railway stations
    r$railway_stations <- stations[!stations$is_rail %in% FALSE, ]
    
    # -- expose bus stations
    r$bus_stations <- stations[stations$is_road %in% TRUE, ]

    
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
    
  })
}
