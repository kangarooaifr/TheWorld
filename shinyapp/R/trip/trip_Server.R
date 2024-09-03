

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

trip_Server <- function(id, map, locations, location_ns, routes, r, path) {
  moduleServer(id, function(input, output, session) {
    
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- get namespace
    ns <- session$ns
    
    # -- settings
    setting(name = "coord_digits", type = "numeric", default = 3)
    
    # -- get names
    route_group_id <- 'route'
    
    
    # --------------------------------------------------------------------------
    # Init
    # --------------------------------------------------------------------------
    
    # -- shared input zone
    output$shared_zone <- NULL
    is_shared_zone <- reactiveVal("")
    
    # -- marker icons
    icons <- location_icons()
    
    
    # --------------------------------------------------------------------------
    # Register observer (map_click)
    # --------------------------------------------------------------------------
    
    obs <- map_click_observer(map, onclick_id = ns("add_location"), coord_digits = setting("coord_digits"))
    
    
    # --------------------------------------------------------------------------
    # Trip management
    # --------------------------------------------------------------------------
    
    # -- id
    trip_kitems_id <- "trip"
    
    # -- launch kitems sub module
    trips <- kitems::kitemsManager_Server(id = trip_kitems_id, r, path$data)
    
    
    # --------------------------------------------------------------------------
    # Trip selector
    # --------------------------------------------------------------------------
    
    # -- observer:
    # feed trip_selector when items are ready
    observeEvent(trips$items(), {
      
      # -- get items & prepare choices
      choices <- trips$items()$id
      names(choices) <- trips$items()$name
      
      # -- update input
      updateSelectizeInput(inputId = "trip_selector", choices = choices,
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }')))
      
    })
    
    
    # --------------------------------------------------------------------------
    # select trip items
    # --------------------------------------------------------------------------
    
    # -- steps
    selected_steps <- reactive(steps$items()[steps$items()$trip.id == input$trip_selector, ])
    
    # -- transports
    selected_transports <- reactive(transports$items()[transports$items()$trip.id == input$trip_selector, ])
    
    # -- accommodations
    selected_accommodations <- reactive(accommodations$items()[accommodations$items()$trip.id == input$trip_selector, ])
    
    # -- select locations
    selected_locations <- reactive(
      location_select(locations$items(), r$airports, location_id = c(selected_steps()$location.id, 
                                                               selected_accommodations()$location.id, 
                                                               selected_route()$origin, 
                                                               selected_route()$destination))) %>% 
      bindEvent(list(selected_steps(), selected_accommodations(), selected_route()))
    
    # -- select route 
    # keep it after location to minimize fly to bounds side effect (half route displayed + refresh after crop)
    selected_route <- reactive(
      route_select(routes = routes$items(), query = selected_transports()$route.id))
    
    
    # --------------------------------------------------------------------------
    # Display trip items on map
    # --------------------------------------------------------------------------
    
    # -- display routes
    observeEvent(selected_route(), {
      
      # -- init
      routes <- selected_route()
      cat(MODULE, "Update map, selected routes =", length(routes$id), "\n")
      
      # -- clear map (group)
      map$proxy %>%
        clearGroup(route_group_id)
      
      # -- check (otherwise unselect, so clearGroup is enough)
      if(length(routes$id) > 0){
        
        # -- Helper: add route to map
        addroute <- function(id){
          
          # -- get route & parameters
          route <- routes[routes$id == id, ]
          origin <- route$origin
          destination <- route$destination
          type  <- route$type
          
          cat("-- route origin", origin, "/ destination", destination, "/ type =", type, "\n")
          
          # -- mode: air
          if(type == 'air'){
            
            # -- get names
            origin_name <- r$airports[r$airports$id == origin, 'name']
            destination_name <- r$airports[r$airports$id == destination, 'name']
            
            # -- compute great circle route
            route <- gcIntermediate(p1 = airport_coord(r$airports, id = origin), 
                                    p2 = airport_coord(r$airports, id = destination), 
                                    n = 100, 
                                    addStartEnd = TRUE)
            
            # -- add fight route
            map$proxy %>%
              addPolylines(data = route, group = route_group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
            
            
            # -- mode: sea
          } else if(type == 'sea'){
            
            # -- get names
            origin_name <- r$seaports()[r$seaports()$id == origin, 'name']
            destination_name <- r$seaports()[r$seaports()$id == destination, 'name']
            
            # -- compute route
            route <- data.frame(lng = c(r$seaports()[r$seaports()$id == origin, 'lng'], r$seaports()[r$seaports()$id == destination, 'lng']),
                                lat = c(r$seaports()[r$seaports()$id == origin, 'lat'], r$seaports()[r$seaports()$id == destination, 'lat']))
            
            # -- add sea route
            map$proxy %>%
              addPolylines(lng = route$lng, lat = route$lat, group = route_group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
            
          }
          
        }
        
        # -- apply helper to routes df
        cat(MODULE, "Looping over route list... \n")
        lapply(routes$id, addroute)
        
      }
      
    })
    
    
    # -- display locations
    observe({
      
      x <- selected_locations()
      
      # -- remove markers
      clearMarkers(map$proxy)
      
      # -- check dim #
      if(nrow(x) != 0){
        
        # -- add icon & popup columns
        x <- location_icon(x)
        x$popup <- location_popups(x, type = 'selected', activity = 'trip_planner', ns = ns, location_ns = location_ns)
        
        # -- display on map
        add_markers(x, map_proxy = map$proxy, icons = icons)
        
        # -- crop map around markers
        map_crop(map_proxy = map$proxy, 
                 lng1 = min(x$lng), 
                 lat1 = min(x$lat), 
                 lng2 = max(x$lng),
                 lat2 = max(x$lat), 
                 fly_duration, 
                 fly_padding)}
      
    }) %>% bindEvent(selected_locations())
    
    
    
    # -- compute timeline table
    # takes into account transports & accommodations (because steps have no date/time)
    timeline_table <- reactive({
      
      cat(MODULE, "Compute timeline table \n")
      
      # -- transports
      transports <- selected_transports()[c('id', 'departure', 'arrival')]
      colnames(transports) <- c('id', 'start', 'end')
      
      # -- accommodations
      accomodations <- selected_accommodations()[c('id', 'checkin', 'checkout')]
      colnames(accomodations) <- c('id', 'start', 'end')
      
      # -- return
      rbind(transports, accomodations)
      
    }) %>% bindEvent(list(selected_transports(), selected_accommodations()),
                     ignoreInit = FALSE)
    
    
    # --------------------------------------------------------------------------
    # trip info outputs
    # --------------------------------------------------------------------------
    
    # -- routes
    output$trip_transport <- renderUI({
      
      cat(MODULE, "Update trip info \n")
      
      id <- selected_transports()$id[[1]]
      cat("id ==", id, "\n")
      
      # -- return
      tagList(
        p(strong('Departure:'), selected_transports()[selected_transports()$id == id, ]$departure),
        p(strong('Arrival:'), selected_transports()[selected_transports()$id == id, ]$arrival),
        
        p(strong('Company:'), selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$company),
        p(strong('Flight #:'), selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$number),
        
        p(strong('Origin:'),
          HTML(
            sprintf(
              paste0(
                
                # -- remove from trip
                actionLink(inputId = "flyto_%s_%s",
                           label =  r$selected_locations[r$selected_locations$id == selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$origin, ]$name,
                           onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                             ns("fly_to_location")))),
              
              r$selected_locations[r$selected_locations$id == selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$origin, ]$lng,
              r$selected_locations[r$selected_locations$id == selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$origin, ]$lat)),
        ),
        
        p(strong('Destination:'),
          HTML(
            sprintf(
              paste0(
                
                # -- remove from trip
                actionLink(inputId = "flyto_%s_%s",
                           label =  r$selected_locations[r$selected_locations$id == selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$destination, ]$name,
                           onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                             ns("fly_to_location")))),
              
              r$selected_locations[r$selected_locations$id == selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$destination, ]$lng,
              r$selected_locations[r$selected_locations$id == selected_route()[selected_route()$id == selected_transports()[selected_transports()$id == id, ]$route.id, ]$destination, ]$lat)),
          
        ))
      
    }) %>% bindEvent(list(selected_transports(), selected_route(), r$selected_locations),
                     ignoreInit = TRUE)
    
    
    # -- Observe: fly_to_location
    observeEvent(input$fly_to_location, {
      
      # -- extract values
      lng <- unlist(strsplit(input$fly_to_location, split = "_"))[2]
      lat <- unlist(strsplit(input$fly_to_location, split = "_"))[3]
      cat("[EVENT] ActionLink click: fly_to_location lng =", lng, ", lat =", lat, "\n")
      
      # -- fly to
      if(!map$freeze()){
        map$proxy %>%
          flyTo(lng = lng,
                lat = lat,
                zoom = setting("fly_zoom"),
                optionss = list(duration = setting("fly_duration"), 
                                padding = c(setting("fly_padding"), setting("fly_padding"))))}
      
    })
    
    
    # -- output: trip_info
    output$trip_info <- renderUI({
      
      # -- compute values
      date_start <- min(c(selected_transports()$departure, selected_accommodations()$checkin))
      date_end <- max(c(selected_transports()$arrival, selected_accommodations()$checkout))
      duration <- round(date_end - date_start, digits = 0)
      
      # -- return tag
      tagList(
        p(strong('Start:'), date_start),
        p(strong('End:'), date_end),
        p(strong('Duration:'), duration),
        
        sliderInput(ns("timeline"), label = "Timeline", 
                    min = as.Date(date_start), max = as.Date(date_end), value = Sys.Date(),
                    animate = TRUE))}) %>% bindEvent(list(selected_transports(), selected_accommodations()), ignoreInit = TRUE)
    
    # -- observer: timeline
    observe({
      
      cat(MODULE, "New timeline value =", input$timeline, "\n")
      
      # -- slice timeline table
      slice <- timeline_table()[as.Date(timeline_table()$start) == input$timeline | as.Date(timeline_table()$end) == input$timeline, ]
      
      str(slice)
      
    }) %>% bindEvent(input$timeline, ignoreInit = TRUE)
    
    
    
    
    # -- accommodations
    output$trip_accommodation <- renderUI({
      
      cat(MODULE, "Update accommodation info \n")
      
      id <- selected_accommodations()$id[[1]]
      cat("id ==", id, "\n")
      
      # -- return
      tagList(
        
        p(strong('Name:'),
          HTML(
            sprintf(
              paste0(

                # -- remove from trip
                actionLink(inputId = "flyto_%s_%s",
                           label =  r$selected_locations[r$selected_locations$id == selected_accommodations()[selected_accommodations()$id == id, ]$location.id, ]$name,
                           onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                                             ns("fly_to_location")))),

              r$selected_locations[r$selected_locations$id == selected_accommodations()[selected_accommodations()$id == id, ]$location.id, ]$lng,
              r$selected_locations[r$selected_locations$id == selected_accommodations()[selected_accommodations()$id == id, ]$location.id, ]$lat))),
        
        p(strong('Check-in:'), selected_accommodations()[selected_accommodations()$id == id, ]$checkin),
        p(strong('Check-out:'), selected_accommodations()[selected_accommodations()$id == id, ]$checkout))
      
    }) %>% bindEvent(list(selected_transports(), selected_route(), r$selected_locations),
                     ignoreInit = TRUE)
    
    
    # -- style test
    output$mywidget <- renderUI(
      
      div(
        p("some text goes here.")
      ) %>% tagAppendAttributes(class = "my_row_class") 
    
    )
    
    # --------------------------------------------------------------------------
    # Step management
    # --------------------------------------------------------------------------
    
    # -- id
    step_kitems_id <- "step"
    
    # -- launch kitems sub module
    steps <- kitems::kitemsManager_Server(id = step_kitems_id, r, path$data)
    
    # -- get names
    step_data_model <- kitems::dm_name(step_kitems_id)
    step_trigger_add <- kitems::trigger_add_name(step_kitems_id)
    step_trigger_delete <- kitems::trigger_delete_name(step_kitems_id)
    
    # -- define triggers
    r$trigger_add_step <- NULL
    r$trigger_remove_step <- NULL
    
    # -- observer
    observeEvent(input$add_location, {
      
      # -- check shared zone
      if(is_shared_zone() == "step"){
        
        # -- clear zone
        output$shared_zone <- NULL
        is_shared_zone("")
        
      } else {
        
        # -- declare cache
        is_shared_zone("step")
        
        # -- output
        output$shared_zone <- renderUI(
          tagList(
            
            # -- location
            selectizeInput(inputId = ns("select_step"), label = "Select", choices = NULL, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'))),
            
            # -- comment
            textInput(inputId = ns("step_comment"), label = "Comment"),
            
            actionButton(inputId = ns("confirm_step"), label = "OK")
            
          )
        )
        
        # -- location search
        result <- search_item(locations$items(), pattern = 'city')
        
        # -- check result size to avoid crash
        if(dim(result)[1] > 0){
          
          choices <- result$id
          names(choices) <- paste0(result$name, ", ", result$city, " - ", result$country)
          
          # -- update input
          updateSelectizeInput(inputId = "select_step", choices = choices)}
        
      }
      
    })
    
    
    # -- observe: confirm step
    observeEvent(input$confirm_step, {
      
      cat("[EVENT] Add step, selected location =", input$select_step, "\n")
      
      # -- clear all
      output$shared_zone <- NULL
      is_shared_zone("")
      
      # -- compute values
      values <- list(id = ktools::getTimestamp(),
                     trip.id = input$trip_selector,
                     location.id = input$select_step,
                     order = step_order(steps$items()),
                     comment = input$step_comment)
      
      # -- create item
      step <- kitems::item_create(values, data.model = r[[step_data_model]]())
      
      # -- call trigger
      r[[step_trigger_add]](step)
      
    })
    
    
    # -- Observe: add_to_trip
    observeEvent(input$add_to_trip, {
      
      # -- extract id
      id <- unlist(strsplit(input$add_to_trip, split = "_"))[2]
      cat("[EVENT] Marker popup click: add_to_trip id =", id, "\n")
      
      # -- get location
      location <- locations$items()[locations$items()$id == id, ]
      
      # -- call trigger
      r$trigger_add_step <- location
      
    })
    
    
    # -- observe: trigger_add_step
    observeEvent(r$trigger_add_step, {

      cat("[TRIGGER] Add step to trip, input location =", r$trigger_add_step$id, "\n")

      # -- compute values
      values <- list(id = ktools::getTimestamp(),
                     trip.id = input$trip_selector,
                     location.id = r$trigger_add_step$id,
                     order = step_order(steps$items()),
                     comment = NULL)
      
      # -- create item
      step <- kitems::item_create(values, data.model = r[[step_data_model]]())
      
      # -- call trigger
      r[[step_trigger_add]](step)
      
    })
    
    
    # -- observe: trigger_remove_step
    observeEvent(r$trigger_remove_step, {
      
      cat("[TRIGGER] Remove step from trip, input location =", r$trigger_remove_step, "\n")
      
      # -- get step id from location.id
      id <- steps$items()[steps$items()$location.id == r$trigger_remove_step, ]$id
      str(id)
      
      # -- call trigger
      r[[step_trigger_delete]](id)
      
    })
    
    
    # -- Observe: remove_from_trip
    observeEvent(input$remove_from_trip, {
      
      # -- extract id
      id <- unlist(strsplit(input$remove_from_trip, split = "_"))[2]
      cat("[EVENT] Marker popup click: remove_from_trip id =", id, "\n")
      
      # -- call trigger
      r$trigger_remove_step <- id
      
    })
    
    
    # --------------------------------------------------------------------------
    # Transport management
    # --------------------------------------------------------------------------
    
    # -- id
    transport_kitems_id <- "transport"
    
    # -- launch kitems sub module
    transports <- kitems::kitemsManager_Server(id = transport_kitems_id, r, path$data)
    
    # -- observer
    observeEvent(input$add_transport, {
      
      # -- hide / show
      if(is_shared_zone() == "transport"){
        
        output$shared_zone <- NULL
        is_shared_zone("")
        
      } else {
        
        cat(MODULE, "Add transport \n")
        
        # -- declare cache
        is_shared_zone("transport")
        
        # -- output
        output$shared_zone <- renderUI(
          tagList(
            
            radioButtons(inputId = ns("route_type"), 
                         label = "", 
                         choiceNames = list(icon("plane"), icon("train"), icon("ship"), icon("bus")),
                         choiceValues = list("air", "rail", "sea", "road"),
                         inline = TRUE),
            
            selectizeInput(inputId = ns("select_route"), label = "Select", choices = NULL, 
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'))),
            
            dateInput(inputId = ns("departure_date"), label = "Departure date", value = Sys.Date()),
            timeInput(inputId = ns("departure_time"), label = "Departure time", value = Sys.time()),
            selectizeInput(inputId = ns("departure_tz"), label = "Departure tz", choices = OlsonNames(), selected = Sys.timezone()),
            
            
            dateInput(inputId = ns("arrival_date"), label = "Arrival date", value = Sys.Date()),
            timeInput(inputId = ns("arrival_time"), label = "Arrival time", value = Sys.time()),
            selectizeInput(inputId = ns("arrival_tz"), label = "Arrival tz", choices = OlsonNames(), selected = Sys.timezone()),
            
            textInput(inputId = ns("route_comment"), label = "Comment"),
            
            actionButton(inputId = ns("confirm_route"), label = "OK")))
        
      }
        
      
      # -- observer: transport mode radio
      observeEvent(input$route_type, {
        
        cat(MODULE, "Event route_type input \n")

        # -- search route
        result <- route_search(routes = routes$items(), pattern = input$route_type, airports = r$airports, seaports = r$seaports())
        
        # -- check
        if(nrow(result) > 0){
          
          # -- compute choices
          choices <- result$id
          names(choices) <- paste(result$company, "[", result$number, "]", result$origin.code, ">", result$destination.code)
          
          # -- update input
          updateSelectizeInput(inputId = "select_route", choices = choices)}
        
      })
        
    })
    
    
    # -- observer: confirm add route
    observeEvent(input$confirm_route, {
      
      # -- clear
      output$shared_zone <- NULL
      is_shared_zone("")
      
      # -- compute values
      departure <- paste(input$departure_date, input$departure_time)
      departure <- as.POSIXct(departure, tz = input$departure_tz)
      arrival <- paste(input$arrival_date, input$arrival_time)
      arrival <- as.POSIXct(arrival, tz = input$arrival_tz)
      
      # -- merge
      values <- list(id = ktools::getTimestamp(),
                     trip.id = input$trip_selector,
                     route.id = input$select_route,
                     departure = departure,
                     arrival = arrival,
                     comment = input$route_comment)
    
      # -- create item
      transport <- kitems::item_create(values, data.model = transports$data_model())

      # -- add item
      kitems::item_add(transports, item = transport, name = "transport")

    })
    
    
    # --------------------------------------------------------------------------
    # -- create tables flight, sea, road to instantiate in route!
    
    # --------------------------------------------------------------------------
    # Accommodation management
    # --------------------------------------------------------------------------
    
    # -- id
    accommodation_kitems_id <- "accommodation"
    
    # -- launch kitems sub module
    accommodations <- kitems::kitemsManager_Server(id = accommodation_kitems_id, r, path$data)
    
    # id, trip.id, location.id, checkin, checkout, breakfast, comment
    
    # -- observer
    observeEvent(input$add_accommodation, {
      
      if(is_shared_zone() == "accommodation"){
        
        # -- clear all
        output$shared_zone <- NULL
        is_shared_zone("")
        
      } else {
        
        cat(MODULE, "Add accommodation \n")
        
        # -- declare cache
        is_shared_zone("accommodation")
        
        # -- output
        output$shared_zone <- renderUI(
          tagList(
            
            # -- accommodation
            selectizeInput(inputId = ns("select_accommodation"), label = "Accommodation", choices = NULL,
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'))),
            
            # -- checkin
            dateInput(inputId = ns("checkin_date"), label = "Checkin date", value = Sys.Date()),
            timeInput(inputId = ns("checkin_time"), label = "Checkin time", value = Sys.time()),
            
            # -- checkout
            dateInput(inputId = ns("checkout_date"), label = "Checkout date", value = Sys.Date()),
            timeInput(inputId = ns("checkout_time"), label = "Checkout time", value = Sys.time()),
            selectizeInput(inputId = ns("accommodation_tz"), label = "Timezone", choices = OlsonNames(), selected = Sys.timezone()),
            
            # -- breakfast
            checkboxInput(inputId = ns("breakfast"), label = "Breakfast", value = FALSE),
            
            # -- comment
            textInput(inputId = ns("accommodation_comment"), label = "Comment"),
            
            # -- btn
            actionButton(inputId = ns("confirm_accommodation"), label = "OK")))
        
        # -- location search
        result <- search_item(locations$items(), pattern = 'accommodation')
        
        # -- check search result size to avoid crash
        if(dim(result)[1] > 0){
          
          choices <- result$id
          names(choices) <- paste0(result$name, ", ", result$city, " - ", result$country)
          
          # -- update input
          updateSelectizeInput(inputId = "select_accommodation", choices = choices)}
        
      }
      
    })
    
    
    observeEvent(input$confirm_accommodation, {
      
      # -- clear
      output$shared_zone <- NULL
      is_shared_zone("")
      
      # -- compute values
      checkin <- paste(input$checkin_date, input$checkin_time)
      checkin <- as.POSIXct(checkin, tz = input$accommodation_tz)
      checkout <- paste(input$checkout_date, input$checkout_time)
      checkout <- as.POSIXct(checkout, tz = input$accommodation_tz)
      
      # -- merge
      values <- list(id = ktools::getTimestamp(),
                     trip.id = input$trip_selector,
                     location.id = input$select_accommodation,
                     checkin = checkin,
                     checkout = checkout,
                     breakfast = input$breakfast,
                     comment = input$accommodation_comment)
  
      
      # -- create item
      accommodation <- kitems::item_create(values, data.model = accommodations$data_model())
      
      # -- add item
      kitems::item_add(accommodations, item = accommodation, name = "accommodation")
      
    })
    
  })
}
