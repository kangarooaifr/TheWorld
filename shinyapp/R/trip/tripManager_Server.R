

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

tripManager_Server <- function(id, locations, countries, location_ns, r, path) {
  moduleServer(id, function(input, output, session) {
    
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- get namespace
    ns <- session$ns
    
    # -- get names
    route_group_id <- 'route'
    
    
    # --------------------------------------------------------------------------
    # Init
    # --------------------------------------------------------------------------
    

    
    # --------------------------------------------------------------------------
    # Map
    # --------------------------------------------------------------------------
    
    # -- call module
    map <- map_Server(id = "trip", r = r, verbose = TRUE)
    
    
    # --------------------------------------------------------------------------
    # Location manager
    # --------------------------------------------------------------------------

    # -- call module
    locationManager_Server(id = paste0(id, "_lm"), map, locations, countries,
                           onSelect = selected_locations, popups = tp_popups,
                           ns.callback = ns)


    # --------------------------------------------------------------------------
    # Trip management
    # --------------------------------------------------------------------------
    
    # -- launch kitems sub module
    trips <- kitems::kitemsManager_Server(id = "trip", r, path$data)
    
    
    # --------------------------------------------------------------------------
    # Step management
    # --------------------------------------------------------------------------
    
    # -- launch kitems sub module
    steps <- kitems::kitemsManager_Server(id = "step", r, path$data)
    
    
    # --------------------------------------------------------------------------
    # Accommodation management
    # --------------------------------------------------------------------------
    
    # -- launch kitems sub module
    accommodations <- kitems::kitemsManager_Server(id = "accommodation", r, path$data)
    
    
    # --------------------------------------------------------------------------
    # Route management
    # --------------------------------------------------------------------------
    
    # -- launch kitems sub module
    routes <- kitems::kitemsManager_Server(id = "route", r, path$data)
    
    
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
    
    # -- routes
    selected_routes <- reactive(routes$items()[routes$items()$trip.id == input$trip_selector, ])
    
    # -- accommodations
    selected_accommodations <- reactive(accommodations$items()[accommodations$items()$trip.id == input$trip_selector, ])
    
    # -- select locations
    selected_locations <- reactive(
      
      select_locations(locations, 
                       pattern = list(id = c(selected_steps()$location.id,
                                             selected_accommodations()$location.id,
                                             selected_routes()$origin,
                                             selected_routes()$destination)), 
                       result = c("locations", "airports"))
      
    ) %>% bindEvent(list(selected_steps(), selected_accommodations(), selected_routes()), ignoreInit = TRUE)
    

    # --------------------------------------------------------------------------
    # Display trip items on map
    # --------------------------------------------------------------------------
    
    # -- display routes
    observeEvent(selected_routes(), {
      
      # -- init
      routes <- selected_routes()
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
            origin_name <- locations$airports[locations$airports$id == origin, 'name']
            destination_name <- locations$airports[locations$airports$id == destination, 'name']
            
            # -- compute great circle route
            route <- geosphere::gcIntermediate(p1 = airport_coord(locations$airports, id = origin), 
                                               p2 = airport_coord(locations$airports, id = destination), 
                                               n = 100, 
                                               addStartEnd = TRUE)
            
            # -- add fight route
            map$proxy %>%
              addPolylines(data = route, group = route_group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
            
            
            # -- mode: sea
          } else if(type == 'sea'){
            
            # -- get names
            origin_name <- locations$seaports()[locations$seaports()$id == origin, 'name']
            destination_name <- locations$seaports()[locations$seaports()$id == destination, 'name']
            
            # -- compute route
            route <- data.frame(lng = c(locations$seaports()[locations$seaports()$id == origin, 'lng'], locations$seaports()[locations$seaports()$id == destination, 'lng']),
                                lat = c(locations$seaports()[locations$seaports()$id == origin, 'lat'], locations$seaports()[locations$seaports()$id == destination, 'lat']))
            
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
    
    
    # -- compute timeline table
    # takes into account routes & accommodations (because steps have no date/time)
    timeline_table <- reactive({
      
      cat(MODULE, "Compute timeline table \n")
      
      # -- routes
      route_df <- selected_routes()[c('id', 'departure', 'arrival')]
      colnames(route_df) <- c('id', 'start', 'end')
      
      # -- accommodations
      accomodations <- selected_accommodations()[c('id', 'checkin', 'checkout')]
      colnames(accomodations) <- c('id', 'start', 'end')
      
      # -- return
      rbind(route_df, accomodations)
      
    }) %>% bindEvent(list(selected_routes(), selected_accommodations()),
                     ignoreInit = FALSE)
    
    
    # --------------------------------------------------------------------------
    # trip info outputs
    # --------------------------------------------------------------------------
    
    # -- routes
    output$trip_transport <- renderUI({
      
      cat(MODULE, "Update trip info \n")
      
      id <- selected_routes()$id[[1]]
      cat("id ==", id, "\n")
      
      # -- return
      tagList(
        p(strong('Departure:'), selected_routes()[selected_routes()$id == id, ]$departure),
        p(strong('Arrival:'), selected_routes()[selected_routes()$id == id, ]$arrival),
        
        p(strong('Company:'), selected_routes()[selected_routes()$id == id, ]$company),
        p(strong('Flight #:'), selected_routes()[selected_routes()$id == id, ]$number),
        
        p(strong('Origin:'),
          HTML(
            sprintf(
              paste0(
                
                # -- remove from trip
                actionLink(inputId = "flyto_%s_%s",
                           label =  r$selected_locations[r$selected_locations$id == selected_routes()[selected_routes()$id == id, ]$origin, ]$name,
                           onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                             ns("fly_to_location")))),
              
              r$selected_locations[r$selected_locations$id == selected_routes()[selected_routes()$id == id, ]$origin, ]$lng,
              r$selected_locations[r$selected_locations$id == selected_routes()[selected_routes()$id == id, ]$origin, ]$lat)),
        ),
        
        p(strong('Destination:'),
          HTML(
            sprintf(
              paste0(
                
                # -- remove from trip
                actionLink(inputId = "flyto_%s_%s",
                           label =  r$selected_locations[r$selected_locations$id == selected_routes()[selected_routes()$id == id, ]$destination, ]$name,
                           onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                             ns("fly_to_location")))),
              
              r$selected_locations[r$selected_locations$id == selected_routes()[selected_routes()$id == id, ]$destination, ]$lng,
              r$selected_locations[r$selected_locations$id == selected_routes()[selected_routes()$id == id, ]$destination, ]$lat)),
          
        ))
      
    }) %>% bindEvent(list(selected_routes(), r$selected_locations),
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
      date_start <- min(c(selected_routes()$departure, selected_accommodations()$checkin))
      date_end <- max(c(selected_routes()$arrival, selected_accommodations()$checkout))
      duration <- round(date_end - date_start, digits = 0)
      
      # -- return tag
      tagList(
        p(strong('Start:'), date_start),
        p(strong('End:'), date_end),
        p(strong('Duration:'), duration),
        
        sliderInput(ns("timeline"), label = "Timeline", 
                    min = as.Date(date_start), max = as.Date(date_end), value = Sys.Date(),
                    animate = TRUE))}) %>% bindEvent(list(selected_routes(), selected_accommodations()), ignoreInit = TRUE)
    
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
      
    }) %>% bindEvent(list(selected_routes(), r$selected_locations),
                     ignoreInit = TRUE)
    
    
  })
}
