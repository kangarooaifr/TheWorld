

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

trip_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    
    # -------------------------------------
    # Trip management
    # -------------------------------------
    
    # -- id
    trip_kitems_id <- "trip"
    
    # id, name, status, description, comment
    # status = draft, planned, inwork, done
    # date.start, date.end, cities, countries >> computed from other tables based on trip.id
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = trip_kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(trip_kitems_id)
    # r_trigger_add <- kitems::trigger_add_name(id = trip_kitems_id)
    # r_trigger_delete <- kitems::trigger_delete_name(id = trip_kitems_id)
    
    
    # -------------------------------------
    # Trip selector
    # -------------------------------------
    
    # -- observer:
    # feed trip_selector when items are ready
    observeEvent(r[[r_items]](), {
      
      # -- get items & prepare choices
      trips <- r[[r_items]]()
      choices <- trips$id
      names(choices) <- trips$name
      
      # -- update input
      updateSelectizeInput(inputId = "trip_selector", choices = choices)
      
    })
    
    
    # -- observer:
    observeEvent(input$trip_selector, {
      
      cat("[trip] Trip selector, id =", input$trip_selector, "\n")
      
    })
    
    
    # -------------------------------------
    # Transport management
    # -------------------------------------
    
    # -- id
    transport_kitems_id <- "transport"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = transport_kitems_id, r, path$data)
    
    # -- to be defined: functions
    # cache_trip_id
    # cache_route_id
    
    
    # -- init outputs
    output$transport_zone <- NULL
    
    
    # -- observer
    observeEvent(input$add_transport, {
      
      cat("[trip] Add transport \n")
      
      output$transport_zone <- renderUI(
        tagList(
          
          radioButtons(inputId = ns("route_type"), 
                       label = "", 
                       choiceNames = list(icon("plane"), icon("train"), icon("ship"), icon("bus")), 
                       choiceValues = list("flight", "train", "sea", "road"),
                       selected = character(0),
                       inline = TRUE),
          
          selectizeInput(inputId = ns("select_route"), label = "Select", choices = NULL)
          
      ))
      
      
      observeEvent(input$route_type, {
        
        r$route_search_string<- input$route_type})
      
      
      observeEvent(r$route_search_result(), {
        
        cat("[trip] Route search result, dim =", dim(r$route_search_result()), "\n")
        
        # -- compute choices
        result <- r$route_search_result()
        choices <- result$id
        names(choices) <- paste(result$company, result$number, result$origin.iata, result$destination.iata)
        
        updateSelectizeInput(inputId = "select_route", choices = choices)
        
        
      })
        

    })
    
    
    
    # -------------------------------------
    # -- create tables flight, sea, road to instantiate in route!
    
    # -------------------------------------
    # Accommodation management
    # -------------------------------------
    
    # -- id
    accomodation_kitems_id <- "accomodation"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = accomodation_kitems_id, r, path$data)
    
    # id, trip.id, location.id, checkin, checkout, breakfast, comment
    
    # -- to be defined: functions
    # cache_trip_id
    # cache_location_id
    
  })
}
