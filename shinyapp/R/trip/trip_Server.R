

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
      
      # -- get transports
      transports <- r[[transport_r_items]]()
      transports <- transports[transports$trip.id == input$trip_selector, ]
      
      output$tmp_trip_1 <- renderPrint(transports)
      
      # -- get accomodations
      accommodations <- r[[accomodation_r_items]]()
      accommodations <- accommodations[accommodations$trip.id == input$trip_selector, ]
      
      output$tmp_accomodation_1 <- renderPrint(accommodations)
      
      r$location_select <- accommodations$location.id
      
      
      # -- compute values
      date_start <- min(c(transports$departure, accommodations$checkin))
      date_end <- max(c(transports$arrival, accommodations$checkout))
      duration <- date_end - date_start
      
      output$tmp_trip_date <- renderPrint(paste("Start:", date_start, "/ end:", date_end, "/ duration:", duration))
      
    })
    
    
    # -------------------------------------
    # Transport management
    # -------------------------------------
    
    # -- id
    transport_kitems_id <- "transport"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = transport_kitems_id, r, path$data)
    
    # -- names
    transport_r_items <- kitems::items_name(transport_kitems_id)
    transport_r_data_model <- kitems::dm_name(transport_kitems_id)
    transport_r_trigger_add <- kitems::trigger_add_name(transport_kitems_id)
    
    # -- init outputs
    output$transport_zone <- NULL
    
    # -- other
    isTransportZone <- reactiveVal(FALSE)
    
    # -- observer
    observeEvent(input$add_transport, {
      
      # -- hide / show
      if(isTransportZone()){
        
        output$transport_zone <- NULL
        isTransportZone(FALSE)
        
      } else {
        
        cat("[trip] Add transport \n")
        
        isTransportZone(TRUE)
        
        output$transport_zone <- renderUI(
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
        r$route_search_string <- input$route_type})
      
      
      # -- observer: search result
      observeEvent(r$route_search_result(), {
        
        cat("[trip] Route search result, dim =", dim(r$route_search_result()), "\n")
        
        # -- compute choices
        result <- r$route_search_result()
        choices <- result$id
        names(choices) <- paste(result$company, "[", result$number, "]", result$origin.code, ">", result$destination.code)
        
        updateSelectizeInput(inputId = "select_route", choices = choices)
        
      })
        
    })
    
    
    # -- observer: confirm add route
    observeEvent(input$confirm_route, {
      
      # -- clear output
      output$transport_zone <- NULL
      
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
      transport <- kitems::item_create(values, data.model = r[[transport_r_data_model]]())

      # -- call trigger
      r[[transport_r_trigger_add]](transport)

    })
    
    
    # -------------------------------------
    # -- create tables flight, sea, road to instantiate in route!
    
    # -------------------------------------
    # Accommodation management
    # -------------------------------------
    
    # -- id
    accomodation_kitems_id <- "accomodation"
    
    # -- names
    accomodation_r_items <- kitems::items_name(accomodation_kitems_id)
    accomodation_r_trigger_add <- kitems::trigger_add_name(accomodation_kitems_id)
    accomodation_r_data_model <- kitems::dm_name(accomodation_kitems_id)
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = accomodation_kitems_id, r, path$data)
    
    # id, trip.id, location.id, checkin, checkout, breakfast, comment
    
    # -- to be defined: functions
    # cache_trip_id
    # cache_location_id
    
    # -- init output
    output$accomodation_zone <- NULL
    
    # -- hide / show
    isAccomodationZone <- reactiveVal(FALSE)
    
    # -- observer
    observeEvent(input$add_accomodation, {
      
      if(isAccomodationZone()){
        
        output$accomodation_zone <- NULL
        isAccomodationZone(FALSE)
        
      } else {
        
        cat("[trip] Add accommodation \n")
        
        isAccomodationZone(TRUE)
        
        # -- build output
        output$accomodation_zone <- renderUI(
          tagList(
            
            # -- accommodation
            selectizeInput(inputId = ns("select_accomodation"), label = "Accomodation", choices = NULL,
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }'))),
            
            # -- checkin
            dateInput(inputId = ns("checkin_date"), label = "Checkin date", value = Sys.Date()),
            timeInput(inputId = ns("checkin_time"), label = "Checkin time", value = Sys.time()),
            
            # -- checkout
            dateInput(inputId = ns("checkout_date"), label = "Checkout date", value = Sys.Date()),
            timeInput(inputId = ns("checkout_time"), label = "Checkout time", value = Sys.time()),
            selectizeInput(inputId = ns("accomodation_tz"), label = "Timezone", choices = OlsonNames(), selected = Sys.timezone()),
            
            # -- breakfast
            checkboxInput(inputId = ns("breakfast"), label = "Breakfast", value = FALSE),
            
            # -- comment
            textInput(inputId = ns("accommodation_comment"), label = "Comment"),
            
            # -- btn
            actionButton(inputId = ns("confirm_accomodation"), label = "OK")))
        
        # -- init location search trigger
        r$location_search_string <- 'Accomodation'
        
        # -- observer: search result
        observeEvent(r$location_search_result(), {
          
          cat("[trip] Accomodation search result, dim =", dim(r$location_search_result()), "\n")
          
          # -- compute choices
          result <- r$location_search_result()
          choices <- result$id
          names(choices) <- paste0(result$name, ", ", result$city, " - ", result$country)
          
          # -- update input
          updateSelectizeInput(inputId = "select_accomodation", choices = choices)
          
        })
        
      }
      
    })
    
    
    observeEvent(input$confirm_accomodation, {
      
      # -- clear output
      output$accomodation_zone <- NULL
      
      # -- compute values
      checkin <- paste(input$checkin_date, input$checkin_time)
      checkin <- as.POSIXct(checkin, tz = input$accomodation_tz)
      checkout <- paste(input$checkout_date, input$checkout_time)
      checkout <- as.POSIXct(checkout, tz = input$accomodation_tz)
      
      # -- merge
      values <- list(id = ktools::getTimestamp(),
                     trip.id = input$trip_selector,
                     location.id = input$select_accomodation,
                     checkin = checkin,
                     checkout = checkout,
                     breakfast = input$breakfast,
                     comment = input$accommodation_comment)
  
      
      # -- create item
      accomodation <- kitems::item_create(values, data.model = r[[accomodation_r_data_model]]())
      
      # -- call trigger
      r[[accomodation_r_trigger_add]](accomodation)
      
      
      
    })
    
    
  })
}
