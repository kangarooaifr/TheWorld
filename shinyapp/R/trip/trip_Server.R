

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

trip_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- shared input zone
    output$shared_zone <- NULL
    is_shared_zone <- reactiveVal("")
    
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
      updateSelectizeInput(inputId = "trip_selector", choices = choices,
                           options = list(placeholder = 'Please select an option below',
                                          onInitialize = I('function() { this.setValue(""); }')))
      
    })
    
    
    # -- observer:
    observeEvent(input$trip_selector, {
      
      cat("[trip] Trip selector, id =", input$trip_selector, "\n")
      
      # -- get steps
      steps <- r[[step_items]]()
      steps <- steps[steps$trip.id == input$trip_selector, ]
      
      str(steps)
      
      # -- get transports
      transports <- r[[transport_r_items]]()
      transports <- transports[transports$trip.id == input$trip_selector, ]
      
      # -- call trigger (select route)
      r$route_select <- transports$route.id
      
      # -- output
      str(transports)
      
      # -- get accommodations
      accommodations <- r[[accommodation_r_items]]()
      accommodations <- accommodations[accommodations$trip.id == input$trip_selector, ]
      
      # -- output
      str(accommodations)
      
      # -- select locations
      r$location_select <- c(steps$location.id, accommodations$location.id, r$selected_route()$origin, r$selected_route()$destination)
      
      
      # -- compute values
      date_start <- min(c(transports$departure, accommodations$checkin))
      date_end <- max(c(transports$arrival, accommodations$checkout))
      duration <- date_end - date_start
      
      output$trip_info <- renderUI(
        tagList(
          p(strong('Start:'), date_start),
          p(strong('End:'), date_end),
          p(strong('Duration:'), duration)))
      
    }, ignoreInit = TRUE)
    
    
    # -------------------------------------
    # Step management
    # -------------------------------------
    
    # -- id
    step_kitems_id <- "step"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = step_kitems_id, r, path$data)
    
    # -- get names
    step_data_model <- kitems::dm_name(step_kitems_id)
    step_items <- kitems::items_name(step_kitems_id)
    step_trigger_add <- kitems::trigger_add_name(step_kitems_id)
    
    # id, trip.id, location.id, order, comment
    
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
        
        # -- init location search trigger
        r$location_search_string <- 'City'
        
        # -- observer: search result
        observeEvent(r$location_search_result(), {
          
          cat("[trip] Location search result, dim =", dim(r$location_search_result()), "\n")
          
          # -- compute choices
          result <- r$location_search_result()
          choices <- result$id
          names(choices) <- paste0(result$name, ", ", result$city, " - ", result$country)
          
          # -- update input
          updateSelectizeInput(inputId = "select_step", choices = choices)
          
        })
        
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
                     order = step_order(r[[step_items]]()),
                     comment = input$step_comment)
      
      # -- create item
      step <- kitems::item_create(values, data.model = r[[step_data_model]]())
      
      # -- call trigger
      r[[step_trigger_add]](step)
      
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
    
    
    # -- observer
    observeEvent(input$add_transport, {
      
      # -- hide / show
      if(is_shared_zone() == "transport"){
        
        output$shared_zone <- NULL
        is_shared_zone("")
        
      } else {
        
        cat("[trip] Add transport \n")
        
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
    accommodation_kitems_id <- "accommodation"
    
    # -- names
    accommodation_r_items <- kitems::items_name(accommodation_kitems_id)
    accommodation_r_trigger_add <- kitems::trigger_add_name(accommodation_kitems_id)
    accommodation_r_data_model <- kitems::dm_name(accommodation_kitems_id)
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = accommodation_kitems_id, r, path$data)
    
    # id, trip.id, location.id, checkin, checkout, breakfast, comment
    
    # -- observer
    observeEvent(input$add_accommodation, {
      
      if(is_shared_zone() == "accommodation"){
        
        # -- clear all
        output$shared_zone <- NULL
        is_shared_zone("")
        
      } else {
        
        cat("[trip] Add accommodation \n")
        
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
        
        # -- init location search trigger
        r$location_search_string <- 'Accommodation'
        
        # -- observer: search result
        observeEvent(r$location_search_result(), {
          
          cat("[trip] Accommodation search result, dim =", dim(r$location_search_result()), "\n")
          
          # -- compute choices
          result <- r$location_search_result()
          choices <- result$id
          names(choices) <- paste0(result$name, ", ", result$city, " - ", result$country)
          
          # -- update input
          updateSelectizeInput(inputId = "select_accommodation", choices = choices)
          
        })
        
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
      accommodation <- kitems::item_create(values, data.model = r[[accommodation_r_data_model]]())
      
      # -- call trigger
      r[[accommodation_r_trigger_add]](accommodation)
      
      
      
    })
    
    
  })
}
