

# ------------------------------------------------------------------------------
# Shiny module: dependencies
# ------------------------------------------------------------------------------

# -- Library
library(geosphere)


# ------------------------------------------------------------------------------
# Module Server logic
# ------------------------------------------------------------------------------

transport_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -------------------------------------
    # Load resources (airports)
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
    
    
    # -- load airports
    airports <- kfiles::read_data(file = filename,
                                  path = path$resources, 
                                  colClasses = colClasses_airports,
                                  create = FALSE)
    
    
    # -------------------------------------
    # Data manager (transports)
    # -------------------------------------
    
    # -- module id
    kitems_id <- "transport"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    r_trigger_create <- kitems::trigger_create_name(id = kitems_id)
    
    
    # -------------------------------------
    # Add transport
    # -------------------------------------
    
    observeEvent(input$add_transport, {
      
      # -- display modal
      showModal(modalDialog(
        
        title = "Select route",
        
        "Select the origin.",
        
        # -- create the selectizeInput with empty choices (perfo)
        selectizeInput(inputId = ns("select_origin"), label = "Origin", choices = NULL),
        selectizeInput(inputId = ns("select_destination"), label = "Destination", choices = NULL),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("confirm_route"), label = "OK")
        )
      ))
      
      # -- update the selectizeInput with choices (server)
      choices <-airports$id
      names(choices) <- airports$iata
      choices <- as.list(choices)
      updateSelectizeInput(session, 'select_origin', choices = choices, server = TRUE)
      updateSelectizeInput(session, 'select_destination', choices = choices, server = TRUE)
      
      # -- observe modal action btn
      observeEvent(input$confirm_route, {
        
        # -- close
        removeModal()
        
        # -- declare cache function
        cache_origin <<- function() {
          cat("[cache_origin] origin =", input$select_origin, "\n ")
          input$select_origin}
        
        # -- declare cache function
        cache_destination <<- function() {
          cat("[cache_origin] origin =", input$select_destination, "\n ")
          input$select_destination}
        
        # -- call trigger
        r[[r_trigger_create]](r[[r_trigger_create]]() + 1)
        
      })
      
    })
    
    
    # -------------------------------------
    # Display routes
    # -------------------------------------

    # -- add path to map
    observeEvent(input$show_transport, {
      
      cat("[transportS] Updating transport segments \n")
      
      # -- get flight data
      transports <- r[[r_items]]()
      transports <- transports[transports$type == 'flight', ]
      
      # -- Helper: add transport route to map
      addtransport <- function(transportid){
        
        # -- get route parameters
        origin <- transports[transports$id == transportid, 'origin']
        destination <- transports[transports$id == transportid, 'destination']
        origin_name <- airports[airports$id == origin, 'name']
        destination_name <- airports[airports$id == destination, 'name']
        
        cat("-- transport origin", origin, "/ destination", destination, "\n")
        
        # -- compute great circle route
        route <- gcIntermediate(p1 = airport_coord(airports, id = origin), 
                                p2 = airport_coord(airports, id = destination), 
                                n = 100, 
                                addStartEnd = TRUE)
        
        # -- add to proxy map
        r$proxymap %>%
          
          # add fight route
          addPolylines(data = route, group = "transports", color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
        
      }
      
      # -- apply helper to transports df
      cat("[transportS] Looping over transport list... \n")
      lapply(transports$id, addtransport)
      
      # -- update hide / show
      updateCheckboxInput(inputId = "show_hide", value = TRUE)
        
    })
    

    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$show_hide, {
      
      # checkbox marked
      if(input$show_hide){
        
        cat("Show group: transports \n")
        
        # proxy map
        r$proxymap %>%
          
          # Show group
          showGroup('transports')
        
      }else{
        
        cat("Hide group: transports \n")
        
        # proxy map
        r$proxymap %>%
          
          # clear group
          hideGroup('transports')
      }
      
    })
    
  })
}

