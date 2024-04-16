

# ------------------------------------------------------------------------------
# Shiny module: dependencies
# ------------------------------------------------------------------------------

# -- Library
library(geosphere)


# ------------------------------------------------------------------------------
# Module Server logic
# ------------------------------------------------------------------------------

route_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- id
    group_id <- "routes"
    
    # -------------------------------------
    # Data manager (routes)
    # -------------------------------------
    
    # -- module id
    kitems_id <- "route"
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = kitems_id, r, path$data)
    
    # -- items name
    r_items <- kitems::items_name(id = kitems_id)
    r_data_model <- kitems::dm_name(id = kitems_id)
    r_trigger_add <- kitems::trigger_add_name(id = kitems_id)
    r_trigger_delete <- kitems::trigger_delete_name(id = kitems_id)
    r_trigger_create <- kitems::trigger_create_name(id = kitems_id)
    
    
    # -------------------------------------
    # Add route
    # -------------------------------------
    
    observeEvent(input$add_route, {
      
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
      choices <- r$airports$id
      names(choices) <- r$airports$iata
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
    observeEvent(r[[r_items]](), {
      
      cat("[routeS] Updating route segments \n")
      
      # -- get flight data
      routes <- r[[r_items]]()
      routes <- routes[routes$type == 'flight', ]
      
      # -- Helper: add route route to map
      addroute <- function(routeid){
        
        # -- get route parameters
        origin <- routes[routes$id == routeid, 'origin']
        destination <- routes[routes$id == routeid, 'destination']
        origin_name <- r$airports[r$airports$id == origin, 'name']
        destination_name <- r$airports[r$airports$id == destination, 'name']
        
        cat("-- route origin", origin, "/ destination", destination, "\n")
        
        # -- compute great circle route
        route <- gcIntermediate(p1 = airport_coord(r$airports, id = origin), 
                                p2 = airport_coord(r$airports, id = destination), 
                                n = 100, 
                                addStartEnd = TRUE)
        
        # -- add to proxy map
        r$proxymap %>%
          
          hideGroup(group_id) %>%
          
          # add fight route
          addPolylines(data = route, group = group_id, color = "purple", weight = 2, popup = route_labels(origin_name, destination_name))
        
      }
      
      # -- apply helper to routes df
      cat("[routes] Looping over route list... \n")
      lapply(routes$id, addroute)
        
    })
    

    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$hide_show, {
      
      # checkbox marked
      if(input$hide_show){
        
        cat("Show group:", group_id, "\n")
        
        # proxy map
        r$proxymap %>%
          
          # Show group
          showGroup(group_id)
        
      }else{
        
        cat("Hide group:", group_id, "\n")
        
        # proxy map
        r$proxymap %>%
          
          # clear group
          hideGroup(group_id)
      }
      
    })
    
  })
}

