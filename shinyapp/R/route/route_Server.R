

# ------------------------------------------------------------------------------
# Shiny module: dependencies
# ------------------------------------------------------------------------------

# -- Library
library(geosphere)
library(dplyr)


# ------------------------------------------------------------------------------
# Module Server logic
# ------------------------------------------------------------------------------

route_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
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
      
      # -- compute available choices:
      if(input$transport_mode == "air"){
        
        choices <- r$airports$id
        names(choices) <- r$airports$iata
        
      } else if(input$transport_mode == "sea"){
        
        choices <- r$seaports()$id
        names(choices) <- paste(r$seaports()$city, r$seaports()$name, sep = "-")
        
      }
      choices <- as.list(choices)
      
      # -- update the selectizeInput with choices (server)
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
    
  })
}

