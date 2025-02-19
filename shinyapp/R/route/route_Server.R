

# ------------------------------------------------------------------------------
# Shiny module: dependencies
# ------------------------------------------------------------------------------

# -- Library
library(geosphere)
library(dplyr)


# ------------------------------------------------------------------------------
# Module Server logic
# ------------------------------------------------------------------------------

route_Server <- function(id, routeId, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # get namespace
    ns <- session$ns
    
    
    # --------------------------------------------------------------------------
    # Data manager
    # --------------------------------------------------------------------------

    # -- launch kitems sub module
    routes <- kitems::kitemsManager_Server(id = routeId, r, path$data)
    
    
    # --------------------------------------------------------------------------
    # Add route
    # --------------------------------------------------------------------------
    
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
        
        # ********* To be reworked (r$airports is now locations$airports)
        # choices <- r$airports$id
        # names(choices) <- r$airports$iata
        
      } else if(input$transport_mode == "sea"){
        
        # ********* To be reworked (r$seaports is now locations$seaports)
        # choices <- r$seaports()$id
        # names(choices) <- paste(r$seaports()$city, r$seaports()$name, sep = "-")
        
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
        # Need to find another way to fire create !!
        # r[[r_trigger_create]](r[[r_trigger_create]]() + 1)
        
      })
      
    })
    
    
    # --------------------------------------------------------------------------
    # Return
    # --------------------------------------------------------------------------

    # -- return (the reactive reference, not the value!)
    routes
    
  })
}
