

# -- function definition
confirm_update_location_observer <- function(map, input, locations){
  
  cat("[confirm_update_location_observer] map id =", map$id, "\n")
  
  # -- Event: btn confirm_update_location
  observeEvent(input$confirm_update_location, {
    
    # -- close dialog
    removeModal()
    
    # -- extract location id
    id <- unlist(strsplit(input$action_update, split = "_"))[2]
    
    # -- get location to update
    location <- locations$items()[locations$items()$id == id, ]
    
    # -- update values
    location$name = input$name
    location$type = input$type
    location$country = input$country
    location$state = input$state
    location$zip.code = input$zip.code
    location$city = input$city
    location$address = input$address
    location$comment = input$comment
    location$been.there = input$been.there
    location$wish.list = input$wish.list
    
    # -- update location
    kitems::item_update(locations$items, location, name = locations$id)
    
  })
  
}
