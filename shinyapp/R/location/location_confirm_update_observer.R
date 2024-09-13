

# -- function definition
location_confirm_update_observer <- function(mapId, input, locations){
  
  # -- Event: btn confirm_update_location
  observeEvent(input$confirm_update_location, {
    
    # -- close dialog
    cat(paste0("[", mapId, "]"), "Confirm add location \n")
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
