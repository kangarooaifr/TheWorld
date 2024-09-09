

action_beenthere_observer <- function(input, map, locations){
  
  # -- Observe: action_beenthere
  observeEvent(input$action_beenthere, {
    
    MODULE <- paste0("[", map$id, "]")
    
    # -- extract id
    id <- unlist(strsplit(input$action_beenthere, split = "_"))[2]
    cat(MODULE, "Marker popup click: been-there id =", id, "\n")
    
    # -- update item
    location <- locations$items()[locations$items()$id == id, ]
    location$been.there <- TRUE
    location$wish.list <- FALSE
    
    # -- update location
    kitems::item_update(locations$items, location, name = locations$id)
    
  })
  
}
