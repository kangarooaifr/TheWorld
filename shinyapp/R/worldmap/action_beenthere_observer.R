

action_beenthere_observer <- function(mapId, input, locations){
  
  # -- Observe: action_beenthere
  observeEvent(input$action_beenthere, {

    # -- extract id
    id <- unlist(strsplit(input$action_beenthere, split = "_"))[2]
    cat(paste0("[", mapId, "]"), "Marker popup click: been-there id =", id, "\n")
    
    # -- update item
    location <- locations$items()[locations$items()$id == id, ]
    location$been.there <- TRUE
    location$wish.list <- FALSE
    
    # -- update location
    kitems::item_update(locations$items, location, name = locations$id)
    
  })
  
}
