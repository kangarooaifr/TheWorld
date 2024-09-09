

# -- function definition
location_delete_observer <- function(mapId, input, locations){
  
  # -- Observe: action_delete
  observeEvent(input$action_delete, {
    
    # -- extract id
    id <- unlist(strsplit(input$action_delete, split = "_"))[2]
    cat(paste0("[", mapId, "]"), "Marker popup click: delete id =", id, "\n")
    
    # -- delete item
    kitems::item_delete(locations$items, id, name = "location")
    
  })
  
}
