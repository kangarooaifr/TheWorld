

# -- function definition
delete_location_observer <- function(map, input, locations){
  
  cat("[delete_location_observer] map id =", map$id, "\n")
  
  # -- Observe: action_delete
  observeEvent(input$action_delete, {
    
    # -- extract id
    id <- unlist(strsplit(input$action_delete, split = "_"))[2]
    cat(paste0("[", map$id, "]"), "Marker popup click: delete id =", id, "\n")
    
    # -- delete item
    kitems::item_delete(locations, id, name = "location")
    
  })
  
}
