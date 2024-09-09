

# -- function definition
location_update_observer <- function(mapId, input, locations, choices, ns){
  
  # -- Observe: action_update
  observeEvent(input$action_update, {
    
    # -- extract id
    id <- unlist(strsplit(input$action_update, split = "_"))[2]
    cat(paste0("[", mapId, "]"), "Marker popup click: update id =", id, "\n")
    
    # -- get location to update
    location <- locations$items()[locations$items()$id == id, ]
    
    # -- display form
    showModal(location_modal(location, choices = choices(), ns = ns))
    
  })
  
}
