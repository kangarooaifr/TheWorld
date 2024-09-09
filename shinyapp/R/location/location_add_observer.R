

# -- function definition
location_add_observer <- function(map, input, choices, ns){
  
  # -- return (observer reference !)
  observeEvent(input$action_add, {
    
    cat(paste0("[", map$id, "]"), "Marker popup click / add location \n")
    
    # -- get lng, lat
    lng <- map$click()[['lng']]
    lat <- map$click()[['lat']]
    
    # -- display form
    showModal(location_modal(location = NULL, lng, lat, choices(), ns))
    
  })
  
}
