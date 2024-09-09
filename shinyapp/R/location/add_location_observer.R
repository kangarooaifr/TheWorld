

# -- function definition
add_location_observer <- function(map, input, add_location = "add_location", choices, ns){
  
  cat("[add_location_observer] map id =", map$id, "\n")
  
  # -- return (observer reference !)
  observeEvent(input$add_location, {
    
    # -- get lng, lat
    lng <- map$click()[['lng']]
    lat <- map$click()[['lat']]
    
    # -- display form
    showModal(location_modal(location = NULL, lng, lat, choices(), ns))
    
  })
  
}
