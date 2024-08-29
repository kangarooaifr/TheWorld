

# -- function definition
map_click_observer <- function(r, mapId, coord_digits, location_ns){
  
  cat("[map_click_observer] mapId =", mapId, "\n")
  
  # -- get names
  map_click <- paste0(mapId, "_click")
  map_proxy <- paste0(mapId, "_proxy")
  
  # -- return (observer reference !)
  observeEvent(r[[map_click]](), {
    
    cat(paste0("[", mapId, "]"), "Map click event received \n")
    
    # -- get values
    lng <- r[[map_click]]()[['lng']]
    lat <- r[[map_click]]()[['lat']]
    
    # -- display popup
    r[[map_proxy]] %>% 
      clearPopups() %>%
      addPopups(lng, lat, 
                paste("Longitude:", round(lng, digits = coord_digits), br(),
                      "Latitude:", round(lat, digits = coord_digits), 
                      hr(),
                      actionLink(inputId = mapId, 
                                 label =  "add to my locations", 
                                 icon = icon("plus"),
                                 onclick = sprintf(
                                   'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                   paste(location_ns, "add_to_locations", sep = '-')))))
  })
  
}
