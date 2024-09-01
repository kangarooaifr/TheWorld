

# -- function definition
map_click_observer <- function(map, coord_digits, location_ns){
  
  cat("[map_click_observer] map id =", map$id, "\n")
  
  # -- return (observer reference !)
  observeEvent(map$click(), {
    
    cat(paste0("[", map$id, "]"), "Map click event received \n")
    
    # -- get values
    lng <- map$click()[['lng']]
    lat <- map$click()[['lat']]
    
    # -- display popup
    map$proxy %>% 
      clearPopups() %>%
      addPopups(lng, lat, 
                paste("Longitude:", round(lng, digits = coord_digits), br(),
                      "Latitude:", round(lat, digits = coord_digits), 
                      hr(),
                      actionLink(inputId = map$id, 
                                 label =  "add to my locations", 
                                 icon = icon("plus"),
                                 onclick = sprintf(
                                   'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                   paste(location_ns, "add_to_locations", sep = '-')))))
  })
  
}
