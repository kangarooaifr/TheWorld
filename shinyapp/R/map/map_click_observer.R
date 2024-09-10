

# -- function definition
map_click_observer <- function(map, ns, coord_digits){
  
  # -- return (observer reference !)
  observeEvent(map$click(), {
    
    cat(paste0("[", map$id, "]"), "Map click event received \n")
    
    # -- get values
    lng <- map$click()[['lng']]
    lat <- map$click()[['lat']]
    
    # -- get address info from coordinates
    address <- reverse_geocoding(lng, lat)
    
    # -- display popup
    map$proxy %>% 
      clearPopups() %>%
      addPopups(lng, lat, 
                paste("Longitude:", round(lng, digits = coord_digits), br(),
                      "Latitude:", round(lat, digits = coord_digits), 
                      hr(),
                      "address:", address$display_name,
                      hr(),
                      actionLink(inputId = map$id, 
                                 label =  "add to my locations", 
                                 icon = icon("plus"),
                                 onclick = sprintf(
                                   'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                   ns("action_add")))))
  })
  
}
