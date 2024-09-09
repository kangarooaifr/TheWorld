

# -- function definition
confirm_add_location_observer <- function(map, locations, input, confirm_add_location = "confirm_add_location"){
  
  cat("[confirm_add_location_observer] map id =", map$id, "\n")
  
  observeEvent(input$confirm_add_location, {
    
    # -- secure against empty locations #161
    req(input$name, input$type, input$country, input$city)
    
    # -- close dialog
    removeModal()
    
    # -- clear popup
    map$proxy %>% 
      clearPopups()
    
    # -- build values
    input_values <- data.frame(id = NA,
                               name = input$name,
                               type = input$type,
                               lng = map$click()[['lng']],
                               lat = map$click()[['lat']],
                               country = input$country,
                               state = input$state,
                               zip.code = input$zip.code,
                               city = input$city,
                               address = input$address,
                               comment = input$comment,
                               been.there = input$been.there,
                               wish.list = input$wish.list)
    
    # -- create item
    item <- kitems::item_create(values = input_values, data.model = locations$data_model())
    
    # -- call trigger
    kitems::item_add(locations$items, item, name = locations$id)
    
  })
  
}
