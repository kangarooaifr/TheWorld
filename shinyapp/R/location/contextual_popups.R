

contextual_popups <- function(locations, ns){
  
  sprintf(
    paste0(
      
      "<b>", locations$name, "</b>",
      "<p>", "lng = ", round(locations$lng, digits = 3), " / lat = ", round(locations$lat, digits = 3), "</p>",
      
      hr(),
      
      actionLink(inputId = "add_%s",
                 label =  "Add to trip", 
                 onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', ns("add_to_trip")))),
    
    locations$id)
  
}
