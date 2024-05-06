

location_popups <- function(locations, type, activity, ns){
  
  cat("[location] Build popups, activity =", activity, "/ type =", type, "\n")
  
  sprintf(
    paste0(
      
      "<b>", locations$name, "</b>",
      "<p>", "lng = ", round(locations$lng, digits = 3), " / lat = ", round(locations$lat, digits = 3), "</p>",
      
      hr(),
      
      # -- conditional actions
      if(activity == "world_map"){
        
        paste0(
          actionLink(inputId = "update_%s", 
                     label =  "Update", 
                     onclick = sprintf(
                       'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                       ns("action_update"))),
          
          actionLink(inputId = "delete_%s", 
                     label =  "Delete", 
                     onclick = sprintf(
                       'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                       ns("action_delete"))),
          
          actionLink(inputId = "been-there_%s", 
                     label =  "Been there", 
                     onclick = sprintf(
                       'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                       ns("action_beenthere"))))
        
      } else {
        
        actionLink(inputId = "add_%s",
                   label =  "Add to trip", 
                   onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', ns("add_to_trip")))
        
      }),
    
    locations$id, locations$id, locations$id)
  
}
