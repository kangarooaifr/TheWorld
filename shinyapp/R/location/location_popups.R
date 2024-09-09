

location_popups <- function(locations, type, activity, ns){
  
  cat("[location] Build popups, activity =", activity, "/ type =", type, "\n")
  
  # -- compute header
  header <- paste0(
    
    "<b>", locations$name, "</b>","<br>",
    "lng = ", round(locations$lng, digits = 3), " / lat = ", round(locations$lat, digits = 3))
  
  
  # -- compute body
  body <- paste0(
    
    "<p>", locations$type, "<p>")
  
  
  # -- compute footer
  footer <- if(activity == "world_map"){
    
    # -- world_map activity
    sprintf(
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
                     ns("action_beenthere")))),
      
      locations$id, locations$id, locations$id)
    
  } else {
    
    # -- trip_planner activity
    sprintf(
      paste0(
        
        if(type == 'selected')
          
          # -- remove from trip
          actionLink(inputId = "remove_%s",
                     label =  "Remove from trip", 
                     onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                       ns("remove_from_trip")))
        
        else
          
          # -- add to trip
          actionLink(inputId = "add_%s",
                     label =  "Add to trip", 
                     onclick = sprintf('Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})', 
                                       ns("add_to_trip")))),
      
      locations$id)
    
  }
  
  
  # -- merge all & return
  paste0(header, body, footer)
  
}
