

location_popups <- function(locations, ns, activity){
  
  cat("[location] Build standard popups \n")
  
  # -- compute header
  header <- paste0(
    
    "<b>", locations$name, "</b>","<br>",
    "lng = ", round(locations$lng, digits = 3), " / lat = ", round(locations$lat, digits = 3))
  
  
  # -- compute body
  body <- paste0(
    
    "<p>", locations$type, "<p>")

  
  # -- location
  footer <- sprintf(
    
    paste(
      actionLink(inputId = "update_%s", 
                 label =  "Update", 
                 onclick = sprintf(
                   'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                   ns("action_update"))),
      
      actionLink(inputId = "delete_%s", 
                 label =  "Delete", 
                 onclick = sprintf(
                   'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                   ns("action_delete")))),
    
    locations$id, locations$id)

  
  # -- merge all & return
  paste(header, body, paste("Actions", "<br/>", activity, footer), sep = "<hr/>")
  
}
