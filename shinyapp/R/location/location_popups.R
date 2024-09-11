

location_popups <- function(locations, type, activity, ns){
  
  cat("[location] Build popups, activity =", activity, "/ type =", type, "\n")
  
  # -- compute header
  header <- paste0(
    
    "<b>", locations$name, "</b>","<br>",
    "lng = ", round(locations$lng, digits = 3), " / lat = ", round(locations$lat, digits = 3))
  
  
  # -- compute body
  body <- paste0(
    
    "<p>", locations$type, "<p>")

  
  # -- location
  footer_location <- sprintf(
    
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

  
  # -- compute footer
  footer_activity <- if(activity == "world_map"){
    
    # -- been.there (init empty vector)
    link_been <- vector()
    
    # -- conditional feed (will extend vector with same length as df rows)
    link_been[!locations$been.there] <- sprintf(
      
      paste0(
        actionLink(inputId = "been-there_%s", 
                   label =  "Been there", 
                   onclick = sprintf(
                     'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                     ns("action_beenthere")))),
      
      locations[!locations$been.there, ]$id)
    
    # -- replace generated NAs
    link_been[is.na(link_been)] <- ""
    
    
    # -- wish.list (init empty vector)
    link_wish <- vector()
    
    # -- conditional feed
    link_wish[!locations$wish.list] <- sprintf(
      
      paste0(
        actionLink(inputId = "wish-list_%s", 
                   label =  "Wish list", 
                   onclick = sprintf(
                     'Shiny.setInputValue(\"%s\", this.id, {priority: \"event\"})',
                     ns("action_wishlist")))),
      
      locations[!locations$wish.list, ]$id)
    
    # -- replace generated NAs
    link_wish[is.na(link_wish)] <- ""
    
    # -- merge & return
    paste(link_been, link_wish)
    
    
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
  paste(header, body, paste("Actions", "<br/>", footer_location, footer_activity), sep = "<hr/>")
  
}
