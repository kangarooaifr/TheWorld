

# -- function definition
tp_popups <- function(locations, ns, type = 'selected'){
  
  # -- Implement specific actions for airports & stations (new route from here, new route to here)
  #
  
  # -- init emtpy vector
  popups <- vector()
  
  # -- conditional feed (will extend vector with same length as df rows)
  # adding locked so that airports & stations are excluded #203
  popups[!locations$locked] <- sprintf(paste0(
    
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
  
  # -- replace generated NAs
  popups[is.na(popups)] <- ""
  
  # -- return
  popups
  
}
