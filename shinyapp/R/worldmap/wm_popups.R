

# -- function definition
wm_popups <- function(locations, ns){
  
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
  
}
