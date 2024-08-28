

search_item <- function(r, id, search_string){
  
  # -- check for empty string (otherwise the whole df is returned)
  if(identical(search_string, ""))
    NULL
  
  else {
    
    # -- get item reference
    r_items <- kitems::items_name(id)
    
    cat("[search_item] items =", id, "/ Search string =", search_string, "\n")
    
    # -- filter items & return
    result <- r[[r_items]]() %>%
      filter_all(any_vars(grepl(search_string, .)))
    
    # -- check
    cat("[search_item] Search result, dim =", dim(result), "\n")
    
    # -- return
    result}
  
}
