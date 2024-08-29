

search_item <- function(r, id, pattern){
  
  # -- check for empty string (otherwise the whole df is returned)
  if(identical(pattern, ""))
    NULL
  
  else {
    
    # -- get item reference
    r_items <- kitems::items_name(id)
    
    cat("[search_item] items =", id, "/ Search string =", pattern, "\n")
    
    # -- filter items & return
    result <- r[[r_items]]() %>%
      filter_all(any_vars(grepl(pattern, .)))
    
    # -- check
    cat("[search_item] Search result, dim =", dim(result), "\n")
    
    # -- return
    result}
  
}
