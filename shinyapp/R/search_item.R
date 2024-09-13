

search_item <- function(items, pattern){
  
  # -- check for empty string (otherwise the whole df is returned)
  if(identical(pattern, ""))
    NULL
  
  else {
    
    cat("[search_item] Search string =", pattern, "\n")
    
    # -- filter items & return
    result <- items %>%
      filter_all(any_vars(grepl(pattern, .)))
    
    # -- check
    cat("[search_item] Search result, dim =", dim(result), "\n")
    
    # -- return
    result}
  
}
