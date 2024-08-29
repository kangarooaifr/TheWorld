

# -- function definition
route_search <- function(routes, pattern, airports, seaports){
  
  # -- check for empty string (otherwise the whole df is returned)
  if(identical(pattern, ""))
    NULL
  
  else {
    
    cat("[route_search] Search pattern =", pattern, "\n")
    
    # -- filter items
    result <- routes %>%
      filter_all(any_vars(grepl(pattern, .)))
    
    # -- air
    if(pattern == 'air'){
      
      result <- merge(result, airports[c("id", "iata")], by.x = "origin", by.y = "id")
      result <- merge(result, airports[c("id", "iata")], by.x = "destination", by.y = "id")
      names(result)[names(result) == 'iata.x'] <- 'origin.code'
      names(result)[names(result) == 'iata.y'] <- 'destination.code'
      
      # -- sea
    } else if(pattern == 'sea'){
      
      result <- merge(result, seaports[c("id", "name")], by.x = "origin", by.y = "id")
      result <- merge(result, seaports[c("id", "name")], by.x = "destination", by.y = "id")
      names(result)[names(result) == 'name.x'] <- 'origin.code'
      names(result)[names(result) == 'name.y'] <- 'destination.code'
      
    }
    
    # -- check
    cat("[route_search] output dim =", dim(result), "\n")
    
    # -- return
    result
    
  }
}
