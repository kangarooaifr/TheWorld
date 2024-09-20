
# -------------------------------------
# Select
# -------------------------------------
# query pattern:
# - numeric vector to select by id's
# - character string to select by column, type=air

# -- fucntion definition
route_select <- function(routes, query) {
  
  # -- check #204
  req(!is.null(query))
  
  cat("[route_select] Query =", as.character(query), "\n")

  # -- check: numeric input
  if(is.numeric(query)){
    
      # -- apply selection
      cat("-- select by id \n")
      routes <- routes[routes$id %in% query, ]
    
  } else {
    
    # -- split 'key=value' into c(key, value)
    query <- unlist(strsplit(query, split = "="))
    cat("-- select by key,", query[1], "=", query[2], "\n")
    
    # -- select
    routes <- routes[routes$query[1] %in% query[2], ]
    
  }
  
  # -- check
  cat("-- output dim =", dim(routes), "\n")
  
  # -- return
  routes
  
}
