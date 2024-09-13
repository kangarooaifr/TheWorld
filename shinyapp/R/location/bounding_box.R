

bounding_box <- function(x, bounds){
  
  cat("Filter by bounding box, input dim =", dim(x), "\n")
  
  # -- apply filter
  x[x$lng > bounds$west &
      x$lng < bounds$east &
      x$lat > bounds$south &
      x$lat < bounds$north, ]
  
}
