

step_order <- function(steps){
  
  if(dim(steps)[1] == 0)
    return(1)
  else
    max(steps$order) + 1
  
}