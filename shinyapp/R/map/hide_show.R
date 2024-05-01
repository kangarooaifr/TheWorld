

hide_show <- function(proxy, id, show = TRUE){
  
  # checkbox marked
  if(show){
    
    cat("[map] Show group:", id, "\n")
    
    # proxy map
    proxy %>%
      
      # Show group
      showGroup(id)
    
  }else{
    
    cat("[map] Hide group:", id, "\n")
    
    # proxy map
    proxy %>%
      
      # clear group
      hideGroup(id)
  }
  
}
