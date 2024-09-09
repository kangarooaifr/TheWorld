

map_layers_control <- function(layer_control, baseGroups = NULL, overlayGroups = NULL, remove = FALSE){

  # -- check
  if(is.null(baseGroups) & is.null(overlayGroups))
    return(layer_control)
  
  # -- check
  if(remove){
    
    cat("[layer_control] Remove groups =", paste(overlayGroups, sep = ","), "\n")
    
    # -- baseGroups
    if(!is.null(baseGroups))
      baseGroups <- layer_control()$baseGroups[!layer_control()$baseGroups %in% baseGroups]
    
    # -- overlayGroups
    if(!is.null(overlayGroups))
      overlayGroups <- layer_control()$overlayGroups[!layer_control()$overlayGroups %in% overlayGroups]
      
  } else {
    
    # -- baseGroups
    if(!is.null(baseGroups))
      baseGroups <- c(layer_control()$baseGroups, baseGroups[!baseGroups %in% layer_control()$baseGroups])
    
    # -- overlayGroups
    if(!is.null(overlayGroups))
      overlayGroups <- c(layer_control()$overlayGroups, overlayGroups[!overlayGroups %in% layer_control()$overlayGroups])
    
  }
  
  # -- store
  layer_control(list(baseGroups = baseGroups,
                     overlayGroups = overlayGroups))
  
}
