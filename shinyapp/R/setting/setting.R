

setting <- function(name, type = NULL, value = NULL, default = NULL){
  
  # -- check if setting exists
  if(!name %in% settings$name){
    
    # -- check values
    if(is.null(type) | is.null(default)){
      
      cat("[setting] Warning! type & default arguments are mandatory to create new setting. \n")
      return(NULL)}
    
    # -- create setting
    new_setting <- data.frame(name = name,
                              type = type,
                              value = default,
                              default = default)
    
    # -- merge
    settings <<- if(is.null(settings))
      new_setting
    else 
      rbind(settings, new_setting)
    
    # -- return
    cat("[setting] Setting name =", name, paste0("[", type, "]"), "has been created, default =", default, "\n")
    default
    
  } else {
   
    # -- check value
    if(is.null(value)){
      
      # -- return
      cat("[setting] Setting name =", name, "/ value =", settings[settings$name == name, ]$value, "\n")
      settings[settings$name == name, ]$value
    
    } else {
      
      # -- update value
      settings[settings$name == name, ]$value <<- value
      
      # -- return
      cat("[setting] Setting name =", name, "has been updated, value =", value, "\n")
      value}
    
  }
  
}
