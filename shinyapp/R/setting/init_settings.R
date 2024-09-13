

init_settings <- function(path, filename){
  
  cat("[setting] Init settings \n")
  
  # -- check path
  if(!dir.exists(path))
    dir.create(path, showWarnings = FALSE)
  
  # -- check file & read
  if(file.exists(file.path(path, filename))){
    
    # -- read
    cat("-- Reading settings file \n")
    readRDS(file = filename)
  
  } else NULL
  
}
