

settingsAdmin_UI <- function(id){
  
  # -- get namespace
  ns <- NS(id)
  
  # -- return
  tagList(
    
    # -- title
    h3("settings"),
    
    # -- from server
    uiOutput(ns("settings_admin")))
  
}
