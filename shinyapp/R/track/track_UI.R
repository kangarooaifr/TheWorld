

# -------------------------------------
# UI items section
# -------------------------------------

# -- Panel
track_UI <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # -- UI
  wellPanel(
    
    h4("Tracks"),
    
    # -- hide / show checkbox
    checkboxInput(ns("hide_show"), label = "tracks", value = FALSE, width = NULL))
  
}
