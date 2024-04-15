

# -------------------------------------
# UI items section
# -------------------------------------

# -- Where gone checkbox
country_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    h4("Countries"),
    p("Displays visited countries.", br(), "(locations marked as *been there*)"),
    
    # hide / show checkbox
    checkboxInput(ns("hide_show"), label = "Hide / Show", value = FALSE)
    
  )
  
}