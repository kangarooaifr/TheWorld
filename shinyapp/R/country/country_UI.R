

# -------------------------------------
# UI items section
# -------------------------------------

# -- Where gone checkbox
country_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    p("Countries"),
    p("Displays already visited countries."),
    
    # hide / show checkbox
    checkboxInput(ns("hide_show"), label = "Hide / show countries", value = FALSE, width = NULL)
    
  )
  
}