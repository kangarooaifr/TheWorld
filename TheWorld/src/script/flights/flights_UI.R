
# -------------------------------------
# UI items section
# -------------------------------------

# -- Where gone checkbox
flights_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # hide / show checkbox
    actionButton(inputId = ns("print_flights"),
                 label = "print flights"),
    
    checkboxInput(inputId =  ns("submit_flights"), 
                  label = "flights", 
                  value = FALSE, 
                  width = NULL)
    
  )
  
}