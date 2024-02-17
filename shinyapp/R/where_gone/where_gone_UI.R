

# ------------------------------------------------------------------------------
# UI items section
# ------------------------------------------------------------------------------

# -- Where gone
whereGone_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # hide / show checkbox
    checkboxInput(ns("submit_whereGone"), label = "Where gone", value = TRUE, width = NULL),
    
    # new location
    textInput(ns("country"), "country", value = "", width = 300, placeholder = "country"),
    textInput(ns("state"), "state", value = "", width = 300, placeholder = "state"),
    textInput(ns("city"), "city", value = "", width = 300, placeholder = "city"),
    actionButton(ns("submit_addLocation"), label = "Add Mark")
    
  )
  
}
