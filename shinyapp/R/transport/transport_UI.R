

# -------------------------------------
# UI items section
# -------------------------------------

# -- panel
transport_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    p("Transport"),
    
    # + button
    actionButton(inputId = ns("add_transport"),
                 label = "",
                 icon = icon(name = "plus", lib = "font-awesome")),
    
    kitems::create_BTN(ns("transport")))
  
}