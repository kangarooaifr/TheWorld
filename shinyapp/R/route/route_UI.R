

# -------------------------------------
# UI items section
# -------------------------------------

# -- panel
route_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    h4("Routes"),
    
    # + button
    actionButton(inputId = ns("add_route"),
                 label = "",
                 icon = icon(name = "plus", lib = "font-awesome")),
    
    # -- radio btn
    radioButtons(inputId = ns("transport_mode"), 
                 label = "", 
                 choiceNames = list(icon("plane"), icon("train"), icon("ship"), icon("bus")), 
                 choiceValues = list("air", "rail", "sea", "road"), 
                 inline = TRUE))

}
