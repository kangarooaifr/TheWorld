

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
    
    # -- radio btn
    radioButtons(inputId = ns("show_transport_option"), 
                 label = "", 
                 choiceNames = list(icon("plane"), icon("ship"), icon("bus")), 
                 choiceValues = list("air", "sea", "road"), 
                 inline = TRUE),
    
    # -- display btn
    actionButton(inputId = ns("show_transport"), label = "Show transport"),
    
    # -- show / hide
    checkboxInput(inputId = ns("show_hide"), label = "Show / hide")
    
    )
  
}