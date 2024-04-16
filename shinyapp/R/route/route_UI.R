

# -------------------------------------
# UI items section
# -------------------------------------

# -- panel
route_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    p("Route"),
    
    # + button
    actionButton(inputId = ns("add_route"),
                 label = "",
                 icon = icon(name = "plus", lib = "font-awesome")),
    
    # -- radio btn
    radioButtons(inputId = ns("show_route_option"), 
                 label = "", 
                 choiceNames = list(icon("plane"), icon("train"), icon("ship"), icon("bus")), 
                 choiceValues = list("air", "train", "sea", "road"), 
                 inline = TRUE),
    
    # -- display btn
    actionButton(inputId = ns("show_route"), label = "Show route"),
    
    # -- show / hide
    checkboxInput(inputId = ns("hide_show"), label = "Hide / Show")
    
    )
  
}