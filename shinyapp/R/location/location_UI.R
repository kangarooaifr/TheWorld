

# -------------------------------------
# UI functions
# -------------------------------------

# -- location panel
location_panel_UI <- function(id)
{
  
  # -- namespace
  ns <- NS(id)
  
  # -- return
  wellPanel(
    
    # -- title
    h4("Locations"),
    
    # -- display
    radioButtons(inputId = ns("display_options"), label = "", choiceNames = list(icon("circle-check"), icon("heart"), icon("location-dot")), choiceValues = list("been-there", "wish-list", "all"), inline = TRUE))
  
}
