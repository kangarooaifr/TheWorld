

# -------------------------------------
# UI functions
# -------------------------------------

# -- location panel
location_panel_UI <- function(id)
{
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  wellPanel(
    p("Locations"),
    uiOutput(ns("location_panel")),
    radioButtons(inputId = ns("show_location_option"), label = "", choiceNames = list(icon("circle-check"), icon("heart"), icon("location-dot")), choiceValues = list("been-there", "wish-list", "all"), inline = TRUE),
    actionButton(inputId = ns("show_location"), label = "Show locations"))
  
}
