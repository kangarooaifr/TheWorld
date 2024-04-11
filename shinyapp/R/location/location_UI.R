

# -------------------------------------
# UI functions
# -------------------------------------

# -- location panel
location_panel_UI <- function(id)
{
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  uiOutput(ns("location_panel"))
  
}

# -- location panel
location_form_UI <- function(id)
{
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  uiOutput(ns("add_location_form"))
  
}


# -- location markers
show_location_BTN <- function(id)
{
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  tagList(
    p("Locations"),
    radioButtons(inputId = ns("show_location_option"), label = "", choiceNames = list(icon("circle-check"), icon("heart"), icon("location-dot")), choiceValues = list("been-there", "wish-list", "all"), inline = TRUE),
    actionButton(inputId = ns("show_location"), label = "Show locations"))
  
}
