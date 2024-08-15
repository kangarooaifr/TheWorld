

# -------------------------------------
# UI items section
# -------------------------------------

# -- Main map
map_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # map
  leafletOutput(ns("map"), height = 800)
  
}


# -------------------------------------
# Input items section
# -------------------------------------

# -- Search input form
search_Input <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # search input
    searchInput(ns("search"), label = "Search", value = ""))
  
}


# -- freeze
freeze_INPUT <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  div(style="display: inline-block", checkboxInput(inputId = ns("map_freeze"), label = icon("anchor")))
  
}
