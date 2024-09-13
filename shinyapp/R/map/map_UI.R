

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
map_search_Input <- function(id) {
  
  # -- namespace
  ns <- NS(id)
  
  # -- return
  wellPanel(
    
    # -- search input
    searchInput(ns("search"), label = "Search", value = "", placeholder = "Enter search string", width = '100%'),
    
    p("Search on map uses OpenStreetMap API."))
  
}


# -- freeze
map_freeze_INPUT <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  div(style="display: inline-block", checkboxInput(inputId = ns("map_freeze"), label = icon("anchor")))
  
}
