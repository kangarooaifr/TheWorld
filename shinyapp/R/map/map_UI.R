
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


# -- filter
country_INPUT <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # -- UI
  selectizeInput(inputId = ns("filter_country"), 
                 label = "Country", 
                 choices = c("France", "Norway"),
                 options = list(placeholder = 'Please select an option below',
                                onInitialize = I('function() { this.setValue(""); }'),
                                create = TRUE))
  
}

