

# -------------------------------------
# UI functions
# -------------------------------------

# -- filter
country_INPUT <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  tagList(
    div(style="display: inline-block", selectizeInput(inputId = ns("filter_country"), 
                                                      label = "Country", 
                                                      choices = NULL,
                                                      multiple = TRUE,
                                                      options = list(placeholder = 'Please select an option below',
                                                                     onInitialize = I('function() { this.setValue(""); }'),
                                                                     create = FALSE))))
  
}


# -- location panel
location_INPUT <- function(id)
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
