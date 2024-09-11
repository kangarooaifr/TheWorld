

# -------------------------------------
# UI functions
# -------------------------------------

# -- location panel
worldmap_INPUT <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # -- return
  wellPanel(
    
    # -- title
    h4("Locations"),
    p("Select options to customize display."),
    
    # -- country filter
    selectizeInput(inputId = ns("filter_country"), 
                   label = "Country", 
                   choices = NULL,
                   multiple = TRUE,
                   options = list(placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }'),
                                  create = FALSE)),
    
    # -- location options
    radioButtons(inputId = ns("display_options"), 
                 label = "Location type", 
                 choiceNames = list(icon("circle-check"), 
                                    icon("heart"), 
                                    icon("location-dot")), 
                 choiceValues = list("been-there", "wish-list", "all"), 
                 inline = TRUE))
  
}
