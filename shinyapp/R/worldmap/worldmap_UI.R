

# -- filter
country_INPUT <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # -- ui
  tagList(
    #div(style="display: inline-block", actionButton(inputId = ns("filter_country_reset"), label = "Reset filter")),
    div(style="display: inline-block", selectizeInput(inputId = ns("filter_country"), 
                                                      label = "Country", 
                                                      choices = NULL,
                                                      multiple = TRUE,
                                                      options = list(placeholder = 'Please select an option below',
                                                                     onInitialize = I('function() { this.setValue(""); }'),
                                                                     create = TRUE))))
  
}
