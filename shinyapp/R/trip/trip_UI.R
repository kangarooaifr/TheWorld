

# -------------------------------------
# UI functions
# -------------------------------------

# -- panel
trip_panel_UI <- function(id)
{
  
  # -- namespace
  ns <- NS(id)
  
  # -- return
  wellPanel(
    
    # -- trip selector, create btn
    div(style="display: inline-block;width: 200px;", selectizeInput(inputId = ns("trip_selector"), label = "Select trip", choices = NULL)),
    div(style="display: inline-block;width: 20px;",HTML("<br>")),
    div(style="display: inline-block;", kitems::create_BTN("tripmngr-trip")),
    
    # -- display options
    #radioButtons(inputId = ns("display_options"), label = "", choiceNames = list(icon("compass-drafting"), icon("circle-check"), icon("layer-group")), choiceValues = list("plan", "done", "all"), inline = TRUE),
    
    # -- general info
    hr(),
    uiOutput(ns("trip_info")),
    
    # -- transport
    hr(),
    uiOutput(ns("trip_transport")),
    
    # -- accommodation
    hr(),
    uiOutput(ns("trip_accommodation")),
    
    # -- line break
    hr(),
    
    # -- Add buttons
    p("Add"),
    actionButton(inputId = ns("add_location"), label = "", icon = icon("location-dot")),
    actionButton(inputId = ns("add_transport"), label = "", icon = icon("route")),
    actionButton(inputId = ns("add_accommodation"), label = "", icon = icon("bed")),
    
    # -- add_transport zone
    uiOutput(ns("shared_zone")),
    
    # -- test
    uiOutput(ns("mywidget"))
    
    )
  
}
