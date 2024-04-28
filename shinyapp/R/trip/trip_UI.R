

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
    
    # -- title
    h4("Trips"),
    
    # -- create btn
    kitems::create_BTN("tripmngr-trip"),
    
    # -- trip selector
    selectizeInput(inputId = ns("trip_selector"), label = "Select trip", choices = NULL),
    
    # -- display options
    radioButtons(inputId = ns("display_options"), label = "", choiceNames = list(icon("compass-drafting"), icon("circle-check"), icon("layer-group")), choiceValues = list("plan", "done", "all"), inline = TRUE),
    
    # -- hide / show
    checkboxInput(inputId = ns("hide_show"), label = "Hide / Show", value = TRUE),
    
    # -- line break
    hr(),
    
    # -- Add buttons
    p("Add"),
    actionButton(inputId = ns("add_transport"), label = "", icon = icon("route")),
    actionButton(inputId = ns("add_accomodation"), label = "", icon = icon("bed")),
    
    # -- add_transport zone
    uiOutput(ns("transport_zone")),
    uiOutput(ns("accomodation_zone"))
    
    )
  
}


tmp_trip_ui <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # --
  tagList(
    
    h4("Info"),
    verbatimTextOutput(ns("tmp_trip_date")),
    
    h4("Transports"),
    verbatimTextOutput(ns("tmp_trip_1")),
    
    h4("Accomodation"),
    verbatimTextOutput(ns("tmp_accomodation_1")))
  
}
