

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
    div(style="display: inline-block;width: 50px;",HTML("<br>")),
    div(style="display: inline-block;", kitems::create_BTN("tripmngr-trip")),
    
    # -- display options
    #radioButtons(inputId = ns("display_options"), label = "", choiceNames = list(icon("compass-drafting"), icon("circle-check"), icon("layer-group")), choiceValues = list("plan", "done", "all"), inline = TRUE),
    
    # -- hide / show
    #checkboxInput(inputId = ns("hide_show"), label = "Hide / Show", value = TRUE),
    
    # -- line break
    hr(),
    
    # -- Add buttons
    p("Add"),
    actionButton(inputId = ns("add_location"), label = "", icon = icon("location-dot")),
    actionButton(inputId = ns("add_transport"), label = "", icon = icon("route")),
    actionButton(inputId = ns("add_accomodation"), label = "", icon = icon("bed")),
    
    # -- add_transport zone
    uiOutput(ns("shared_zone"))
    
    )
  
}


tmp_trip_ui <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  # --
  tagList(
    
    h4("Info"),
    verbatimTextOutput(ns("tmp_trip_date")),
    
    h4("Steps"),
    verbatimTextOutput(ns("tmp_step_1")),
    
    h4("Transports"),
    verbatimTextOutput(ns("tmp_trip_1")),
    
    h4("Accomodation"),
    verbatimTextOutput(ns("tmp_accomodation_1")))
  
}
