

# ------------------------------------------------------------------------------
# Shiny module: country
# ------------------------------------------------------------------------------

# -- Library
library(geojsonio)
library(promises)
library(future)


# -------------------------------------
# Server logic
# -------------------------------------

country_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns

    # -- files
    filename_iso <- "countries.csv"
    filename_geojson <- "countries.geojson"
    
    # -- ids
    group_id <- "countries"
    
    
    # -------------------------------------
    # Init
    # -------------------------------------
    
    # -- Panel (waiting for file to be loaded)
    output$panel_ui <- renderUI(
      wellPanel(
        h4("Countries"),
        p("Loading country boundaries in progres...")))
    
    
    # -------------------------------------
    # Load resources: iso countries
    # -------------------------------------
    
    # -- colClasses
    colClasses_iso <- c("id" = "numeric",
                        "country.en" = "character",
                        "2digits.code" = "character",
                        "3digits.code" = "character",
                        "numeric.code" = "numeric",
                        "latitude" = "numeric",
                        "longitude" = "numeric")
    
    # -- load country ISO list
    r$countries_iso <- kfiles::read_data(file = filename_iso, 
                                       path = path$resources,
                                       colClasses = colClasses_iso,
                                       create = FALSE)

    
    # -------------------------------------
    # Load resources: geojson data
    # -------------------------------------
    
    # -- set async strategy
    plan(multisession)
    
    # -- init
    geojson_data <- reactiveVal()
    
    # -- async read data
    cat("[countries] Asynchronous -- start reading countries geojson data... \n")
    future(
      geojson_read(file.path(path$resources, filename_geojson), what = "sp")
    ) %...>%
      geojson_data() %...!%  # -- Assign to geojson_data
      (function(e) {
        geojson_data(NULL)
        warning(e)
        session$close()
      }) # -- if error
      
    
    # -- observe when geojson_data is ready
    observeEvent(geojson_data(), {
      
      cat("[countries] Asynchronous -- read countries geojson data done. \n")
      
      # -- notify user
      showNotification("Country boundaries are now available", type = c("message"))
      
      # -- update ui
      output$panel_ui <- renderUI({
      
        wellPanel(
          
          h4("Countries"),
          p("Displays visited countries.", br(), "(locations marked as *been there*)"),
          
          # hide / show checkbox
          checkboxInput(ns("hide_show"), label = "Hide / Show", value = FALSE))})
      
    })
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
    
    observeEvent({
      geojson_data()
      r$visited_countries()
      r$filter_country}, {

        # -- because ignoreNULL = FALSE
        # need to wait for async data to be ready
        req(geojson_data())

        # -- get visited countries
        selected_countries <- r$visited_countries()
        
        # -- apply filter
        if(!is.null(r$filter_country))
          selected_countries <- selected_countries[selected_countries %in% r$filter_country]
        
        # -- switch to country code
        # WARNING! the column name is switched to X3digits.code upon reading the file
        selected_countries <- r$countries_iso[r$countries_iso$country.en %in% selected_countries, 'X3digits.code']
        
        # -- selected geojson to be displayed
        selected_geojson <- geojson_data()[geojson_data()@data$ISO_A3 %in% selected_countries, ]

        # -- update map
        r$proxymap %>%

          # -- cleanup
          clearGroup("countries") %>%

          # -- add areas
          addPolygons(data = selected_geojson, weight = 1, color = "red", group = "countries")

      }, ignoreNULL = FALSE)
    
    
    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$hide_show, 
                 hide_show(proxy = r$proxymap, id = group_id, show = input$hide_show))
    
    
  })
}

