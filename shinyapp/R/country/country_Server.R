

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

country_Server <- function(id, r, path, map_proxy, filter_country) {
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
                        "X2digits.code" = "character",
                        "X3digits.code" = "character",
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
    r$geojson_data <- NULL
    
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
      
      # -- expose
      r$geojson_data <- geojson_data()
      
    })
    
  })
}

