

# ------------------------------------------------------------------------------
# Shiny module: country
# ------------------------------------------------------------------------------

# -- Library
library(geojsonio)
library(promises)
library(future)


# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

country_Server <- function(id, path) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    # -- files
    filename_iso <- "countries.csv"
    filename_geojson <- "countries.geojson"

    
    # --------------------------------------------------------------------------
    # Communication objects
    # --------------------------------------------------------------------------
    
    # -- geojson
    geojson_data <- reactiveVal()
    
    
    # --------------------------------------------------------------------------
    # Load resources: iso countries
    # --------------------------------------------------------------------------
    
    # -- colClasses
    colClasses_iso <- c("id" = "numeric",
                        "country.en" = "character",
                        "X2digits.code" = "character",
                        "X3digits.code" = "character",
                        "numeric.code" = "numeric",
                        "latitude" = "numeric",
                        "longitude" = "numeric")
    
    # -- load country ISO list
    iso <- kfiles::read_data(file = filename_iso, 
                             path = path$resources,
                             colClasses = colClasses_iso,
                             create = FALSE)

    
    # --------------------------------------------------------------------------
    # Load resources: geojson data
    # --------------------------------------------------------------------------
    
    # -- set async strategy
    plan(multisession)
    
    # -- async read data
    cat(MODULE, "Asynchronous -- start reading countries geojson data... \n")
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
      
      cat(MODULE, "Asynchronous -- read countries geojson data done. \n")
      
      # -- notify user
      showNotification("Country boundaries are now available", type = c("message"))
      
    })
    
    
    # -- module return value
    list(iso = iso,
         geojson = geojson_data)
    
  })
}

