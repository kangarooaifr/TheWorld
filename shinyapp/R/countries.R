

# --------------------------------------------------------------------------------
# Shiny module: countries
# --------------------------------------------------------------------------------

# -- COMMENTS
# TODO: rework this, should probably be part of wheregone/country !?

# -- Library
library(geojsonio)

# -- Source dependencies


# -------------------------------------
# UI items section
# -------------------------------------

# -- Where gone checkbox
countries_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # hide / show checkbox
    checkboxInput(ns("submit_countries"), label = "Countries", value = FALSE, width = NULL)
    
  )
  
}


# -------------------------------------
# Server logic
# -------------------------------------

countries_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- load data
    WorldCountry <- geojson_read(file.path(path$resources, "countries.geojson"), what = "sp")

    # -- countries    
    countries <- reactive(unique(r$whereGone()$country))
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
  
    observeEvent(countries(), {
                 
                 data_Map <- WorldCountry[WorldCountry@data$ADMIN %in% countries(), ]
                 
                 r$proxymap %>% 
                   addPolygons(data = data_Map, weight = 1, color = "red", group = "countries")
                 
                 })
    
    
    # -- Observe checkbox
    observeEvent(input$submit_countries, {

      # checkbox marked
      if(input$submit_countries){

        cat("Show group: countries \n")

        # proxy map
        r$proxymap %>%

          # Show group
          showGroup('countries')

      }else{

        cat("Hide group: countries \n")

        # proxy map
        r$proxymap %>%

          # clear group
          hideGroup('countries')
      }

    })
    
  })
}

