

# ------------------------------------------------------------------------------
# Shiny module: country
# ------------------------------------------------------------------------------

# -- Library
library(geojsonio)


# -------------------------------------
# Server logic
# -------------------------------------

country_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- load data
    cat("[countries] Read countries geojson data... \n")
    countries_geojson <- geojson_read(file.path(path$resources, "countries.geojson"), what = "sp")

    # -- countries    
    r_items <- "location_items"
    countries <- reactive(unique(r[[r_items]]()[r[[r_items]]()$been.there, 'country']))
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
  
    observeEvent(countries(), {
                 
                 data_map <- countries_geojson[countries_geojson@data$ADMIN %in% countries(), ]
                 
                 r$proxymap %>% 
                   addPolygons(data = data_map, weight = 1, color = "red", group = "countries")
                 
                 })
    
    
    # -- Observe checkbox
    observeEvent(input$hide_show, {

      # checkbox marked
      if(input$hide_show){

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

