

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
    
    # -- get namespace
    ns <- session$ns

    # -- files
    filename_iso <- "countries.csv"
    filename_geojson <- "countries.geojson"
    
    # -- ids
    group_id <- "countries"
    
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
    countries_iso <- kfiles::read_data(file = filename_iso, 
                                       path = path$resources,
                                       colClasses = colClasses_iso,
                                       create = FALSE)
    
    # -- expose
    r$countries_iso <- countries_iso
    
    
        
    # -- load data
    cat("[countries] Read countries geojson data... \n")
    countries_geojson <- geojson_read(file.path(path$resources, filename_geojson), what = "sp")

    # -- countries    
    r_items <- "location_items"
    countries <- reactive(unique(r[[r_items]]()[r[[r_items]]()$been.there, 'country']))
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
    
    observeEvent({
      countries()
      r$filter_country}, {
        
        selected_countries <- countries()
        
        if(!is.null(r$filter_country))
          selected_countries <- selected_countries[selected_countries %in% r$filter_country]
        
        data_map <- countries_geojson[countries_geojson@data$ADMIN %in% selected_countries, ]
        
        r$proxymap %>%
          
          # -- cleanup
          clearGroup("countries") %>%
          
          # -- add areas
          addPolygons(data = data_map, weight = 1, color = "red", group = "countries")
        
      }, ignoreNULL = FALSE)
    
    
    # -- Observe checkbox
    observeEvent(input$hide_show, {

      # checkbox marked
      if(input$hide_show){

        cat("Show group:", group_id, "\n")

        # proxy map
        r$proxymap %>%

          # Show group
          showGroup(group_id)

      }else{

        cat("Hide group:", group_id, "\n")

        # proxy map
        r$proxymap %>%

          # clear group
          hideGroup(group_id)
      }

    })
    
  })
}

