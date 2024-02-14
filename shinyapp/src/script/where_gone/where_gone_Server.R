
# --------------------------------------------------------------------------------
# Shiny module: wheregone
# --------------------------------------------------------------------------------

# -- Library


# -------------------------------------
# Server logic
# -------------------------------------

whereGone_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- declare
    filename <- "where_gone.csv"
    cols <- c(country = "character",
              state = "character",
              city = "character",
              lng = "numeric",
              lat = "numeric")
    
    # -- load data
    r$whereGone <- reactiveVal(kfiles::read_data(file = filename,
                                                 path = path$data,
                                                 colClasses = cols,
                                                 create = FALSE))
    
    # -- add markers to map (hidden)
    observeEvent(r$whereGone(), {
      
      cat("Updating where gone markers \n")
      
      DEBUG_WHERE_GONE <<- r$whereGone()
      
      # get proxy map
      r$proxymap %>%
        
        # clean
        clearGroup("wheregone") %>%
        
        # Add markers
        addMarkers(data = r$whereGone(),
                   lng = ~lng,
                   lat = ~lat,
                   group = "wheregone",
                   label = ~as.character(paste("Ville du marker : ", city)),
                   popup = make_labels(r$whereGone()),
                   #clusterOptions = markerClusterOptions(),
                   clusterOptions = NULL
                   )
    })
    
    
    # -------------------------------------
    # Outputs
    # -------------------------------------
  
    
    # -------------------------------------
    # Event observers
    # -------------------------------------

    # -- Observe whereGone button
    observeEvent(input$submit_whereGone, {

      # checkbox marked
      if(input$submit_whereGone){
        
        cat("Show group: wheregone \n")
        
        # proxy map
        r$proxymap %>%
          
          # Show group
          showGroup('wheregone')
            
      }else{
        
        cat("Hide group: wheregone \n")
        
        # proxy map
        r$proxymap %>%
          
          # clear markers
          hideGroup('wheregone')
      }
      
    })
    
    
    # -- Observe new location button
    observeEvent(input$submit_addLocation, {
      
      # check
      req(input$country,
          input$state,
          input$city)
      
      # get map center
      center <- r$map_center
      lng <- center[['lng']]
      lat <- center[['lat']]
      
      # trace
      cat("New Point lng =", lng, "/ lat =", lat, "\n")
      
      # new location
      new_location <- data.frame(country = input$country,
                                 state = input$state,
                                 city = input$city,
                                 lng = lng,
                                 lat = lat)
      
      # add to list
      r$whereGone(rbind(r$whereGone(), new_location))
      
      # save
      write.data(r$whereGone(), path$data, filename)
      
    })
    
  })
}

