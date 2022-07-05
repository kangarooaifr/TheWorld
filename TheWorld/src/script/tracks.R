

# --------------------------------------------------------------------------------
# Shiny module: tracks
# --------------------------------------------------------------------------------

# -- Library
library(sf)
library(ggplot2)
library(leaflet)

# -- Source dependencies


# -------------------------------------
# UI items section
# -------------------------------------

# -- Where gone checkbox
tracks_UI <- function(id){
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # hide / show checkbox
    checkboxInput(ns("submit_tracks"), label = "tracks", value = FALSE, width = NULL)
    
  )
  
}


# -------------------------------------
# Server logic
# -------------------------------------

tracks_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- prepare
    # file
    gpx_file <- file.path(path$data, "gpx/Randonn_e__04_09_2021_17_07.gpx")
    
    
    # check available layers
    #st_layers(gpx_file)
    
    
    # -- load track
    #r$airports <- reactiveVal(read.data(path$resource, filename, cols))
    track1 <- reactive(read_sf(gpx_file, layer = "track_points"))
    
    observeEvent(track1(), {
      
      # Convert track
      track2 <- track1() %>%
        st_combine() %>%
        st_cast(to = "LINESTRING") %>%
        st_sf()
      
      # add to leaflet map
      r$proxymap %>%
        addPolylines(data = track2, group = "tracks")
    })

    
    # -------------------------------------
    # Event observers
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$submit_tracks, {
      
      # checkbox marked
      if(input$submit_tracks){
        
        cat("Show group: tracks \n")
        
        # proxy map
        r$proxymap %>%
          
          # Show group
          showGroup('tracks')
        
      }else{
        
        cat("Hide group: tracks \n")
        
        # proxy map
        r$proxymap %>%
          
          # clear group
          hideGroup('tracks')
      }
      
    })
    
  })
}
