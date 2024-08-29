

# ------------------------------------------------------------------------------
# Shiny module: tracks
# ------------------------------------------------------------------------------

# -- Library
library(sf)
library(leaflet)


# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

track_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    
    # -- trace
    MODULE <- paste0("[", id, "]")
    cat(MODULE, "Starting module server... \n")
    
    
    # --------------------------------------------------------------------------
    # Communication objects
    # --------------------------------------------------------------------------
    
    # -- declare
    r$track <- NULL
    
    
    # --------------------------------------------------------------------------
    # Load resources
    # --------------------------------------------------------------------------
    
    # -- prepare file
    gpx_file <- file.path(path$data, "gpx/Randonn_e__04_09_2021_17_07.gpx")
    
    
    # check available layers
    #st_layers(gpx_file)
    
    
    # -- load track
    track1 <- reactive(read_sf(gpx_file, layer = "track_points"))
    
    observeEvent(track1(), {
      
      cat(MODULE, "Display tracks on map \n")
      
      # Convert track
      track2 <- track1() %>%
        st_combine() %>%
        st_cast(to = "LINESTRING") %>%
        st_sf()
      
      # -- expose
      r$track <- track2
      
    })
    
  })
}
