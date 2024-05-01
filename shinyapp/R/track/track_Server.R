

# ------------------------------------------------------------------------------
# Shiny module: tracks
# ------------------------------------------------------------------------------

# -- Library
library(sf)
library(leaflet)


# -------------------------------------
# Server logic
# -------------------------------------

track_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # get namespace
    ns <- session$ns
    
    # -- id
    group_id <- "tracks"
    
    # -- prepare
    # file
    gpx_file <- file.path(path$data, "gpx/Randonn_e__04_09_2021_17_07.gpx")
    
    
    # check available layers
    #st_layers(gpx_file)
    
    
    # -- load track
    track1 <- reactive(read_sf(gpx_file, layer = "track_points"))
    
    observeEvent(track1(), {
      
      # Convert track
      track2 <- track1() %>%
        st_combine() %>%
        st_cast(to = "LINESTRING") %>%
        st_sf()
      
      # add to leaflet map
      r$proxymap %>%
        
        # -- hidden by default
        hideGroup(group_id) %>%
        
        # -- add on map
        addPolylines(data = track2, group = group_id)

    })
    
    
    # -------------------------------------
    # Hide / Show
    # -------------------------------------
    
    # -- Observe checkbox
    observeEvent(input$hide_show, 
                 hide_show(proxy = r$proxymap, id = group_id, show = input$hide_show))

    
  })
}
