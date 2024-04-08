

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    # -- launch kitems sub module
    kitems::kitemsManager_Server(id = "location", r, path)
  
    # -- 
    observeEvent(r$map_click, {
      
      cat("map click", str(r$map_click), "\n")
      
    })
    
  })
}
