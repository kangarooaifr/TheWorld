

# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

location_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -- get namespace
    ns <- session$ns
    
    
    
    
    # -- location table
    # id, name, lng, lat, ... city/country?, beenthere, wishlist, type
    
    # -- 
    kitems::kitemsManager_Server(id = "location", r, path)
    
    
    
  })
}
