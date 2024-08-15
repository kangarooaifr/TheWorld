

# --------------------------------------------------------------------------
# map_crop
# --------------------------------------------------------------------------

# -- observe trigger
map_crop <- function(map_proxy, lng1, lat1, lng2, lat2, fly_duration, fly_padding){
  
  # -- trace
  cat("[map_crop] Applying flyToBounds \n")
  
  # -- crop view
  map_proxy %>%
    flyToBounds(lng1, lat1, lng2, lat2, 
                options = list(duration = fly_duration, 
                               padding = c(fly_padding, fly_padding)))
  
}
