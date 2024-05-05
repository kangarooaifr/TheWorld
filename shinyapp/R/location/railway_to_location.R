

# -- function definition
railway_to_location <- function(stations){
  
  # -- turn airport df into location df 
  data.frame(id = stations$id,
             name = stations$name,
             type = 'railway_station',
             lng = stations$lng,
             lat = stations$lat,
             country = stations$country,
             state = NA,
             zip.code = NA,
             city = stations$name,
             address = NA,
             comment = NA,
             been.there = FALSE,
             wish.list = FALSE)
  
}
