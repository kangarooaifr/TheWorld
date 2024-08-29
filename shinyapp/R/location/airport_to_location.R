


airport_to_location <- function(airports){
  
  # -- turn airport df into location df 
  data.frame(id = airports$id,
             name = paste(airports$iata, airports$name),
             type = 'airport',
             lng = airports$lng,
             lat = airports$lat,
             country = airports$country,
             state = NA,
             zip.code = NA,
             city = airports$city,
             address = NA,
             comment = NA,
             been.there = FALSE,
             wish.list = FALSE)
  
}
