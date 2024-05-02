

location_icon <- function(locations){
  
  locations <- locations %>%
    mutate(icon = case_when(been.there ~ 'been.there',
                            wish.list ~ 'wish.list',
                            type == 'Port' ~ 'port',
                            type == 'Airport' ~ 'airport',
                            type == 'Accommodation' ~ 'bed'))
  
}