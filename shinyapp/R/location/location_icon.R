

location_icon <- function(locations){
  
  locations <- locations %>%
    dplyr::mutate(icon = dplyr::case_when(been.there ~ 'been.there',
                                          wish.list ~ 'wish.list',
                                          type == 'seaport' ~ 'seaport',
                                          type == 'airport' ~ 'airport',
                                          type == 'railway_station' ~ 'railway_station',
                                          type == 'bus_station' ~ 'bus_station',
                                          type == 'accommodation' ~ 'accommodation',
                                          .default = "default"))
  
}
