
mygeocode <- function(addresses)
{
  # addresses: vector with all addresses as character
  
  nominatim_osm <- function(address = NULL)
  {
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  
  # apply helper function
  tableau <- t(sapply(addresses, nominatim_osm))
  
  # colnames when some result is found
  if(dim(tableau)[2] > 1)
    colnames(tableau) <- c("lng","lat")
  else
    tableau <- NULL
  
  # return
  tableau
  
}