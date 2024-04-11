
# -------------------------------------
# Module config
# -------------------------------------

# -- File names
file_list <- list("airports" = "airports.csv",
                  "flights" = "flights.csv")


# -- Airport file columns
colClasses_airports <- c(Name = "character",
                         City = "character",
                         Country = "character",
                         IATA = "character",
                         ICAO = "character",
                         Latitude = "numeric",
                         Longitude = "numeric",
                         Altitude = "numeric")


# -- Flight file columns
colClasses_flights <- c(id = "numeric",
                        from = "character",
                        to = "character",
                        date = "character",
                        airline = "character",
                        flight.number = "character",
                        departure.time = "character",
                        arrival.time = "character")
