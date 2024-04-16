Documentation
================

## Presentation

\[…\]

## Location

### Connectors

Location module exposes the following connectors into the global r
communication object:

- r\$filter_country_choices: vector, country names from the (unfiltered)
  location items
- r\$airports: data.frame, data about airports
- r\$visited_countries: reactive, vector containing the list of visited
  countries (been.there == TRUE)

### Event Observers

Location module has observers taking dependencies on the following
connectors:

- r\$map_click: event displays a popup on the map
- r\$filter_country: event applies filter on the selected locations

### Other dependencies

- r\$proxymap: to update the map with markers
- r\$map_click: used to add new location

## Countries

### Connectors

Countries module exposes the following connectors into the global r
communication object:

- r\$countries_iso: data.frame, ISO data about countries

### Event Observers

### Other dependencies
