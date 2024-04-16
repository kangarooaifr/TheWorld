Documentation
================

## Presentation

\[…\]

## Locations

#### Connectors

- r\$filter_country_choices: vector, country names from the (unfiltered)
  location items
- r\$airports: data.frame, data about airports
- r\$visited_countries: reactive, vector containing the list of visited
  countries (been.there == TRUE)

#### Event Observers

- r\$map_click: event displays a popup on the map
- r\$filter_country: event applies filter on the selected locations

#### Other dependencies

- r\$proxymap: to update the map with markers
- r\$map_click: used to add new location

## Countries

#### Connectors

- r\$countries_iso: data.frame, ISO data about countries

#### Event Observers

- r\$visited_countries: event updates country boundaries
- r\$filter_country: event applies filter on the selected countries

#### Other dependencies

- r\$proxymap: used to display boundaries (addPolygons) on the map

## Routes

#### Connectors

- 

#### Event Observers

- 

#### Other dependencies

- r\$airports: used to set origin / destination of flight routes
- r\$proxymap: used to display routes (addPolylines) on the map

## Tracks

#### Connectors

- 

#### Event Observers

- 

#### Other dependencies

- r\$proxymap: used to display tracks (addPolylines) on the map
