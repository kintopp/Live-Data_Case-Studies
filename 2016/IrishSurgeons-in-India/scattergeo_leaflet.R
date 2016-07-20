## =============================== License ========================================
## ================================================================================
## This work is distributed under the MIT license, included in the parent directory
## Copyright Owner: University of Oxford
## Date of Authorship: 2016
## Author: Martin John Hadley (orcid.org/0000-0002-3039-6849)
## Academic Contact: Felix Krawatzek
## Data Source: local file
## ================================================================================

library(leaflet)

## =========================== Leaflet Tests ====================================
## ==============================================================================

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R") %>%
  addCircleMarkers(lng = 174, lat = 30, radius = 10)
m

## =========================== Actual Data Viz ====================================
## ==============================================================================

m <- leaflet(place_of_birth) %>%
  addTiles() %>%
  setView(lat = 53.347778, lng = -6.259722, zoom = 6) %>%
  addCircleMarkers(
    lng = ~Lon,
    lat = ~Lat,
    radius = rescale(place_of_birth$latlong.location.tally, to = c(5,20)),
    popup = paste0("Location Name: ",as.character(place_of_birth$PoB..Town.or.Parish.),
                  "<br/>",
                  "Births at location: ",
                  as.character(place_of_birth$latlong.location.tally))
  )
m



