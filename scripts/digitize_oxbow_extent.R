library(mapedit)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)

# Get tower to center map on farm:

tower <-
  c(-78.118, 38.978) %>% 
  st_point() %>% 
  st_sfc(crs = 4326)

# Generate map and digitize extent of the farm:

temp <-
  mapview::mapView(tower)@map %>% 
  leaflet.extras::addDrawToolbar(
    markerOptions = leaflet.extras::drawMarkerOptions(repeatMode = TRUE)) %>% 
  mapedit::editMap()

temp$all %>% 
  st_write("data/processed/oxbow_extent.geojson")
