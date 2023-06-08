library(sf)
library(tidyverse)

# Get oxbow extent:

oxbow_extent <-
  st_read("data/processed/oxbow_extent.geojson")

# Get nodes file and gather the median coordinate values:

nodes <-
  
  # List names of node health files:
  
  list.files(
    "data/node_health",
    pattern = "2023-0[56]-",
    full.names = TRUE) %>% 
  
  # Read in each file and spit out as a data frame:
  
  map_dfr(
    ~ read_csv(.x) %>% 
      select(node_id = NodeId,
             lon = Longitude,
             lat = Latitude)) %>% 
  
  # Calculate median coordinates for each node:
  
  summarize(
    across(
      lon:lat,
      ~ median(.x, na.rm = TRUE)),
    .by = node_id)

# Convert to sf and write:

nodes %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326) %>% 
  
  # Subset to nodes that show up within the Oxbow extent:
  
  st_filter(oxbow_extent) %>% 
  
  # Add names for labels and pins:
  
  mutate(
    Name = node_id,
    Description = node_id) %>% 
  
  # Write to file as a kml:
  
  st_write(
    "data/processed/nodes_sf.kml")
