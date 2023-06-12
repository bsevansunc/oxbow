# setup -------------------------------------------------------------------

library(sf)
library(tmap)
library(tidyverse)

# !Important! Run read_write_deployed.R first!

# For t-mapping:

tmap_mode("view")

tmap_options(
  basemaps = 
    c(Imagery = "Esri.WorldImagery",
      Canvas = "Esri.WorldGrayCanvas",
      Map = "OpenStreetMap"))

# initial data processing -------------------------------------------------

# Nodes:

nodes <- 
  st_read("data/processed/nodes_sf.kml") %>% 
  select(node_id = Name)

# Deployment data:

deployed_tags <- 
  readxl::read_excel("deployed_tags.xlsx") %>% 
  filter(year(date) == 2023,
         is.na(recovered_on)) %>% 
  select(tag_id, nest_id, deploy_date = date)

# Read in all detections for this year:

detections <-
  read_csv("data/processed/deployed_detections_2023.csv") %>% 
  
  # Convert detections (which are in UTC) to Eastern Time:
  
  mutate(
    time = 
      force_tzs(time, 
                tzones = "UTC", 
                tzone_out = "America/New_York")) %>% 
  
  # Add nest_ids from deployment data:
  
  inner_join(deployed_tags, by = "tag_id") %>% 
  
  # Subset to detections 1 hour after a given tag was deployed:
  
  filter(time > {min(time) + 3600},
         .by = tag_id)

# Get dawn and dusk times for each day:

dawn_dusk <-
  detections %>% 
  distinct(
    date = as_date(time)) %>% 
  mutate(
    lat = 38.98,
    lon = -78.12) %>% 
  as.data.frame() %>% 
  suncalc::getSunlightTimes(
    data = .,
    keep = c("dawn", "dusk"),
    tz = "America/New_York") %>% 
  select(date, dawn:dusk)

# plot theme --------------------------------------------------------------

daily_update_theme <-
  function() {
    theme(
      panel.background = element_rect("white"),
      panel.grid.major = element_line("#dcdcdc"),
      panel.grid.minor = 
        element_line(
          "#dcdcdc",
          linetype = "dashed"),
      axis.line = 
        element_line("black", linewidth = 0.25),
      strip.background = element_rect(color = "black"))
  }


# when last observed ------------------------------------------------------

get_last_observation <-
  function(target_tag, time_window = "1 hours") {
      detections %>% 
      
      # Subset to the target tag:
      
      filter(tag_id == target_tag) %>% 
      
      # Grab the last day of observations:
      
      filter(
        date(time) == 
          max(
            date(time))) %>% 
      
      # Get last time interval:
      
      mutate(
        time_round = round_date(time, time_window)) %>% 
      
      # Grab the last hour of observations:
      
      filter(
        time_round == max(time_round))
  }


# map last observed -------------------------------------------------------

map_last_time <-
  function(target_tag, time_window = "1 hours") {
    
    last_detection_at_nodes <- 
      
      # Get last set of observations in a given time window:
      
      get_last_observation(target_tag, time_window) %>% 
      
      # Get the maximum rssi from each node:
      
      group_by(node_id) %>% 
      summarize(rssi = max(rssi)) %>% 
      
      # Arrange by rssi value:
      
      arrange(
        desc(rssi))
    
    # Plot the data:
    
    nodes %>% 
      inner_join(
        last_detection_at_nodes,
        by = "node_id") %>% 
      drop_na(rssi) %>% 
      tm_shape(
        name = "node_id",
        bbox = nodes) +
      tm_dots(
        size = 0.2,
        palette = "-YlOrRd",
        col = "rssi")
    }






