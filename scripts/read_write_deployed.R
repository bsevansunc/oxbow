# This script provides the end points for CTTs API and the mechanism for 
# reading and writing to a csv file. It is NOT AS FAST as the PostgreSQL 
# database solution (thus we should go back to that). It's `okay` for now,
# but will get prohibitively slow as the data grow.

# MODIFIED: This version refers to deployed tags ONLY

library(httr)
library(tidyverse)

# starts ------------------------------------------------------------------

# The location on my computer where files are stored:

local_dir <- getwd()
"~/Documents/oxbow"

host <-
  'https://api.internetofwildlife.com/' 

my_token <- 
  "11a65e30181eec8d2ade2fd58b6a9fe6ad569d6ea2920b6d43c504df06c7669a"

project <-
  '/station/api/projects'

stations <-
  '/station/api/stations/'

files <- 
  '/station/api/file-list'

file_types <-
  c("data", "node-data", "gps")

# get project and station information -------------------------------------

# Project information:

projects <- 
  POST(host, 
       path = project, 
       body = list(token = my_token), 
       encode="json", 
       verbose()) %>% 
  content("parsed") %>% 
  .$projects %>% 
  pluck(1)

# Get station information:

my_stations <-
  POST(host, 
       path = stations, 
       body = 
         list(
           token = my_token,
           'project-id' = projects$id),
       encode="json", timeout(60)) %>% 
  content() %>% 
  .$stations %>% 
  pluck(1)

# define files that need to be retrieved ----------------------------------

# Get list of files online:

my_files <-
  POST(
    host,
    path = files,
    body = 
      list(token = my_token,
           "station-id" = my_stations$station$id,
           begin = lubridate::as_date(my_stations$`deploy-at`),
           'file-types' = file_types),
    encode = 'json') %>% 
  content() %>% 
  .$files

# Get file names and ids of online files:

files_available <-
  names(my_files) %>% 
  map_dfr(
    function(file_type) {
      my_files[[file_type]] %>% 
        map_dfr(
          ~ tibble(
            id = .$id,
            file_name = .$name
          ))
    }) 

# Get files currently stored locally:

files_present <-
  tibble(
    file_name = 
      list.files(
        local_dir, 
        recursive = TRUE) %>%
      str_remove('^.*/'))

# Get file ids that are online, but not stored locally:

files_to_download <-
  files_available %>% 
  anti_join(files_present,
            by = 'file_name')

# deployed tags -----------------------------------------------------------

# Read in tags deployed this year:

deployed_tags <-
  readxl::read_excel("deployed_tags.xlsx") %>% 

  # Subset to this year:
  
  filter(year(date) == 2023) %>% 
  
  # Subset to columns of interest:
  
  select(tag_id,
         date:time, 
         species, 
         nest_id, 
         recovered_on) %>% 
  
  # Make deployment datetime a single variable:
  
  unite("deployed_at",
        date:time, 
        sep = " ") %>% 
  mutate(
    tag_id,
    across(
      c(deployed_at, recovered_on),
      
      # Add seconds:
      
      ~ str_c(.x, ":00") %>% 
      
      # Convert deployed_at to datetime variable with Eastern Time tz:
      
        as_datetime(tz = "America/New_York") %>% 
        
        # Convert to UTC:
        
        force_tzs(tzones = "America/New_York", 
                  tzone_out = "UTC")),
    nest_id,
    .keep = "none")

# download files ----------------------------------------------------------

# This will download files not currently present in your local directory:

if(
  nrow(files_to_download) > 0) {
  map(
    1:nrow(files_to_download),
    function(i) {
      
      # Define the location of the subdirectory where the file will be stored:
      
      subdirectory <-
        files_to_download[i, ] %>% 
        pull(file_name) %>% 
        str_extract('gps|node-health|raw') %>% 
        str_replace('-', '_')
      
      # Read in the file:
      
      POST(
        host,
        path = "/station/api/download-file/",
        body = 
          list(token = my_token,
               "file-id" = files_to_download[i, ]$id),
        encode = 'json') %>% 
        content() %>% 
        
        # Write to local directory:
        
        write_csv(
          file.path("data",
                    subdirectory,
                    files_to_download[i, ]$file_name))
    })
}

# Get new detections and bind into a tibble -------------------------------

new_files_start <-
  
  # Get a vector of new file names:
  
  list.files(
    'data/raw',
    pattern = 
      str_c(files_to_download$file_name, 
            collapse = "|"),
    full.names = TRUE) %>% 
  
  # Read in each file and bind into a tibble:
  
  purrr::map_dfr(
    ~ read_csv(., col_types = 'Ticdci') %>% 
      
      # Subset to deployed tags:
      
      filter(TagId %in% deployed_tags$tag_id) %>% 
      
      # Remove records with no NodeId or TagRSSI values:
      
      drop_na(NodeId, TagRSSI) %>% 
      
      # Remove duplicates:
      
      distinct() %>% 
      
      # I subset to just validated records (is this what I think it is?):
      
      filter(Validated == 1) %>% 
      select(!Validated) %>% 
      
      # Get the maximum RSSI value for the tag id, node, and time:
      
      slice_max(
        order_by = TagRSSI,
        n = 1,
        by = c(TagId, Time, NodeId),
        with_ties = FALSE)) %>% 
  
  # I, of course, don't like the names:
  
  set_names(
    tolower(
      names(.))) %>% 
  rename(tag_id = tagid, 
         rssi = tagrssi,
         node_id = nodeid)

# subset to detections after deployment and before recovery ---------------

new_files <-
  deployed_tags %>% 
  pull(tag_id) %>% 
  map_dfr(
    ~ {
      
      # Get deployment info:
      
      deployment <-
        deployed_tags %>% 
        filter(tag_id == .x)
      
      new_files_start %>% 
        filter(tag_id == .x) %>% 
        filter(time > deployment$deployed_at) %>% 
        {if(!is.na(deployment$recovered_on)) {
          filter(.,
                 .$time < deployment$recovered_on)
        } else .}
    }) %>% 
  arrange(time, tag_id)

# combine old and new files -----------------------------------------------

# Read in old detections file:

read_csv("data/processed/deployed_detections_2023.csv") %>% 
  
  # Add new files:
  
  bind_rows(new_files) %>% 
  
  # Make sure there are no duplicates:
  
  distinct() %>% 
  
  # Write to file:
  
  write_csv("data/processed/deployed_detections_2023.csv")




