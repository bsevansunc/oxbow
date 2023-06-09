# setup -------------------------------------------------------------------

source("scripts/daily_updates/daily_updates_source.R")

# daily records -----------------------------------------------------------

detections_by_day <-
  detections %>% 
  mutate(time = as_date(time)) %>% 
  filter(time != today()) %>% 
  group_by(
    time,
    nest_id,
    tag_id)

# Number of pings on a given day:

detections_by_day %>% 
  summarize(
    n_pings = n(),
    .groups = "drop") %>% 
  ggplot() +
  aes(x = time,
      y = n_pings,
      color = tag_id) +
  geom_point() +
  geom_line() +
  facet_wrap(~ nest_id, scales = "free") +
  daily_update_theme()

# Number of nodes pinged on a given day:

detections_by_day %>% 
  summarize(
    n_nodes = 
      length(
        unique(node_id)),
    .groups = "drop") %>% 
  ggplot() +
  aes(x = time,
      y = n_nodes,
      color = tag_id) +
  geom_point() +
  geom_line() +
  facet_wrap(~ nest_id, scales = "free") +
  daily_update_theme()

# Mean rssi, by node, on a given day:

detections_by_day %>% 
  summarize(
    sd_rssi = 
      mean(
        sd(rssi),
        na.rm = TRUE),
    .groups = "drop") %>% 
  ggplot() +
  aes(x = time,
      y = sd_rssi,
      color = tag_id) +
  geom_point() +
  geom_line() +
  facet_wrap(~ nest_id, scales = "free") +
  daily_update_theme()

# dig in deeper with hourly records of the last 2 days --------------------

detections_2_days <- 
  detections %>% 
  filter(time > today() - 2,
         time < today()) %>% 
  mutate(
    date = as_date(time),
    time = round_date(time, "1 hours")) %>% 
  
  # Subset to records between dawn and one hour before dusk:
  
  left_join(
    dawn_dusk,
    by = "date") %>% 
  select(-date) %>%
  filter(
    between(time, dawn, dusk - 3600)) %>% 
  
  # Group data for summaries:
  
  group_by(
    time,
    nest_id,
    tag_id)

# Pings per hour:

detections_2_days %>% 
  summarize(
    n_pings = n(),
    .groups = "drop") %>% 
  ggplot() +
  aes(x = time,
      y = n_pings,
      color = tag_id) +
  geom_point() +
  geom_line() +
  facet_wrap(~ nest_id, scales = "free") +
  daily_update_theme()

# Nodes pinged per hour:

detections_2_days %>% 
  summarize(
    n_nodes = 
      length(
        unique(node_id)),
    .groups = "drop") %>% 
  ggplot() +
  aes(x = time,
      y = n_nodes,
      color = tag_id) +
  geom_point() +
  geom_line() +
  facet_wrap(~ nest_id, scales = "free") +
  daily_update_theme()

# Mean rssi, by node, on a given day:

detections_2_days %>% 
  summarize(
    sd_rssi = 
      mean(
        sd(rssi),
        na.rm = TRUE),
    .groups = "drop") %>% 
  ggplot() +
  aes(x = time,
      y = sd_rssi,
      color = tag_id) +
  geom_point() +
  geom_line() +
  facet_wrap(~ nest_id, scales = "free") +
  daily_update_theme()

# last observed at ... ----------------------------------------------------

# Find a tag that may be stationary? Use the code below to select a tag and
# identify where the tag pinged in the last hour and the rssi values of those
# detections.

# See a vector of the deployed tags:

deployed_tags$tag_id %>% 
  sort()

# When was a given tag last observed?

get_last_observation("1E554B19", time_window = "1 hours")

# Where was a given tag in the last hour?

map_last_time("55526107", time_window = "6 hours")
