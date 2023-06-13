
# setup -------------------------------------------------------------------

source("scripts/daily_updates/daily_updates_source.R")

# Deployed tags by species:

deployed_spp <- 
  readxl::read_excel("deployed_tags.xlsx") %>% 
  filter(year(date) == 2023,
         is.na(recovered_on)) %>% 
  select(tag_id, species, nest_id)

# Detections list:

species <-
  c("AMKE", "BOBO", "EAME")

detections_spp <-
  map(
    species,
    ~ detections %>% 
      semi_join(
        deployed_spp %>% 
          filter(species == .x),
        by = "tag_id")) %>% 
  set_names(species)

# yesterday table ---------------------------------------------------------

# Summary of yesterday's observations across species, nests, and tags:

yesterday_tables <-
  map(
    species,
    ~ detections_spp %>% 
      pluck(.x) %>% 
      filter(as_date(time) == today() - 1) %>% 
      group_by(tag_id, nest_id) %>% 
      summarize(
        n_pings = n(), 
        n_nodes = length(unique(node_id)), 
        closest_node = first(node_id[rssi == max(rssi)]), 
        closest_node_rssi = max(rssi), 
        .groups = "drop") %>% 
      full_join(
        deployed_spp %>% 
          filter(species == .x)) %>% 
      mutate(
        across(
          n_pings:n_nodes,
          ~ replace_na(.x, 0)),
        closest_node = replace_na(closest_node, "NA")) %>% 
      select(!species) %>% 
      DT::datatable()) %>% 
  set_names(species)

# functions for plotting --------------------------------------------------

# Number of detections:

plot_pings <-
  function(data) {
    data %>% 
      summarize(
        n_pings = n(),
        .groups = "drop") %>% 
      ggplot() +
      aes(x = time,
          y = n_pings,
          color = tag_id) +
      geom_point() +
      geom_line() +
      facet_wrap(
        ~ nest_id, 
        scales = "free",
        ncol = 1) +
      daily_update_theme()
  }

# Number of nodes:

plot_nodes <-
  function(data) {
    data %>%
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
      facet_wrap(
        ~ nest_id, 
        scales = "free",
        ncol = 1) +
      daily_update_theme()
  }

# Standard deviation:

plot_sd <-
  function(data) {
    data %>% 
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
      facet_wrap(
        ~ nest_id, 
        scales = "free",
        ncol = 1) +
      daily_update_theme()
  }

# daily detections --------------------------------------------------------

# Data prep for daily detections:

daily_detections <-
  map(
    detections_spp,
    ~ .x %>% 
      mutate(time = as_date(time)) %>% 
      filter(time != today()) %>% 
      group_by(
        time,
        nest_id,
        tag_id))

# Plot number of detections:

daily_pings <-
  map(
    daily_detections,
    ~ plot_pings(.x))

# Plot number of nodes detected:

daily_nodes <-
  map(
    daily_detections,
    ~ plot_nodes(.x))

# Plot standard deviation of daily rssi values:

daily_sd <-
  map(
    daily_detections,
    ~ plot_sd(.x))

# the last 2 days (hourly records) ----------------------------------------

# Data prep for hourly detections:

hourly_detections <-
  map(
    detections_spp,
    ~ .x %>% 
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
        tag_id))

# Plot number of detections:

hourly_pings <-
  map(
    hourly_detections,
    ~ plot_pings(.x))

# Plot number of nodes detected:

hourly_nodes <-
  map(
    hourly_detections,
    ~ plot_nodes(.x))

# Plot standard deviation of daily rssi values:

hourly_sd <-
  map(
    hourly_detections,
    ~ plot_sd(.x))


  
  


