
### 04 Sentosa Insects - iNaturalist Data Visualisation with Leaflet

# This script visualises the data cleaned from 02 and combined with data from 03.
# It focuses on experimenting with the package "leaflet", which can create maps as HTML files,
# with limited interactivity.

# After "Initialisation", which initialises the occurrence data (the code is based on 02 and 03 scripts) 
# the remaining sections each creates a type of map, as per the section header title. The scripts written
# here eventually contributed to the development of the Occurrence Mapper web app.

source("start.R")

# ----------------------- Initialisation -----------------------

initialise_script04 <- function() {
  
  # Imports
  obs <- read_csv(paste0(wd$proc, "clean-sentosa-observations-155147.csv"))
  obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)
  
  singapore_map_sp <- readOGR(paste0(wd$raw, "divagis_SGP_adm0-refactored.shp"))
  sentosa_map_sp <- singapore_map_sp[singapore_map_sp$location == "Sentosa", ]
  sentosa_map_sf <- st_as_sf(sentosa_map_sp, crs = 4326)
  
  # Get scientific name and taxo data and merge to produce more complete dataset
  clean <- 
    obs_sf %>% 
    separate(scientific_name, c("genus", "species", "subspecies"), remove = FALSE) %>%
    unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE) 
  
  clean$latitude <- as.numeric(clean$latitude)
  clean$longitude <- as.numeric(clean$longitude)
  
  taxo_data <- read_csv(paste0(wd$proc, "insects-taxdata-full.csv"))
  full <- merge(clean, taxo_data, by.x = "scientific_name_simple", by.y = "species", all.x = TRUE)
  return(full)
}

write.csv(initialise_script04(), "clean-sentosa-observations-155147_formatted.csv", row.names = FALSE)

# ----------------------- Occurrences by Order: Trial -----------------------

# Maps I want
# - one map for all occurrences by order
# - one map per order showing the species name - can automate
# - one map for the distribution of top 5 species together

library("leaflet")
library("htmltools")
library("htmlwidgets")

full <- initialise_script04() %>% 
  mutate(popup_text = paste("<i>", scientific_name_simple, "</i><br/>",
                            observed_on, "<br/>",
                            "by", user_login))

dfs_byorder <- full %>% split(list(.$order))
dfs_byorder <- dfs_byorder[order(sapply(dfs_byorder, nrow), decreasing = TRUE)]

basemap <- leaflet(full) %>%
  setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>%
  addTiles(group = "Street") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addLayersControl(
    baseGroups = c("Light", "Dark", "Street", "Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

num_of_orders <- length(dfs_byorder)
names_of_dfs <- names(dfs_byorder)
colors_per_df <- RColorBrewer::brewer.pal(num_of_orders, "Set1") # use a qualitative color palette
#colors_per_df <- palette(rainbow(num_of_orders))

ordermap <- basemap

for (i in seq(1:num_of_orders)) {
  df <- dfs_byorder[i][[1]]
  ordermap <- ordermap %>% 
    addCircleMarkers(data = df,
                     ~longitude, ~latitude,
                     label = lapply(df$popup_text, htmltools::HTML),
                     radius = 1.2, stroke = TRUE, fillOpacity = 1,
                     color = colors_per_df[i],
                     group = names_of_dfs[i])
}

ordermap <- ordermap %>%
  addLayersControl(
    baseGroups = c("Light", "Dark", "Street", "Satellite"),
    overlayGroups = names_of_dfs,
    options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend("bottomright", 
            colors = colors_per_df,
            labels = names_of_dfs)

ordermap

# ----------------------- Occurrences by Order: Dark Theme -----------------------

library("leaflet")
library("htmltools")
library("htmlwidgets")

full <- initialise_script04() %>% 
  mutate(popup_text = paste("<i>", scientific_name_simple, "</i><br/>",
                            observed_on, "<br/>",
                            "by", user_login))

dfs_byorder <- full %>% split(list(.$order))
dfs_byorder <- dfs_byorder[order(sapply(dfs_byorder, nrow), decreasing = TRUE)]

makemap_order <- function(insectdata, radio = FALSE) {
  
  basemap <- leaflet(insectdata) %>%
    setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark")
  
  num_of_orders <- length(dfs_byorder)
  names_of_dfs <- names(dfs_byorder)
  ordermap <- basemap
  
  if (radio == TRUE) {
    
    for (i in seq(1:num_of_orders)) {
      df <- dfs_byorder[i][[1]]
      ordermap <- ordermap %>% 
        addCircleMarkers(data = df,
                         ~longitude, ~latitude,
                         label = lapply(df$popup_text, htmltools::HTML),
                         radius = 1.2, stroke = TRUE, fillOpacity = 1,
                         color = "red",
                         group = names_of_dfs[i])
    }
    ordermap <- ordermap %>%
      addLayersControl(
        baseGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE))
    
  } else {
    
    colors_per_df <- RColorBrewer::brewer.pal(num_of_orders, "Set1") # use a qualitative color palette
    
    for (i in seq(1:num_of_orders)) {
      df <- dfs_byorder[i][[1]]
      ordermap <- ordermap %>% 
        addCircleMarkers(data = df,
                         ~longitude, ~latitude,
                         label = lapply(df$popup_text, htmltools::HTML),
                         radius = 1.2, stroke = TRUE, fillOpacity = 1,
                         color = colors_per_df[i],
                         group = names_of_dfs[i])
    }
    ordermap <- ordermap %>%
      addLayersControl(
        overlayGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("bottomright", 
                colors = colors_per_df,
                labels = names_of_dfs)
  }
  
  ordermap
}

# ----------------------- Occurrences by Order: Toggle Order Radio, Cluster and Basemap Theme at R Code -----------------------

full <- initialise_script04() %>% 
  mutate(popup_text = paste("<i>", scientific_name_simple, "</i><br/>",
                            observed_on, "<br/>",
                            "by", user_login))

dfs_byorder <- full %>% split(list(.$order))
dfs_byorder <- dfs_byorder[order(sapply(dfs_byorder, nrow), decreasing = TRUE)]

makemap_order <- function(data, order_radio = TRUE, cluster = FALSE, theme = "Satellite") {
  # available themes: Voyager, Dark, Light, Street, Satellite
  
  basemap <- leaflet(data) %>% 
    setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>%
    purrr::when(theme == "Voyager" ~ addProviderTiles(., providers$CartoDB.Voyager),
                theme == "Dark" ~ addProviderTiles(., providers$CartoDB.DarkMatter),
                theme == "Light" ~ addProviderTiles(., providers$CartoDB.Positron),
                theme == "Street" ~ addProviderTiles(., providers$OpenStreetMap),
                theme == "Satellite" ~ addProviderTiles(., providers$Esri.WorldImagery))

  num_of_orders <- length(dfs_byorder)
  names_of_dfs <- names(dfs_byorder)
  ordermap <- basemap
  
  if (order_radio == TRUE) {
    # all data points will be red
    
    if (cluster == TRUE) {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = "red",
                           group = names_of_dfs[i],
                           clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option
      }
      
    } else {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = "red",
                           group = names_of_dfs[i])
      }
    }
    
    ordermap <- ordermap %>%
      addLayersControl(
        baseGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE))
    
  } else {
    # order_radio == FALSE: all data points will have different colors across insect orders
    
    colors_per_df <- RColorBrewer::brewer.pal(num_of_orders, "Set1") # use a qualitative color palette
    
    if (cluster == TRUE) {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = colors_per_df[i],
                           group = names_of_dfs[i],
                           clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option)
      }
      
    } else {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = colors_per_df[i],
                           group = names_of_dfs[i])
      }
    }
    ordermap <- ordermap %>%
      addLayersControl(
        overlayGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("bottomright", 
                colors = colors_per_df,
                labels = names_of_dfs) %>%
      hideGroup(names_of_dfs[2:num_of_orders])
  }
  
  saveWidget(ordermap, file = file.path(wd$map, "orders_cluster_voyager.html"), title = "Records by Order")
}

library("htmltools")
makemap_order(data = full, order_radio = TRUE, cluster = TRUE, theme = "Voyager")


# ----------------------- Occurrences by Order: Same as above, except - Checkbox the Basemap Themes -----------------------

full <- initialise_script04() %>% 
  mutate(popup_text = paste("<i>", scientific_name_simple, "</i><br/>",
                            observed_on, "<br/>",
                            "by", user_login))

dfs_byorder <- full %>% split(list(.$order))
dfs_byorder <- dfs_byorder[order(sapply(dfs_byorder, nrow), decreasing = TRUE)]

makemap_order <- function(data, order_radio = TRUE, cluster = FALSE) {

  basemap <- leaflet(data) %>% 
    setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>%
    addTiles(group = "Street") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager")
  
  num_of_orders <- length(dfs_byorder)
  names_of_dfs <- names(dfs_byorder)
  ordermap <- basemap
  
  if (order_radio == TRUE) {
    # all data points will be red
    
    if (cluster == TRUE) {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = "red",
                           group = names_of_dfs[i],
                           clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option
      }
      
    } else {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = "red",
                           group = names_of_dfs[i])
      }
    }
    
    ordermap <- ordermap %>%
      addLayersControl(
        baseGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE))
    
  } else {
    # order_radio == FALSE: all data points will have different colors across insect orders
    
    colors_per_df <- RColorBrewer::brewer.pal(num_of_orders, "Set1") # use a qualitative color palette
    
    if (cluster == TRUE) {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = colors_per_df[i],
                           group = names_of_dfs[i],
                           clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option)
      }
      
    } else {
      # cluster == FALSE
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = colors_per_df[i],
                           group = names_of_dfs[i])
      }
    }
    ordermap <- ordermap %>%
      addLayersControl(
        baseGroups = c("Voyager", "Street", "Satellite", "Light", "Dark"),
        overlayGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("bottomright", 
                colors = colors_per_df,
                labels = names_of_dfs) %>%
      hideGroup(names_of_dfs[2:num_of_orders])
  }
  ordermap
}

makemap_order(data = full, order_radio = TRUE, cluster = FALSE)


# ----------------------- Map per Species -----------------------

full <- initialise_script04() %>% 
  mutate(popup_text = paste("<i>", scientific_name_simple, "</i><br/>",
                            observed_on, "<br/>", 
                            "by", user_login, "<br/>",
                            id))

makemap_species <- function(data, speciesname, cluster = FALSE) {
  # species must be only genus + species, no subspecies taken into account
  
  df <- data %>% filter(scientific_name_simple == speciesname)
  count <- nrow(df)

  maptitle <- tagList(
    tags$h1(tags$b(speciesname), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")),
    tags$p(tags$b(paste0("(", count, ")")), tags$style("p {display: inline; color: black; font-size: 14px; text-align: center;}")))

  map <- leaflet(df) %>% 
    setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>%
    addTiles(group = "Street") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    #addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    #addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager")
  
  if (cluster == TRUE) {
    map <- map %>%
      addCircleMarkers(data = df,
                       ~longitude, ~latitude,
                       label = lapply(df$popup_text, htmltools::HTML),
                       radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                       color = "red",
                       clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option

    } else {
      map <- map %>%
        addCircleMarkers(data = df,
                         ~longitude, ~latitude,
                         label = lapply(df$popup_text, htmltools::HTML),
                         radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                         color = "red")

    }

    m <- map %>%
       addControl(maptitle, position = "bottomleft") %>%
       addLayersControl(
         baseGroups = c("Voyager", "Street", "Satellite"),
         options = layersControlOptions(collapsed = FALSE))
    
    saveWidget(m, file = file.path(wd$map, paste0(speciesname, "_cluster.html")), title = paste(speciesname, "Records"))
}

library("htmlwidgets")

scinames <- c(unique(full$scientific_name_simple))

for (i in unique(full$scientific_name_simple)) {
  makemap_species(data = full, speciesname = i, cluster = TRUE)
}




for (i in unique(full$scientific_name_simple)) {
  makemap_species_nocluster(data = full, speciesname = i, cluster = FALSE)
}


makemap_species_nocluster <- function(data, speciesname, cluster = FALSE) {
  # species must be only genus + species, no subspecies taken into account
  
  df <- data %>% filter(scientific_name_simple == speciesname)
  count <- nrow(df)
  
  maptitle <- tagList(
    tags$h1(tags$b(speciesname), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")),
    tags$p(tags$b(paste0("(", count, ")")), tags$style("p {display: inline; color: black; font-size: 14px; text-align: center;}")))
  
  map <- leaflet(df) %>% 
    setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>%
    addTiles(group = "Street") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    #addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    #addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager")
  
  if (cluster == TRUE) {
    map <- map %>%
      addCircleMarkers(data = df,
                       ~longitude, ~latitude,
                       label = lapply(df$popup_text, htmltools::HTML),
                       radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                       color = "red",
                       clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option
    
  } else {
    map <- map %>%
      addCircleMarkers(data = df,
                       ~longitude, ~latitude,
                       label = lapply(df$popup_text, htmltools::HTML),
                       radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                       color = "red")
    
  }
  
  m <- map %>%
    addControl(maptitle, position = "bottomleft") %>%
    addLayersControl(
      baseGroups = c("Voyager", "Street", "Satellite"),
      options = layersControlOptions(collapsed = FALSE))
  
  saveWidget(m, file = file.path(wd$map, paste0(speciesname, ".html")), title = paste(speciesname, "Records"))
}
