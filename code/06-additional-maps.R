#### 1. Download packages ####

# data management
library("readr")
library("dplyr")
library("tidyr")

# data viz
library("ggplot2")
library("ggtext")
library("ggmap")

# interactive map
library("htmltools")
library("leaflet")
library("htmlwidgets")

# rasters / vector management
library("rgdal")
library("sf")

wd <- list()
wd$root <- getwd()
wd$code <- file.path(getwd(), "code")
wd$raw <- file.path(getwd(), "data", "raw")
wd$proc <- file.path(getwd(), "data", "processed")
wd$img <- file.path(getwd(), "images")
wd$map <- file.path(getwd(), "maps")


#### 2. Configuration (Change your colors here) ####

### Set color scheme of maps

# For maps with only 1 dataset plotted
color_of_data <- "black"

# For the map with both datasets plotted
color_of_EC_data <- "navy"
color_of_CS_data <- "red"

### Read datasets

# Read Citizen Science dataset in csv format
obs_cs <- read_csv(file.path(wd$raw, "CSdata15.csv"))
obs_cs$Source <- "Citizen Science"

# Read Expert Collected dataset in csv format
obs_ec <- read_csv(file.path(wd$raw, "ECdata15.csv"))
obs_ec$Source <- "Expert Collected"

# Get map of Singapore and convert it from st to sf format (source: https://earthworks.stanford.edu/catalog/stanford-pg798kr1205)
singapore_map <- readOGR(paste0(wd$raw, "/", "SGP_adm0.shp"))
singapore_map_sf <- st_as_sf(singapore_map, crs = 4326)




#### 3. Map only Citizen Science data ####

# Fetch original dataset
obs <- st_as_sf(obs_cs, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
obs_sp <- as(obs, "Spatial")

# Clean data by removing data points in ocean
proj4string(obs_sp) <- proj4string(singapore_map)
singapore_obs <- over(obs_sp, singapore_map)
clean_singapore_obs_sf <- cbind(obs, singapore_obs)
obs <- clean_singapore_obs_sf[!is.na(clean_singapore_obs_sf$ISO), ]

# Create map
map <- leaflet(obs) %>% 
  setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
  addTiles(group = "Street") %>%
  addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
  addCircleMarkers(data = obs,
                   ~Longitude, ~Latitude,
                   radius = 1, stroke = TRUE, opacity = .7, fillOpacity = .7,
                   color = color_of_data)

count <- nrow(obs)

maptitle <- tagList(
  tags$h1(tags$b("Citizen Science Observations"), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")),
  tags$p(tags$b(paste0("(n = ", count, ")")), tags$style("p {display: inline; color: black; font-size: 14px; text-align: center;}")))

map <- map %>%
  addControl(maptitle, position = "bottomleft")

map

# Download map
saveWidget(map, file = file.path(wd$map, paste0("Citizen Science Data", ".html")))





#### 4. Map only Expert Collected data ####

obs <- st_as_sf(obs_ec, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
obs_sp <- as(obs, "Spatial")

# Clean data by removing data points in ocean
proj4string(obs_sp) <- proj4string(singapore_map)
singapore_obs <- over(obs_sp, singapore_map)
clean_singapore_obs_sf <- cbind(obs, singapore_obs)
obs <- clean_singapore_obs_sf[!is.na(clean_singapore_obs_sf$ISO), ]

# Create map
map <- leaflet(obs) %>% 
  setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
  addTiles(group = "Street") %>%
  addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
  addCircleMarkers(data = obs,
                   ~Longitude, ~Latitude,
                   radius = 1, stroke = TRUE, opacity = .7, fillOpacity = .7,
                   color = color_of_data)

count <- nrow(obs)

maptitle <- tagList(
  tags$h1(tags$b("Expert Collected Observations"), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")),
  tags$p(tags$b(paste0("(n = ", count, ")")), tags$style("p {display: inline; color: black; font-size: 14px; text-align: center;}")))

map <- map %>%
  addControl(maptitle, position = "bottomleft")

map

# Download map
saveWidget(map, file = file.path(wd$map, paste0("Expert Collected Data", ".html")))





#### 5. Map both Expert Collected and Citizen Science data ####

obs_all <- rbind(obs_ec, obs_cs)

obs <- st_as_sf(obs_all, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
obs_sp <- as(obs, "Spatial")

# Clean data by removing data points in ocean
proj4string(obs_sp) <- proj4string(singapore_map)
singapore_obs <- over(obs_sp, singapore_map)
clean_singapore_obs_sf <- cbind(obs, singapore_obs)
obs <- clean_singapore_obs_sf[!is.na(clean_singapore_obs_sf$ISO), ]

# Create map
pal <- colorFactor(
  palette = c(color_of_EC_data, color_of_CS_data),
  domain = obs$Source
)

map <- leaflet(obs) %>% 
  setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
  addTiles(group = "Street") %>%
  addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
  addCircleMarkers(data = obs,
                   ~Longitude, ~Latitude,
                   radius = 1, stroke = TRUE, opacity = .7, fillOpacity = .7,
                   color = ~pal(Source)) %>%
  addLegend("bottomright", pal = pal, values = ~Source,
            title = "Collection Source",
            opacity = 1
  )

count <- nrow(obs)

maptitle <- tagList(
  tags$h1(tags$b("Expert Collected and Citizen Science Observations"), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")),
  tags$p(tags$b(paste0("(n = ", count, ")")), tags$style("p {display: inline; color: black; font-size: 14px; text-align: center;}")))

map <- map %>%
  addControl(maptitle, position = "bottomleft")

map

# Download map
saveWidget(map, file = file.path(wd$map, paste0("All Data", ".html")))




#### 6. Map of Singapore ####

map <- leaflet() %>%
  setView(lng = 117.8198, lat = 5.3521, zoom = 5) %>%
  addTiles(group = "Street") %>%
  addPolygons(data = singapore_map, 
              stroke = FALSE, opacity = 1,
              color = "red",
              fillOpacity = 1,
              fillColor = "red"
  )

maptitle <- tagList(
  tags$h1(tags$b("Location of Singapore in Southeast Asia"), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")))

map <- map %>%
  addControl(maptitle, position = "bottomleft")

map

# Download map
saveWidget(map, file = file.path(wd$map, paste0("Southeast Asia Map", ".html")))




#### 7. Get data points falling outside of Singapore ####
obs_all <- rbind(obs_ec, obs_cs)

obs <- st_as_sf(obs_all, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)
obs_sp <- as(obs, "Spatial")

proj4string(obs_sp) <- proj4string(singapore_map)
singapore_obs <- over(obs_sp, singapore_map)
clean_singapore_obs_sf <- cbind(obs, singapore_obs)

obs_outside_singapore <- clean_singapore_obs_sf[is.na(clean_singapore_obs_sf$ISO), ]
write.csv(obs_outside_singapore, "Observations outside Singapore boundaries.csv")