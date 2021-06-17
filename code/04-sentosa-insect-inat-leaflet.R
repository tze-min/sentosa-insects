# Visualises the data cleaned from 02 and combined with data from 03.
# Focuses on experimenting with the package "leaflet"

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

# ----------------------- Occurrences by Order -----------------------

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
    options = layersControlOptions(collapsed = FALSE),
    overlayGroups = names_of_dfs) %>%
  addLegend("bottomright", 
            colors = colors_per_df,
            labels = names_of_dfs)

ordermap