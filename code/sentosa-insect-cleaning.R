library("readr")
library("dplyr")
library("tidyr")
library("rgdal")
library("sf")
library("ggplot2")

setwd("C:/Users/TzeMin/Documents/sentosa-insects")

#### Read in observation data and do basic filtering ####
obs <- read_csv("data/observations-155147.csv")
obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)
print(paste("Number of observations in Singapore:", nrow(obs)))

#### Bring in manually labelled Singapore geometries, use sp files ####
singapore_map <- readOGR("data/refactored_singapore_geometries.shp") # source is DIVA-GIS, refctored in QGIS
sentosa_map <- singapore_map[singapore_map$location == "Sentosa", ]
sentosa_map_sf <- st_as_sf(sentosa_map, crs = 4326)

#### Identify valid observations in Sentosa by checking if their points fall within Sentosa's borders ####
obs_sp <- as(obs_sf, "Spatial")
proj4string(obs_sp) <- proj4string(sentosa_map)
sentosa_obs <- over(obs_sp, sentosa_map)
clean_sentosa_obs_sf <- cbind(obs_sf, sentosa_obs)
clean_sentosa_obs_sf <- clean_sentosa_obs_sf[!is.na(clean_sentosa_obs_sf$location), ]
print(paste("Number of observations in Sentosa:", nrow(clean_sentosa_obs_sf)))

ggplot() +
  geom_sf(data = sentosa_map_sf) +
  geom_sf(data = clean_sentosa_obs_sf, 
          alpha = 0.7) +
  coord_sf(xlim = c(103.805, 103.847), ylim = c(1.235, 1.26))

#### Miscellaneous cleaning ####
clean <- 
  clean_sentosa_obs_sf %>% 
  filter(observed_on > "2005-01-01") %>%
  filter(captive_cultivated == FALSE) %>%
  select(-c(sound_url, tag_list, geoprivacy, taxon_geoprivacy, place_town_name, iconic_taxon_name, location)) %>%
  st_drop_geometry() # turns sf object to data.frame object

write_csv(clean, "data/clean-sentosa-observations-155147.csv")