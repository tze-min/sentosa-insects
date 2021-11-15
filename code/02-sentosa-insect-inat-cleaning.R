
### 02 Sentosa Insects - iNaturalist Data Cleaning

# This script cleans occurrence data from iNaturalist for Sentosa's insects, based on what was explored in file 01.

source("start.R")

# Keep observations that fall within Sentosa's borders based on points-in-geometry method
obs <- read_csv(paste0(wd$raw, "observations-155147.csv"))
obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)

singapore_map <- readOGR(paste0(wd$raw, "divagis_SGP_adm0-refactored.shp"))
sentosa_map <- singapore_map[singapore_map$location == "Sentosa", ]
sentosa_map_sf <- st_as_sf(sentosa_map, crs = 4326)

obs_sp <- as(obs_sf, "Spatial")
proj4string(obs_sp) <- proj4string(sentosa_map)
sentosa_obs <- over(obs_sp, sentosa_map)
clean_sentosa_obs_sf <- cbind(obs_sf, sentosa_obs)
clean_sentosa_obs_sf <- clean_sentosa_obs_sf[!is.na(clean_sentosa_obs_sf$location), ]

# Miscellaneous cleaning
clean <- 
  clean_sentosa_obs_sf %>% 
  filter(observed_on >= "2005-01-01") %>%
  filter(captive_cultivated == FALSE) %>%
  dplyr::select(-c(sound_url, tag_list, geoprivacy, taxon_geoprivacy, place_town_name, iconic_taxon_name, location)) %>%
  st_drop_geometry() # turns sf object to data.frame object, i.e. no geometry column

write_csv(clean, paste0(raw$proc, "clean-sentosa-observations-155147.csv"))