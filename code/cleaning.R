library("readr")
library("dplyr")
library("tidyr")
library("rgdal")
library("sf")
library("rnaturalearth")
library("ggplot2")

setwd("C:/Users/TzeMin/Documents/sentosa-insects")

######################### DATA CLEANING #########################

#### Read in data and do basic filtering - for Singapore ####
obs <- read_csv("data/observations-155147.csv")
obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), crs = 4326)
print(paste("Number of observations in Singapore:", nrow(obs)))
#obs$scientific_name %>% table() %>% sort(decreasing = T)

#### Read in data and do basic filtering -for points labelled Sentosa ####
obs$place_guess <- tolower(obs$place_guess)
sentosa <- obs %>% filter(grepl("sentosa", place_guess))
sentosa_sf <- st_as_sf(sentosa, coords = c("longitude", "latitude"), crs = 4326)
print(paste("Number of observations labelled with Sentosa:", nrow(sentosa)))
#sentosa$scientific_name %>% table() %>% sort(decreasing = T)

#### Check if the points lie within Singapore and Sentosa ####
#coast_sf <- ne_coastline(scale = "medium", returnclass = "sf") ## Using rnaturalearth - doesn't have Sentosa's polygons
#countries_sf <- ne_countries(scale = "medium", returnclass = "sf")

administrative <- st_read("data/SGP_adm0.shp", crs = 4326) ## Using shapefiles from DIVA-GIS (Note: Only administrative seems useful)
#inlandwater <- st_read("data/SGP_water_areas_dcw.shx", crs = 4326)
#roads <- st_read("data/SGP_roads.shx", crs = 4326)

ggplot() +
  geom_sf(data = administrative) +
  geom_sf(data = sentosa_sf, 
          alpha = 0.7,
          show.legend = "point") +
  coord_sf(xlim = c(103.805, 103.87), ylim = c(1.239, 1.26))

#obs_in_sentosa <- st_contains(sentosa_sf, administrative) # this doesn't seem to work
#obs_in_singapore <- st_contains(obs_sf, administrative)
st_write(simple_sentosa_sf, "data/simple_sentosa_obs.shp") # so we export to shapefile and view it in qgis

#### Bring in manually labelled Singapore geometries, use sp files ####
singapore_map <- readOGR("data/refactored_singapore_geometries.shp")
sentosa_map <- singapore_map[singapore_map$location == "Sentosa", ]
sentosa_obs_sp <- as(sentosa_sf, "Spatial")
proj4string(sentosa_obs_sp) <- proj4string(sentosa_map)
sentosa_intersection <- over(sentosa_obs_sp, sentosa_map) # points first, then geometry

#### Keep only the Sentosa-labelled points that fall within the Sentosa geometries ####
clean_sentosa_sf <- cbind(sentosa_sf, sentosa_intersection)
clean_sentosa_sf <- clean_sentosa_sf[!is.na(clean_sentosa_sf$location), ]
nrow(clean_sentosa_sf)

#### Are there points not labelled with Sentosa that fall within the Sentosa geometries? ####
non_sentosa <- obs %>% filter(!grepl("sentosa", place_guess))
non_sentosa_sf <- st_as_sf(non_sentosa, coords = c("longitude", "latitude"), crs = 4326)
non_sentosa_sp <- as(non_sentosa_sf, "Spatial")
proj4string(non_sentosa_sp) <- proj4string(sentosa_map)
non_sentosa_intersection <- over(non_sentosa_sp, sentosa_map) # yes there are

#### Keep only the non-Sentosa-labelled points that fall within the Sentosa geometries ####
clean_non_sentosa_sf <- cbind(non_sentosa_sf, non_sentosa_intersection)
clean_non_sentosa_sf <- clean_non_sentosa_sf[!is.na(clean_non_sentosa_sf$location), ]
nrow(clean_non_sentosa_sf)

#### So the place_guess is unreliable. Go by point-geometry analysis only for all obs ####
obs_sp <- as(obs_sf, "Spatial")
proj4string(obs_sp) <- proj4string(sentosa_map)
sentosa_obs <- over(obs_sp, sentosa_map)
clean_sentosa_obs_sf <- cbind(obs_sf, sentosa_obs)
clean_sentosa_obs_sf <- clean_sentosa_obs_sf[!is.na(clean_sentosa_obs_sf$location), ]
print(paste("Number of observations actually in Sentosa:", nrow(clean_sentosa_obs_sf)))

ggplot() +
  geom_sf(data = sentosa_map_sf) +
  geom_sf(data = clean_sentosa_obs_sf, 
          alpha = 0.7) +
  coord_sf(xlim = c(103.805, 103.847), ylim = c(1.235, 1.26))

#### Now do the same for Singapore (not sure if accurate, because SG map geoms may be outdated) ####
# leave for later, visualise the sentosa species first

#### Miscellaneous cleaning ####
clean <- 
  clean_sentosa_obs_sf %>% 
  filter(observed_on > "2005-01-01") %>%
  filter(captive_cultivated == FALSE) %>%
  select(-c(sound_url, tag_list, geoprivacy, taxon_geoprivacy, place_town_name, iconic_taxon_name, location))

#### Split into genus and species and subspecies if present ####
clean <- 
  clean %>% 
  separate(scientific_name, c("genus", "species", "subspecies"), remove = FALSE) %>%
  unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE)

ggplot() +
  geom_sf(data = sentosa_map_sf) +
  geom_sf(data = clean, 
          alpha = 0.7) +
  coord_sf(xlim = c(103.805, 103.847), ylim = c(1.235, 1.26))

rm(clean_non_sentosa_sf, clean_sentosa_sf, clean_sentosa_obs_sf, non_sentosa_, non_sentosa_intersection, non_sentosa_sf, non_sentosa_sp, sentosa_intersection)

######################### VISUALISATION #########################

View(clean)

species_count <- 
  clean$scientific_name_simple %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename("species" = ".", "obs" = "Freq")

topspecies <-
  species_count %>%
  top_n(15)

genus_count <- # not sure if helpful
  clean$genus %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename("genus" = ".", "obs" = "Freq")

ggplot(data = topspecies, aes(x = obs, y = reorder(species, obs))) +
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Top 15 Insect Species in Sentosa",
       subtitle = "Using iNaturalist data and observations from 2005 onwards") +
  xlab("Occurrences") + ylab("Insect Species") +
  theme_minimal()

