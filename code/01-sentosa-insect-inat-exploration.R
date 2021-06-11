# Explores the occurrence data of Insecta class in Singapore, downloaded from iNaturalist.
# Specifically looking at those within Sentosa.

source("start.R")

# ----------------------- Import Data and Maps ----------------------- 

# Read in occurrence data for Singapore
obs <- read_csv(paste0(wd$data, "observations-155147.csv"))
obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), crs = 4326)
print(paste("Number of observations in Singapore:", nrow(obs))) # 55988
View(obs)

# Filter to get occurrence data labelled Sentosa, based on column place_guess
obs$place_guess <- tolower(obs$place_guess)
sentosa <- obs %>% filter(grepl("sentosa", place_guess))
sentosa_sf <- st_as_sf(sentosa, coords = c("longitude", "latitude"), crs = 4326)
print(paste("Number of observations labelled with Sentosa:", nrow(sentosa))) # 157

# ----------------------- Points in Polygon Analysis ----------------------- 

# To check if these Sentosa-labelled points are actually within Sentosa, read in shapefiles of Singapore from DIVA-GIS
administrative <- st_read(paste0(wd$raw, "divagis_SGP_adm0.shp"), crs = 4326) 
inlandwater <- st_read(paste0(wd$raw, "divagis_SGP_water_areas_dcw.shx"), crs = 4326)
roads <- st_read(paste0(wd$raw, "divagis_SGP_roads.shx"), crs = 4326)

ggplot() +
  geom_sf(data = administrative) +
  geom_sf(data = inlandwater) +
  geom_sf(data = roads)

# Only administrative seems useful, so we use that as the base map for a plot of Sentosa's occurrence data
ggplot() +
  geom_sf(data = administrative) +
  geom_sf(data = sentosa_sf, 
          alpha = 0.7,
          show.legend = "point") +
  coord_sf(xlim = c(103.805, 103.87), 
           ylim = c(1.239, 1.26))

# Some points fall outside the boundary, but it's a static map, so we export the occurrence data to a shapefile of points viewable in QGIS
simple_sentosa_sf <- sentosa_sf %>% dplyr::select(id, place_guess, scientific_name, taxon_id)
st_write(simple_sentosa_sf, paste0(wd$proc, "simple_sentosa_obs.shp"))

# We also view the administrative vectors in QGIS, and split them into various islands, then manually label them with their names
singapore_map <- readOGR(paste0(wd$raw, "divagis_SGP_adm0-refactored.shp"))
sentosa_map <- singapore_map[singapore_map$location == "Sentosa", ]
sentosa_map_sf <- st_as_sf(sentosa_map, crs = 4326)
sentosa_obs_sp <- as(sentosa_sf, "Spatial")
proj4string(sentosa_obs_sp) <- proj4string(sentosa_map)

# Keep only the Sentosa-labelled points that fall within the Sentosa geometries 
sentosa_intersection <- over(sentosa_obs_sp, sentosa_map) # points first, then geometry; NA mean point falls outside the boundary
clean_sentosa_sf <- cbind(sentosa_sf, sentosa_intersection)
clean_sentosa_sf <- clean_sentosa_sf[!is.na(clean_sentosa_sf$location), ]
nrow(clean_sentosa_sf) # 154

# But are there points not labelled with Sentosa that fall within the Sentosa geometries?
non_sentosa <- obs %>% filter(!grepl("sentosa", place_guess))
non_sentosa_sf <- st_as_sf(non_sentosa, coords = c("longitude", "latitude"), crs = 4326)
non_sentosa_sp <- as(non_sentosa_sf, "Spatial")
proj4string(non_sentosa_sp) <- proj4string(sentosa_map)
non_sentosa_intersection <- over(non_sentosa_sp, sentosa_map) # Yes, there are

# Keep only the non-Sentosa-labelled points that fall within the Sentosa geometries
clean_non_sentosa_sf <- cbind(non_sentosa_sf, non_sentosa_intersection)
clean_non_sentosa_sf <- clean_non_sentosa_sf[!is.na(clean_non_sentosa_sf$location), ]
nrow(clean_non_sentosa_sf) # 431

# So place_guess is unreliable; go by points-in-polygon method only for all observations
obs_sp <- as(obs_sf, "Spatial")
proj4string(obs_sp) <- proj4string(sentosa_map)
sentosa_obs <- over(obs_sp, sentosa_map)
clean_sentosa_obs_sf <- cbind(obs_sf, sentosa_obs)
clean_sentosa_obs_sf <- clean_sentosa_obs_sf[!is.na(clean_sentosa_obs_sf$location), ]
print(paste("Number of observations actually in Sentosa:", nrow(clean_sentosa_obs_sf))) # 585

ggplot() +
  geom_sf(data = sentosa_map_sf) +
  geom_sf(data = clean_sentosa_obs_sf, 
          alpha = 0.7) +
  coord_sf(xlim = c(103.805, 103.847), ylim = c(1.235, 1.26))

# ----------------------- Try Other Singapore Maps ----------------------- 

# DIVA-GIS's Singapore geoms may be outdated, e.g. see Changi, try the raster files from Gaw, Yee and Richards

# Try raster library
data_tif <- raster::raster(paste0(wd$raw, "gawyeerichards_sgmap.tif"), RAT = TRUE)
data_rgb <- raster::brick(paste0(wd$raw, "gawyeerichards_sgmap.tif"))
data_dbf <- foreign::read.dbf(paste0(wd$raw, "gawyeerichards_sgmap.tif.vat.dbf") # read the dbf in as a data frame
                              
plot(data_tif)
raster::plotRGB(data_rgb)

# Try terra library instead
sg <- terra::rast(paste0(wd$raw, "gawyeerichards_sgmap.tif"))
terra::sources(sg)
terra::hasValues(sg)
plot(sg)

rm(clean_non_sentosa_sf, clean_sentosa_sf, clean_sentosa_obs_sf, non_sentosa_intersection, non_sentosa_sf, non_sentosa_sp, sentosa_intersection)