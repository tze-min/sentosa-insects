## Don't run this, haha

library("readr")
library("dplyr")
library("tidyr")
library("rgdal")
library("sf")
library("ggplot2")

setwd("C:/Users/TzeMin/Documents/sentosa-insects")

######################### DATA CLEANING #########################

#### Read in data and do basic filtering - for Singapore ####
obs <- read_csv("data/observations-155147.csv")
obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), crs = 4326)
print(paste("Number of observations in Singapore:", nrow(obs)))

#### Read in data and do basic filtering -for points labelled Sentosa ####
obs$place_guess <- tolower(obs$place_guess)
sentosa <- obs %>% filter(grepl("sentosa", place_guess))
sentosa_sf <- st_as_sf(sentosa, coords = c("longitude", "latitude"), crs = 4326)
print(paste("Number of observations labelled with Sentosa:", nrow(sentosa)))

administrative <- st_read("data/SGP_adm0.shp", crs = 4326) ## Using shapefiles from DIVA-GIS (Note: Only administrative seems useful)
#inlandwater <- st_read("data/SGP_water_areas_dcw.shx", crs = 4326)
#roads <- st_read("data/SGP_roads.shx", crs = 4326)

ggplot() +
  geom_sf(data = administrative) +
  geom_sf(data = sentosa_sf, 
          alpha = 0.7,
          show.legend = "point") +
  coord_sf(xlim = c(103.805, 103.87), ylim = c(1.239, 1.26))

#st_write(simple_sentosa_sf, "data/simple_sentosa_obs.shp") # export to shapefile and view it in qgis

#### Bring in manually labelled Singapore geometries, use sp files ####
singapore_map <- readOGR("data/refactored_singapore_geometries.shp") # source is DIVA-GIS
#singapore_map <- readOGR("data/GawYeeRichards/figure1.tif.vat.dbf") # source is Gaw, Yee and Richards
sentosa_map <- singapore_map[singapore_map$location == "Sentosa", ]
sentosa_map_sf <- st_as_sf(sentosa_map, crs = 4326)
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

#### Using Singapore map files from Gaw, Yee and Richards ####
library(raster)
options(stringsAsFactors = FALSE)
test <- raster(nrows = 10, ncols = 10)
test[] <- sample(1:10, 100, replace = TRUE)
test_rat <- data.frame('ID' = seq.int(10),
                       'CAT' = c('cat', 'dog', 'banana', 'ship', 'egg',
                                 'tree', 'beer', 'shoe', 'light', 'pen'))
levels(test) <- test_rat
writeRaster(test, 'data/test_rat.tif', datatype = 'INT2S', overwrite = TRUE)
tested <- raster('data/test_rat.tif', RAT = TRUE)
rasterVis::levelplot(tested)

# read the tif in with RAT = TRUE. Should get an ID column at least in the resulting object.
data_tif <- raster('data/GawYeeRichards/figure1.tif', RAT = TRUE)
data_rgb <- brick('data/GawYeeRichards/figure1.tif')

plot(data_tif)
plotRGB(data_rgb)
                   
# read the dbf in as a data frame
data_dbf <- foreign::read.dbf('data/GawYeeRichards/figure1.tif.vat.dbf')

# using terra library instead of raster
library(terra)
sg <- rast("data/GawYeeRichards/figure1.tif")
sources(sg)
hasValues(sg)
plot(sg)

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

genus_count <-
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

######################### Adding Images to Chart ###########################
library(ggtext)
library(dplyr)
library(ggplot2)

set.seed(123)

data <- expand.grid(
  cat = LETTERS[1:5],
  cond= c(FALSE, TRUE),
  phase = c("Interphase", "Prophase", "Metaphase", "Anaphase", "Telophase")
) %>%
  mutate(
    value = floor(rnorm(n())*100),
    value = ifelse(value < 0, 0, value)
  )

# images from: http://www.microbehunter.com/mitosis-stages-of-the-lily/

labels <- c(
  Interphase = "<img src='images/interphase.jpg' width='60' /><br>Interphase",
  Prophase = "<img src='images/prophase.jpg' width='60' /><br>Prophase",
  Metaphase = "<img src='images/metaphase.jpg' width='60' /><br>Metaphase",
  Anaphase = "<img src='images/anaphase.jpg' width='60' /><br>Anaphase",
  Telophase = "<img src='images/telophase.jpg' width='60' /><br>Telophase"
)

ggplot(data, aes(phase, value, fill = cat)) +
  geom_col(position = "dodge") +
  scale_x_discrete(name = NULL, labels = labels) +
  theme(axis.text.x = element_markdown(lineheight = 1.2))

#### Another method
my_axis = function(img) {
  structure(
    list(img=img),
    class = c("element_custom","element_blank", "element") # inheritance test workaround
  )
}

element_grob.element_custom <- function(element, x,...)  {
  stopifnot(length(x) == length(element$img))
  tag <- names(element$img)
  
  # add vertical padding to leave space
  g1 <- textGrob(paste0(tag, "\n\n\n\n\n"), x=x, vjust=0.6,rot = 45)
  g2 <- mapply(rasterGrob, x=x, image=element$img[tag], 
               MoreArgs=list(vjust=0.6, interpolate=FALSE,
                             height=unit(3,"lines")),
               SIMPLIFY=FALSE)
  gTree(children=do.call(gList, c(g2, list(g1))), cl="custom_axis")
}

imgs <- lapply(paste0(df.plot$description, '.png'), png::readPNG); 
g <- lapply(imgs, grid::rasterGrob);

gg <- gg + theme(axis.text.x  = my_axis(pics),
                 axis.text.y  = element_text(size=14),
                 axis.title.x = element_blank())

######################### Incorporating Taxonomic Data ###########################
library("taxize")
library("myTAI")

# help: https://cran.r-project.org/web/packages/myTAI/vignettes/Taxonomy.html

taxonomy(organism = "Elymnias hypermnestra", db = "ncbi", output = "classification")

outputlst <- apply(lst, 1, function(x) taxonomy( organism = x , db = "ncbi", output = "classification" ))

# Parse out the taxonomy levels that you require
taxdata = data.frame()

for(x in 1:length(outputlst)){
  tryCatch({
    phylum=filter(outputlst[[x]], rank =="phylum")$name
    class=filter(outputlst[[x]], rank =="class")$name
    order=filter(outputlst[[x]], rank =="order")$name
    family=filter(outputlst[[x]], rank =="family")$name
    genus=filter(outputlst[[x]], rank =="genus")$name
    species=filter(outputlst[[x]], rank =="species")$name
    
    row <- data.frame(cbind(phylum=phylum,class=class,order=order,family=family,genus=genus))
    taxdata <- bind_rows(taxdata, row)    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
write.csv(taxdata, file="taxdata-full.txt")

######################### TO DO #########################
# Compile family and order per species
# Use the Singapore map shapefile sent by Daniel
# Make chart of observations over the years