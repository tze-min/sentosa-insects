library("readr")
library("dplyr")
library("tidyr")
library("rgdal")
library("sf")
library("ggplot2")
library("purrr")

setwd("C:/Users/TzeMin/Documents/sentosa-insects")

#### Read in data ####
obs <- read_csv("data/clean-sentosa-observations-155147.csv")
obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)
singapore_map_sp <- readOGR("data/refactored_singapore_geometries.shp") # source is DIVA-GIS, refctored in QGIS
sentosa_map_sp <- singapore_map_sp[singapore_map_sp$location == "Sentosa", ]
sentosa_map_sf <- st_as_sf(sentosa_map_sp, crs = 4326)

#### Split into genus and species and subspecies if present ####
clean <- 
  obs_sf %>% 
  separate(scientific_name, c("genus", "species", "subspecies"), remove = FALSE) %>%
  unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE) 

#### How records in Sentosa have changed over the years ####
dates <- data.frame(table(clean$observed_on)) %>% rename("date" = "Var1", "freq" = "Freq")
ggplot(data = dates, aes(x = date, y = freq, group = 1)) + geom_line() + geom_point()
  
#### Top n Insect Species in Sentosa ####
species_count <- 
  clean$scientific_name_simple %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename("species" = ".", "obs" = "Freq")

topn_species <-
  species_count %>%
  top_n(5)

genus_count <-
  clean$genus %>%
  table() %>%
  sort(decreasing = TRUE) %>%
  as.data.frame() %>%
  rename("genus" = ".", "obs" = "Freq")

ggplot(data = topn_species, aes(x = obs, y = reorder(species, obs))) +
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Top 15 Insect Species in Sentosa",
       subtitle = "Using iNaturalist data and observations from 2005 onwards") +
  xlab("Occurrences") + ylab("Insect Species") +
  theme_minimal()

# Download images if the names don't exist
download_image <- function(species) {
  filename <- paste0(species, ".jpg")
  if (file.exists()) ################## Stopped here
}

y = "https://inaturalist-open-data.s3.amazonaws.com/photos/11218116/medium.jpeg?1508147718"
download.file(y, paste0("images/insects/Elymnias hypermnestra", '.jpg'), mode = "wb")

# Inserting images
labels <- c(
  Interphase = "<img src='images/insects/interphase.jpg' width='60' /><br>Interphase",
  Prophase = "<img src='images/insects/prophase.jpg' width='60' /><br>Prophase",
  Metaphase = "<img src='images/insects/metaphase.jpg' width='60' /><br>Metaphase",
  Anaphase = "<img src='images/insects/anaphase.jpg' width='60' /><br>Anaphase",
  Telophase = "<img src='images/insects/telophase.jpg' width='60' /><br>Telophase"
)

ggplot(data, aes(phase, value, fill = cat)) +
  geom_col(position = "dodge") +
  scale_x_discrete(name = NULL, labels = labels) +
  theme(axis.text.x = element_markdown(lineheight = 1.2))