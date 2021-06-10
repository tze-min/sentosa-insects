library("readr")
library("dplyr")
library("tidyr")
library("rgdal")
library("sf")
library("ggplot2")
library("purrr")
library("ggtext")

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
  top_n(15)

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

# Download images and their sources if the names don't exist
download_species_image <- function(species_name, obs_data = clean, image_sources = "images/insects/image-sources.txt") {
  filename <- paste0("images/insects/", species_name, ".jpg")
  sink(image_sources, append = TRUE)

  if (!file.exists(filename)) {
    obs_of_species <- obs_data %>% filter(scientific_name_simple == species_name)
    image <- obs_of_species$image_url[1] # use the image of the first obs
    cat("Image of", obs_of_species$scientific_name_simple[1], "taken by", obs_of_species$user_login[1], "at", obs_of_species$url[1], "\n")
    download.file(image, filename, mode = "wb")
  }
  sink()
}

desired_species <- topn_species$speciess
unique_species <- unique(clean$scientific_name_simple)
lapply(desired_species, download_species_image)

# Inserting images to barplot
n <- length(topn_species$species)
labels <- character(n)
for (i in 1:n) {
  species_name <- as.character(topn_species$species[i])
  labels[i] <- paste0("<img src='images/insects/", species_name, ".jpg' width='60' /><br>", species_name)
}
names(labels) <- as.character(topn_species$species)
  
ggplot(topn_species, aes(x = obs, y = reorder(species, obs))) +
  geom_bar(position = "dodge", stat = "identity") + 
  labs(title = "Top 15 Insect Species in Sentosa",
       subtitle = "Using iNaturalist data and observations from 2005 onwards") +
  xlab("Occurrences") + ylab("Insect Species") +
  scale_y_discrete(name = NULL, labels = labels) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_line(color = "gray"),
        axis.text.y = element_markdown(lineheight = 1.2)) 

