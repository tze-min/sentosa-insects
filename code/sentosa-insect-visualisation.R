library("readr")
library("dplyr")
library("tidyr")
library("rgdal")
library("sf")
library("ggplot2")
library("purrr")
library("ggtext")
library("myTAI")

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
  top_n(10)

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

# Download images and their sources if they don't exist in our image directory
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
  labels[i] <- paste0(species_name, " <img src='images/insects/", species_name, ".jpg' width='60' />")
}
names(labels) <- as.character(topn_species$species)
  
ggplot(topn_species, aes(x = obs, y = reorder(species, obs))) +
  geom_bar(position = "dodge", stat = "identity", width = 0.6) + 
  labs(title = "Top 10 Insect Species in Sentosa",
       subtitle = "Using iNaturalist data and observations from 2005 onwards") +
  xlab("Occurrences") + ylab("Insect Species") +
  scale_y_discrete(name = NULL, labels = labels) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_line(color = "gray"),
        axis.text.y = element_markdown(lineheight = 1.2)) 

#### Retrieve Taxonomic Information ####
#taxize::use_entrez() # log in to NCBI account and create ENTREZ API Key
#usethis::edit_r_environ() # edit Renviron to add in ENTREZ_KEY if needed

taxdata_lst <- lapply(as.character(species_count$species), function(x) taxonomy(organism = x , db = "ncbi", output = "classification"))
taxdata = data.frame()

for (x in 1:length(taxdata_lst)) {
  tryCatch({
    order <- filter(taxdata_lst[[x]], rank == "order")$name
    family <- filter(taxdata_lst[[x]], rank == "family")$name
    species <- filter(taxdata_lst[[x]], rank == "species")$name
    row <- data.frame(cbind(order = order, family = family, species = species))
    taxdata <- bind_rows(taxdata, row)
  }, error = function(e) {cat("ERROR :", conditionMessage(e), "\n")})
}

setdiff(species_count$species, taxdata$species) # we notice there are some extra rows returned by NCBI?
final_taxdata <- merge(taxdata, species_count, by = "species")
#write_csv(select(final_taxdata, c(-obs)), "data/insects-taxdata-full.csv") # DON'T run this or you'll rewrite the manually added data
missing_species <- setdiff(species_count$species, final_taxdata$species) # add the missing data manually

#### Visualise Taxonomic Information ####
library("ggrepel")
taxo_data <- read_csv("data/insects-taxdata-full.csv")
taxo_breakdown <- taxo_data %>% group_by(order, family) %>% summarise(n = n()) %>% as.data.frame()

ggplot(taxo_breakdown, aes(x = reorder(order, -n), y = n, fill = family)) +
  geom_col(position = position_dodge2(width = .9), fill = "black") +
  labs(title = "Breakdown of Orders and Families of Insect Species in Sentosa",
       subtitle = "Using iNaturalist data and observations from 2005 onwards") +
  xlab("Order") + ylab("Occurrences") +
  geom_text(aes(label = family),
            position = position_dodge2(width = .9),
            vjust = 0.25, hjust = -0.2,
            angle = 90) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major.y = element_line(color = "gray")) +
  ylim(0, 32) 

#### Heatmaps? Maps? Just bad vectors??? ####
#remote::install_github("shaunkhoo/ltaer")
library("ltaer") # https://shaunkhoo.github.io/ltaer/articles/ltaerviz.html
library("ggmap")

clean <- clean %>% rename("Longitude" = "longitude", "Latitude" = "latitude") # naming required by plotSGMap package
full <- merge(clean, taxo_data, by.x = "scientific_name_simple", by.y = "species", all.x = TRUE)

lepidoptera <- full %>% filter(order == "Lepidoptera")
exploreSGMap(lepidoptera, colour = "red", size = 1.8, alpha = 0.5, popup = "scientific_name_simple") # note: sg_map: called from Google Maps API on 12 Dec 2018 7.30am

odonata <- full %>% filter(order == "Odonata")
exploreSGMap(odonata, colour = "blue", size = 1.8, alpha = 0.5, popup = "scientific_name_simple") # note: sg_map: called from Google Maps API on 12 Dec 2018 7.30am

# The below uses sg_map, but the zoom is at 1, can't change unless you use Google Maps API and get another instance of the map
ggmap(sg_map, darken = c("0.7")) +
  geom_point(data = obs, aes(x = Longitude, y = Latitude, color = order), size = 1.8, alpha = 0.7) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, -1, -1, 'cm'),
        legend.position = c(0.9, 0.25),
        legend.title = element_text(colour = 'white', size = 10),
        legend.text = element_text(colour = 'white', size = 7),
        legend.background = element_rect(fill = 'black', size = 0))

# Consider using your own google api key?
ggmap::register_google(key = "")
map <- get_map("singapore", maptype = "roadmap", zoom = 11, source = "google", color = "bw")