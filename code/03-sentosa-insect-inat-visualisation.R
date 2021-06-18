# Visualises the data cleaned from 02.
# - Top N occurring species
# - Auto-download image and sources
# - Extract taxonomic data from online databases
# - Visualise based on taxonomic info
# - Map with package "ltaer"

source("start.R")

# ----------------------- Import Data and Maps -----------------------

obs <- read_csv(paste0(wd$proc, "clean-sentosa-observations-155147.csv"))
obs_sf <- st_as_sf(obs, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)

singapore_map_sp <- readOGR(paste0(wd$raw, "divagis_SGP_adm0-refactored.shp"))
sentosa_map_sp <- singapore_map_sp[singapore_map_sp$location == "Sentosa", ]
sentosa_map_sf <- st_as_sf(sentosa_map_sp, crs = 4326)

# ----------------------- Examine Top N Occurring Species -----------------------

# Split into genus and subspecies if present
clean <- 
  obs_sf %>% 
  separate(scientific_name, c("genus", "species", "subspecies"), remove = FALSE) %>%
  unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE) 
  
clean$latitude <- as.numeric(clean$latitude)
clean$longitude <- as.numeric(clean$longitude)

# Top n insect species in Sentosa
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

# ----------------------- Automatically Download Insect Image and Source -----------------------

# Download insect image and its source from iNaturalist if the specified species doesn't exist in the image directory
download_species_image <- function(species_name, 
                                   obs_data = clean, 
                                   sources_outputfile = paste0(wd$img, "insects/image-sources.txt")) {
  
  filename <- paste0(wd$img, "insects/", species_name, ".jpg")
  sink(sources_outputfile, append = TRUE)
  
  if (!file.exists(filename)) {
    obs_of_species <- obs_data %>% filter(scientific_name_simple == species_name)
    
    image <- obs_of_species$image_url[1] # use the image of the first observation in the dataset
    sciname <- obs_of_species$scientific_name_simple[1]
    username <- obs_of_species$user_login[1]
    source_url <- obs_of_species$url[1]
    
    cat("Image of", sciname, "taken by", username, "at", source_url, "\n")
    download.file(image, filename, mode = "wb")
  }
  
  sink()
}

desired_species <- topn_species$species
unique_species <- unique(clean$scientific_name_simple)
lapply(desired_species, download_species_image)

# Insert images to barplot axes
n <- length(topn_species$species)
labels <- character(n)
for (i in 1:n) {
  species_name <- as.character(topn_species$species[i])
  labels[i] <- paste0("<img src='images/insects/", species_name, ".jpg' width='60'/><br><em>", species_name, "</em>")
}
names(labels) <- as.character(topn_species$species)

ggplot(topn_species, aes(x = reorder(species, obs), y = obs)) +
  geom_col(position = "dodge", width = 0.6) + 
  labs(title = "Top 10 Insect Species in Sentosa",
       subtitle = "Using iNaturalist data and observations from 2005 onwards") +
  ylab("Occurrences") + xlab("Insect Species") +
  scale_x_discrete(name = NULL, labels = labels) +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_line(color = "gray"),
        axis.text.x = element_markdown(lineheight = 1.2)) 

# ----------------------- Retrieve Taxonomic Information from NCBI -----------------------

#taxize::use_entrez() # log in to NCBI account and create ENTREZ API Key
#usethis::edit_r_environ() # edit Renviron to add in ENTREZ_KEY if needed

# Get taxonomy data for each species
taxdata_lst <- lapply(as.character(species_count$species), function(x) myTAI::taxonomy(organism = x,
                                                                                       db = "ncbi",
                                                                                       output = "classification"))
taxdata <- data.frame()

for (x in 1:length(taxdata_lst)) {
  tryCatch({
    order <- filter(taxdata_lst[[x]], rank == "order")$name
    family <- filter(taxdata_lst[[x]], rank == "family")$name
    species <- filter(taxdata_lst[[x]], rank == "species")$name
    row <- data.frame(cbind(order = order, family = family, species = species))
    taxdata <- bind_rows(taxdata, row)
  }, error = function(e) {
    cat("ERROR :", conditionMessage(e), "\n")
    }
  )
}

# Check which species still don't have taxonomic data 
setdiff(species_count$species, taxdata$species) 
final_taxdata <- merge(taxdata, species_count, by = "species")
#write_csv(select(final_taxdata, c(-obs)), paste0(wd$proc, "insects-taxdata-full.csv")) # DON'T run this or you'll rewrite the manually added data
missing_species <- setdiff(species_count$species, final_taxdata$species) # add the missing taxo data manually for these species

# ----------------------- Visualise Taxonomic Information ----------------------- 

taxo_data <- read_csv(paste0(wd$proc, "insects-taxdata-full.csv"))

taxo_breakdown <- 
  taxo_data %>% 
  group_by(order, family) %>% 
  summarise(n = n()) %>% 
  as.data.frame()

ggplot(taxo_breakdown, aes(x = reorder(order, -n), y = n, fill = reorder(family, -n))) +
  geom_col(position = position_dodge2(width = .9)) +
  labs(title = "Breakdown of Orders and Families of Insect Species in Sentosa",
       subtitle = "Using iNaturalist data and observations from 2005 onwards") +
  xlab("Order") + 
  ylab("Occurrences") +
  geom_text(aes(label = family),
            position = position_dodge2(width = .9),
            vjust = 0.25, hjust = -0.2,
            angle = 90) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid.major.y = element_line(color = "gray")) +
  ylim(0, 32) 

# ----------------------- Map with Ltaer ----------------------- 

#remote::install_github("shaunkhoo/ltaer") 

full <- merge(clean, taxo_data, by.x = "scientific_name_simple", by.y = "species", all.x = TRUE)
full <- full %>% rename("Longitude" = "longitude", "Latitude" = "latitude") # naming required by ltaer's functions

lepidoptera <- full %>% filter(order == "Lepidoptera")
ltaer::exploreSGMap(lepidoptera, colour = "red", size = 1.8, alpha = 0.5, popup = "scientific_name_simple") 

odonata <- full %>% filter(order == "Odonata")
ltaer::exploreSGMap(odonata, colour = "blue", size = 1.8, alpha = 0.5, popup = "scientific_name_simple")

# The below uses ltaer::sg_map, but zoom=1, can't change unless you use Google Maps API and get another instance of the map
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

# ----------------------- Exploring Mapping with Leaflet ----------------------- 

library("leaflet")
library("htmlwidgets")

full <- merge(clean, taxo_data, by.x = "scientific_name_simple", by.y = "species", all.x = TRUE)

full <- full %>%
  mutate(popup_text = paste("<i>", scientific_name_simple, "</i><br/>",
                            observed_on, "<br/>",
                            "by:", user_login))

table(full$order)
lepidoptera <- full %>% filter(order == "Lepidoptera")
odonata <- full %>% filter(order == "Odonata")
hemiptera <- full %>% filter(order == "Hemiptera")
hymenoptera <- full %>% filter(order == "Hymenoptera")
coleoptera <- full %>% filter(order == "Coleoptera")
orthoptera <- full %>% filter(order == "Orthoptera")

insectorder <- colorFactor(c("#fb3640", "#1d3461", "#758bfd", "#c5d86d", "#c04cfd", "#1b998b", "#ff6700"),
                           domain = unique(full$order))

leaflet(full) %>%
  
  # base groups
  setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>%
  addTiles(group = "Street View") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  # overlay groups
  addCircleMarkers(data = lepidoptera, ~longitude, ~latitude, 
                   clusterOptions = markerClusterOptions(),
                   popup = ~popup_text, radius = 2, stroke = T, fillOpacity = 1, color = "#fb3640", group = "Lepidoptera") %>%
  addCircleMarkers(data = odonata, ~longitude, ~latitude, 
                   clusterOptions = markerClusterOptions(),
                   popup = ~popup_text, radius = 2, stroke = T, fillOpacity = 1, color = "#1d3461", group = "Odonata") %>%
  addCircleMarkers(data = hemiptera, ~longitude, ~latitude,
                   popup = ~popup_text, radius = 2, stroke = T, fillOpacity = 1, color = "#758bfd", group = "Hemiptera") %>%
  addCircleMarkers(data = hymenoptera, ~longitude, ~latitude,
                   popup = ~popup_text, radius = 2, stroke = T, fillOpacity = 1, color = "#1b998b", group = "Hymenoptera") %>%
  
  # layers control
  addLayersControl(
    baseGroups = c("Light", "Street View", "Satellite"),
    overlayGroups = c("Lepidoptera", "Odonata", "Hemiptera", "Hymenoptera"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Consider using your own google api key
ggmap::register_google(key = "")
map <- get_map("singapore", maptype = "roadmap", zoom = 11, source = "google", color = "bw")

# ----------------------- Top Contributors ----------------------- 

users <- clean %>%
  select(user_id, user_login) %>%
  st_drop_geometry() %>%
  unique()

# Examine top contributors and their record count, i.e. anyone with no. of submissions > 1
top_contributors <- 
  table(clean$user_id) %>% 
  sort(decreasing = TRUE) %>% 
  as.data.frame() %>%
  dplyr::rename("user_id" = 1, "num_contributions" = 2) %>% 
  dplyr::filter(num_contributions > 1) %>%
  merge(y = users, by = "user_id", all.x = TRUE)

nrow(top_contributors)

ggplot(data = top_contributors, aes(x = num_contributions, y = reorder(user_login, num_contributions))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Contribution Count of Top iNaturalist Users in Sentosa",
       subtitle = "Using Insecta data and observations from 2005 onwards") +
  xlab("Number of Records Submitted") + ylab("Contributor") +
  geom_text(aes(label = num_contributions), hjust = -0.5) +
  xlim(0, 450) +
  theme_minimal()

top_contributions <- top_contributors %>%
  merge(y = clean, by = "user_id", all.x = TRUE) %>%
  dplyr::group_by(user_id) 

# Break down the species within each person's contributions
top_contributions_by_species <-
  top_contributions %>%
  dplyr::count(scientific_name_simple) %>%
  merge(y = users, by = "user_id", all.x = TRUE) %>%
  dplyr::rename("species" = "scientific_name_simple", "num_contributions" = "n")

ggplot(data = top_contributions_by_species, aes(fill = species, x = num_contributions, y = reorder(user_login, num_contributions))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Contribution Count of Top iNaturalist Users in Sentosa, by Species",
       subtitle = "Using Insecta data and observations from 2005 onwards") +
  xlab("Number of Records Submitted") + ylab("Contributor") +
  viridis::scale_fill_viridis(discrete = T) +
  xlim(0, 450) +
  theme(legend.position = "none") 

# Break down the years of each person's contributions
top_contributions_by_year <- 
  top_contributions %>%
  mutate(year = as.factor(format(as.Date(observed_on), format = "%Y"))) %>%
  mutate(month = as.factor(format(as.Date(observed_on), format = "%m"))) %>%
  count(year) %>%
  merge(y = users, by = "user_id", all.x = TRUE) %>%
  rename("num_contributions" = "n")

levels(top_contributions_by_year$year) <- 
  levels(top_contributions_by_year$year)[order(as.numeric(levels(top_contributions_by_year$year)))]

ggplot(data = top_contributions_by_year, aes(fill = year, x = num_contributions, y = reorder(user_login, num_contributions, sum))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Contribution Count of Top iNaturalist Users in Sentosa, by Year",
       subtitle = "Using Insecta data and observations from 2005 onwards") +
  xlab("Number of Records Submitted") + ylab("Contributor") +
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1) +
  xlim(0, 450)

# And without the highest contributor?
contributions_by_year <-
  top_contributions_by_year %>%
  filter(user_login != "big-simonchan")

ggplot(data = contributions_by_year, aes(fill = year, x = num_contributions, y = reorder(user_login, num_contributions, sum))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Contribution Count of Top iNaturalist Users in Sentosa, by Year",
       subtitle = "Using Insecta data and observations from 2005 onwards (without big-simonchan)") +
  xlab("Number of Records Submitted") + ylab("Contributor") +
  viridis::scale_fill_viridis(discrete = TRUE, direction = -1) +
  xlim(0, 50)


# ----------------------- Contributors by Year and Month ----------------------- 

clean <- 
  clean %>%
  mutate(year = as.factor(format(as.Date(observed_on), format = "%Y"))) %>%
  mutate(month = as.factor(format(as.Date(observed_on), format = "%m")))

contributions_yearly <- 
  clean %>%
  group_by(year) %>%
  st_drop_geometry() %>%
  count(year)

contributions_monthly <-
  clean %>%
  group_by(month) %>%
  st_drop_geometry() %>%
  count(month)