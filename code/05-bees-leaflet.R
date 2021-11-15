
### 05 Bees - Creating Leaflet Maps from Bee Data

# This script deals with a different set of occurrence data - one on bee species across the whole of Singapore.
# Similar to what was done in file 04, this creates a set of Leaflet maps for these bee species instead.

source("start.R")

initialise_script05 <- function() {
  
  # ----------------------- Import Data and Maps -----------------------
  
  library("readxl")
  
  idl <- read_excel(paste0(wd$raw, "IDL_database_28July20.xlsx"))
  
  singapore_map_sp <- readOGR(paste0(wd$raw, "divagis_SGP_adm0-refactored.shp"))
  sentosa_map_sp <- singapore_map_sp[singapore_map_sp$location == "Sentosa", ]
  sentosa_map_sf <- st_as_sf(sentosa_map_sp, crs = 4326)
  
  # ----------------------- Exploration and Cleaning -----------------------
  
  table(idl$Family)
  table(idl$`Species ID`)
  idl %>% filter(`Species ID` == "NUS, Faculty of Science (FoS)") # manually shifted 1 column in xlsx sheet directly
  
  # Extract (=...) if present in Species ID
  extracols <- idl %>% filter(grepl("=", `Species ID`))
  megachile <- idl %>% filter(grepl("Megachile", `Species ID`))
  nobrackets <- idl %>% filter(!grepl("[()]", `Species ID`))
  remaining <- idl %>% 
    filter(!grepl(paste(c("=", "Megachile"), collapse = "|"), `Species ID`)) %>%
    filter(grepl("[(]", `Species ID`))
  
  # Break up into smaller cols
  extracols_clean <-
    extracols %>%
    separate(`Species ID`, c("genus", "subgenus", "species", "additional1", "additional2"), remove = FALSE) %>%
    unite(additional1, additional2, col = "equals", sep = " ", remove = TRUE) %>%
    unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE)
  
  remaining_clean <-
    remaining %>%
    separate(`Species ID`, c("genus", "subgenus", "species", "subspecies"), remove = FALSE) %>%
    unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE)
  
  megachile_clean <-
    megachile %>%
    separate(`Species ID`, c("genus", "subgenus", "species"), sep = "[()]", remove = FALSE) %>%
    unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE) %>%
    mutate_if(is.character, stringr::str_squish)
  
  nobrackets_clean <-
    nobrackets %>%
    separate(`Species ID`, c("genus", "species"), remove = FALSE) %>%
    unite(genus, species, col = "scientific_name_simple", sep = " ", remove = FALSE)
  
  clean <- plyr::rbind.fill(extracols_clean, remaining_clean, megachile_clean, nobrackets_clean)
  
  as.data.frame(colSums(is.na(clean)))
  
  # Some statistics:
  # Longitude on Label (Coordinates) = 128 NAs
  # Location Latitude = 150 NAs
  # Label Latitude (numeric) = 0 NAs
  # Collector(s) / Collector(s) concatenated = 827 NAs
  # Collection Date = 163 NAs
  
  #clean %>% filter(is.na(`Collection Date`))
  
  full <- 
    clean %>% 
    rename(longitude = `Label Longitude (numeric)`, latitude = `Label Latitude (numeric)`) %>%
    select(`Unique Identifier`, `Species ID`, scientific_name_simple, `Collection Date`, `Collection Year (Singapore)`, `Collector(s)`, longitude, latitude)
    st_as_sf(full, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)
    
  return(full)
}

# ----------------------- Map per Bee Species -----------------------

full <- initialise_script05() %>% 
  mutate(popup_text = paste("<i>", `Species ID`, "</i><br/>",
                            `Collection Date`, "<br/>", 
                            "by", `Collector(s)`, "<br/>",
                            `Unique Identifier`))

makemap_species <- function(data, speciesname, cluster = FALSE) {
  # species must be only genus + species, no subspecies taken into account
  
  df <- data %>% filter(`Species ID` == speciesname)
  count <- nrow(df)
  
  maptitle <- tagList(
    tags$h1(tags$b(speciesname), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")),
    tags$p(tags$b(paste0("(", count, ")")), tags$style("p {display: inline; color: black; font-size: 14px; text-align: center;}")))
  
  map <- leaflet(df) %>% 
    setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
    addTiles(group = "Street") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    #addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    #addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager")
  
  if (cluster == TRUE) {
    map <- map %>%
      addCircleMarkers(data = df,
                       ~longitude, ~latitude,
                       label = lapply(df$popup_text, htmltools::HTML),
                       radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                       color = "red",
                       clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option
    
  } else {
    map <- map %>%
      addCircleMarkers(data = df,
                       ~longitude, ~latitude,
                       label = lapply(df$popup_text, htmltools::HTML),
                       radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                       color = "red")
    
  }
  
  map %>%
    addControl(maptitle, position = "bottomleft") %>%
    addLayersControl(
      baseGroups = c("Voyager", "Street", "Satellite"),
      options = layersControlOptions(collapsed = FALSE))
  
}

makemap_species(data = full, speciesname = "Tetragonula (Tetragonula) valdezi (=laeviceps Sakagami)", cluster = FALSE)

unique(full$`Species ID`)
table(full$`Species ID`)
