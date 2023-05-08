source("start.R")

obs_ec <- read_csv(file.path(wd$proc, "ECdata15.csv"))
obs_ec$Source <- "Expert Collected"

obs_cs <- read_csv(file.path(wd$proc, "CSdata15.csv"))
obs_cs$Source <- "Citizen Science"

obs <- rbind(obs_ec, obs_cs)
obs_all <- st_as_sf(obs, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

pal <- colorFactor(
  palette = c("navy", "red"),
  domain = obs_all$Source
)

map <- leaflet(obs_all) %>% 
  setView(lng = 103.8198, lat = 1.3521, zoom = 12) %>%
  addTiles(group = "Street") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
  addCircleMarkers(data = obs_all,
                   ~Longitude, ~Latitude,
                   radius = 1, stroke = TRUE, opacity = .7, fillOpacity = .7,
                   color = ~pal(Source)) %>%
  addLegend("bottomright", pal = pal, values = ~Source,
            title = "Collection Source",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )

count <- nrow(obs)

maptitle <- tagList(
  tags$h1(tags$b("Expert Collected and Citizen Science Observations"), tags$style("h1 {display: inline; color: black; font-size: 15px; font-style: italic; text-align: center;}")),
  tags$p(tags$b(paste0("(n = ", count, ")")), tags$style("p {display: inline; color: black; font-size: 14px; text-align: center;}")))

map <- map %>%
  addControl(maptitle, position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("Voyager", "Street", "Satellite"),
    options = layersControlOptions(collapsed = FALSE))

map

saveWidget(map, file = file.path(wd$map, paste0("All Data", ".html")))
