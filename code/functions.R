
### Functions

# This script contains a list of functions that I've extracted from the other R files in the code folder and 
# generalised to suit the format of any dataset. 

# ----------------------- Make Leaflet Map by Order ----------------------- 

makemap_order <- function(data, order_radio = TRUE, cluster = FALSE, theme = "Satellite") {
  # This is a function to create a HTML Leaflet map that plots the points from your given occurrence dataset, 
  # broken down by the orders (taxonomy) present in the data.
  #
  # Parameters
  #   data :        Your occurrence dataset ought to have at least the 6 columns:
  #                     scientific_name, date_observed, observer_name, order, longitude, latitude
  #                 Although, scientific_name, date_observed, and observer_name can be NA values as they are 
  #                 used to create the textboxes that pop up when you hover over a point in the map.
  #   order_radio : If TRUE, the order to be displayed will be selectable as radio buttons. If FALSE, the 
  #                 orders will be selectable as checkboxes and you can thus display mutliple orders on the
  #                 same map (default = TRUE).
  #   cluster :     If TRUE, the plotted points will be clustered when close together (default = FALSE).
  #   theme :       The basemap theme your points are plotted on top of. Valid values include "Voyager", "Dark",
  #                 "Light", "Street" and "Satellite".
  
  library("dplyr")
  library("htmltools")
  library("htmlwidgets")
  
  data <- data %>% mutate(popup_text  = paste("<i>", scientific_name, "</i><br/>",
                                              date_observed, "<br/>",
                                              "by", observer_name))
  
  dfs_byorder <- data %>% split(list(.$order))
  dfs_byorder <- dfs_byorder[order(sapply(dfs_byorder, nrow), decreasing = TRUE)]
  
  basemap <- leaflet(data) %>% 
    setView(lng = 103.8303, lat = 1.2494, zoom = 15) %>% # set to Singapore's coordinates
    purrr::when(theme == "Voyager" ~ addProviderTiles(., providers$CartoDB.Voyager),
                theme == "Dark" ~ addProviderTiles(., providers$CartoDB.DarkMatter),
                theme == "Light" ~ addProviderTiles(., providers$CartoDB.Positron),
                theme == "Street" ~ addProviderTiles(., providers$OpenStreetMap),
                theme == "Satellite" ~ addProviderTiles(., providers$Esri.WorldImagery))
  
  num_of_orders <- length(dfs_byorder)
  names_of_dfs <- names(dfs_byorder)
  ordermap <- basemap
  
  if (order_radio == TRUE) {
    # all data points will be red
    
    if (cluster == TRUE) {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = "red",
                           group = names_of_dfs[i],
                           clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option
      }
      
    } else {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1.2, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = "red",
                           group = names_of_dfs[i])
      }
    }
    
    ordermap <- ordermap %>%
      addLayersControl(
        baseGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE))
    
  } else {
    # order_radio == FALSE: all data points will have different colors across insect orders
    
    colors_per_df <- RColorBrewer::brewer.pal(num_of_orders, "Set1") # use a qualitative color palette
    
    if (cluster == TRUE) {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = colors_per_df[i],
                           group = names_of_dfs[i],
                           clusterOptions = markerClusterOptions()) # the only difference between this and else cluster option)
      }
      
    } else {
      for (i in seq(1:num_of_orders)) {
        df <- dfs_byorder[i][[1]]
        ordermap <- ordermap %>% 
          addCircleMarkers(data = df,
                           ~longitude, ~latitude,
                           label = lapply(df$popup_text, htmltools::HTML),
                           radius = 1, stroke = TRUE, opacity = 1, fillOpacity = 1,
                           color = colors_per_df[i],
                           group = names_of_dfs[i])
      }
    }
    ordermap <- ordermap %>%
      addLayersControl(
        overlayGroups = names_of_dfs,
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend("bottomright", 
                colors = colors_per_df,
                labels = names_of_dfs) %>%
      hideGroup(names_of_dfs[2:num_of_orders])
  }
  
  saveWidget(ordermap, file = file.path(wd$map, "orders.html"), title = "Records by Order")
}

# Prepare your dataset and make sure the column names align with what's required
full <- initialise_script04() %>%
  dplyr::select(scientific_name_simple, observed_on, user_login, order, longitude, latitude) %>%
  dplyr::rename(scientific_name = scientific_name_simple) %>%
  dplyr::rename(date_observed = observed_on) %>%
  dplyr::rename(observer_name = user_login)

# Make map!
makemap_order(data = full, order_radio = TRUE, cluster = FALSE, theme = "Voyager")
