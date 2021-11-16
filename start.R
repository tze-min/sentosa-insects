
### Start 
# This file loads packages used throughout this repository. All R scripts under the /code subfolder will have
# source("start.R") as their first line, which then runs this script.

# data management
library("readr")
library("dplyr")
library("tidyr")

# data viz
library("ggplot2")
library("ggtext")
library("ggmap")

# interactive map
library("htmltools")
library("leaflet")

# rasters / vector management
library("rgdal")
library("sf")

wd <- list()
wd$root <- getwd()
wd$code <- file.path(getwd(), "code")
wd$raw <- file.path(getwd(), "data", "raw")
wd$proc <- file.path(getwd(), "data", "processed")
wd$img <- file.path(getwd(), "images")
wd$map <- file.path(getwd(), "maps")
