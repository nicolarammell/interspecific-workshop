## Script: map.R
## Interspecific Workshop
## Authors: NR & BK (2026)
## Forest Ecology Research Lab
## Wilfrid Laurier University

# This script is used to visualize sites in terms of their location in the Yukon

# LOAD LIBRARIES ----

# load libraries
library(here)      # for relative working directory 
library(tidyverse) # dplyr, forcats, gglot2, lubridate, purrr, readr, stringr, tibble, tidyr
library(leaflet)   # mapping sites
library(sf)        # mapping sites
library(ggridges)  # for making interspecific plot
library(dplR)      # dendro
library(treeclim)  # dendro
library(TRADER)    # dendro
library(graphics)  # dendro
library(utils)     # dendro

# set theme
theme_set(theme_classic())

# READ IN DATA ----

# read in dendro spreadsheet data with site locations 
sites <- readr::read_csv(here::here("data/raw/dendro_site.csv")) 
View(sites)

# read in infectection data from M. Mihorean
infection <- readr::read_csv(here::here("data/raw/Nicolaaspen.csv")) 
View(infection)

# read in fire perimeter shape files (hand-drawn) - provided by Brian Newton (60s, 70s, 80s, 90s, 00s)
perimeters_bn <- st_read("data/raw/fire_perimeters/Rebuilt_fixed_fire_perimeters.shp") %>%
  st_transform(crs = 4326) #leaflet requires data in WGS84 (EPSG:4326)
#View(perimeters_bn)

# read in GeoYukon fire perimeters to add the 1950s perimeters 
# from https://map-data.service.yukon.ca/GeoYukon/Emergency_Management/Fire_History/
perimeters_yt <- st_read("data/raw/Fire_History.shp/Fire_History.shp") %>%
  st_transform(crs = 4326) %>%
  dplyr::filter(DECADE == "1950")
#View(perimeters_yt)

perimeters_all <- bind_rows(perimeters_bn, perimeters_yt)
#View(perimeters_all)


















































