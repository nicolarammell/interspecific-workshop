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
infection <- readr::read_csv(here::here("data/raw/Nicolaaspen.csv")) %>%
  group_by(siteID) %>% tally(margin_tree) %>%
  drop_na(n) %>%
  dplyr::filter(!siteID == "NEAR 80F24") %>%
  mutate(n = as.character(n)) %>%
  mutate(site_infection = if_else(n == "0", "No", "Yes")) %>%
  dplyr::rename(num_margin_trees = n) %>%
  dplyr::rename(site = siteID)
View(infection)

# join df together
sites_infection <- left_join(infection, sites, by = "site")
View(sites_infection)

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

 # VISUALIZATION ----

#custom palette
custom_colours <- c('#feebe2','#fcc5c0','#fa9fb5','#f768a1','#c51b8a','#7a0177')
                    
palette <- colorFactor(palette = custom_colours, domain = perimeters_all$DECADE)

circle_colours <- c("lightgreen","lightgreen","darkgreen","orange")
  
palette_sites <- colorFactor(palette = circle_colours, domain = sites$spp_samples)

#creating the map
   sites_infection %>%
  dplyr::filter(str_detect(spp_sampled, ('pop_tr'))) %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldTopoMap") %>% # Esri.WorldTopoMap; # Esri.WorldImagery; #Esri.WorldStreetmap
  addPolygons(data = perimeters_all,
              fillColor = ~palette(DECADE), 
              color = "black", 
              fillOpacity = 0.5, 
              weight = 1) %>%
  addCircleMarkers(lng = ~x, lat = ~y, 
                   color = ~palette_sites(spp_sampled), 
                   label = ~site, 
                   ~ifelse(site_infection == "Yes", 5, 1),) %>% 
  addLegend(data = perimeters_all, 
            pal = palette, 
            values = ~DECADE, 
            title = "Burn Decade") %>%
  addLegend(
    colors = c("lightgreen","lightgreen","darkgreen","orange"), 
    labels = c('aspen and spruce', "aspen and white spruce", "aspen and black spruce", "aspen"),
    title = "Species Sampled", 
    position = "bottomleft")
   
   
                   
                   
  
              
              

















































