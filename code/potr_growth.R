## Script: potr_growth.R
## Interspecific Workshop
## Authors: NR & BK (2026)
## Forest Ecology Research Lab
## Wilfrid Laurier University

# This script is used to visualize POTR growth trends by region

# LOAD LIBRARIES ----

# load libraries
library(here)      # for relative working directory 
library(tidyverse) # dplyr, forcats, gglot2, lubridate, purrr, readr, stringr, tibble, tidyr
library(leaflet)   # mapping sites
library(sf)        # mapping sites
library(ggridges)  # data viz
library(dplR)      # dendro
library(treeclim)  # dendro
library(TRADER)    # dendro
library(graphics)  # dendro
library(utils)     # dendro

# set theme
theme_set(theme_classic())


# READ IN DATA ----

# read in ring count data - BK updated raw collection data on July 29th, 2026
# read in txt files
a_potr <- read_tsv(here::here("data/raw/tree_ring_width_data/trial_tree_ring_width_data/A_POTR_attributes.txt"))
view(a_potr)
c_potr <- read_tsv(here::here("data/raw/tree_ring_width_data/trial_tree_ring_width_data/C_POTR_attributes.txt"))
view(c_potr)

# read in rwl data
a_potr_rwl <- read.tucson(here::here("data/raw/tree_ring_width_data/trial_tree_ring_width_data/A_POTR.rwl"))
view(a_potr_rwl)
c_potr_rwl <- read.tucson(here::here("data/raw/tree_ring_width_data/trial_tree_ring_width_data/C_POTR.rwl"))
view(c_potr_rwl)

# POTR BASAL AREA INCREMENT ----

# ring count includes 638 breast height measurements

# Calculate BAI using the inside-out method (pith to bark)
# converts ring-width series (mm) to ring-area series (mm^2)
bai_a_potr <- bai.in(rwl = a_potr_rwl) %>%
  as.data.frame() %>%
  rownames_to_column(var = "year") 
View(bai_a_potr)

# convert to long format 
bai_a_potr_long <- bai_a_potr %>%
  pivot_longer(
    cols = -year,
    names_to = "series_id",
    values_to = "area") %>%
  drop_na(area) %>%
  mutate(decade = substr(as.character(series_id), 1, 2)) %>%
  mutate(site = str_extract(series_id,  ".*(?=T)")) %>%  # take everything before T
  mutate(site_rep = str_extract(site, "(?<=F).*"))  %>%    # full everything F (only had margins in 2025)
  mutate(tree = str_extract(series_id, "(?<=T).*"))  %>%    # take everything after T
  mutate(unique_id = paste(decade, site_rep, tree, sep = "-")) # make a unique_id column that is SideID-TreeNumber to be used later to join with ring width data
View(bai_a_potr_long)

# make plot with individual trees
bai_a_potr_long %>%
  dplyr::filter(year > 1979) %>%
  ggplot(aes(x = year, y = area, group = unique_id)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# calculate site-level means:
bai_a_potr_long_summary <- bai_a_potr_long %>%
  group_by(year, site) %>%
  summarise(
    mean_area = mean(area, na.rm = TRUE),
    n = n(),
    sd = sd(area, na.rm = TRUE),
    se = sd / sqrt(n),
    ci = 1.96 * se,
    .groups = "drop"
  )
View(bai_a_potr_long_summary)
# make plot with means instead of individual trees
bai_a_potr_long_summary %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::filter(!site == "50B5") %>% # something weird with this site
  ggplot(aes(x = year, y = mean_area, group = site)) +
  #geom_ribbon(aes(ymin = mean_area - ci, ymax = mean_area + ci), alpha = 0.25, colour = NA) +
  geom_line(size = 2, color = "orange") +
  geom_vline(xintercept = 1990, color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~site, scales = "free_y") +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020))


# ORGANIZE SITE BY FIRE DISTRICT


sites <- readr::read_csv(here::here("data/raw/dendro_site.csv"))
sites_sf <- st_as_sf(sites, coords = c("x", "y"), crs = 4326)

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

# Check which geometries are invalid                                             #done to fix an error 
st_is_valid(perimeters_all)

# Repair invalid geometries
perimeters_all <- st_make_valid(perimeters_all)


# Make sure both datasets use the same coordinate system                         #may be redundant?
sites_sf <- st_transform(sites_sf, st_crs(perimeters_all))

# Spatial join
sites_fire <- st_join(sites_sf,perimeters_all,join = st_intersects)   
view(sites_fire)


# PLOT PELLY CROSSING SITES BY BAI 

#pull the pelly crossing sites from sites_fire and then get the BAI data for those sites from bai_a_potr_long. Plot the data 

 
