## Script: data_viz.R
## Interspecific Workshop
## Authors: NR & BK (2026)
## Forest Ecology Research Lab
## Wilfrid Laurier University

# This script is used to visualize interspecific tree age data from 2024/2025 sites
# sampled in Yukon Territory. 

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

# read in ring count data - NR & BK created these raw collection files April 28th, 2026
c_potr <- read_tsv(here::here("data/raw/interspecific_workshop_C_POTR_tab.txt"))  # read_tsv is for tab-separated
#View(c_potr)
a_potr <- read_tsv(here::here("data/raw/interspecific_workshop_A_POTR_tab.txt"))  # read_tsv is for tab-separated
#View(a_potr)
a_pigl <- read_tsv(here::here("data/raw/interspecific_workshop_A_PIGL_tab.txt"))  # read_tsv is for tab-separated
#View(a_pigl)
c_pigl <- read_tsv(here::here("data/raw/interspecific_workshop_C_PIGL_tab.txt"))  # read_tsv is for tab-separated
#View(c_pigl)

# read in rwl data
c_potr_rwl <- read.tucson(here::here("data/raw/interspecific_workshop_C_POTR.rwl"))
#View(c_potr_rwl)
c_pigl_rwl <- read.tucson(here::here("data/raw/interspecific_workshop_C_PIGL.rwl"))
#View(c_pigl_rwl)
a_potr_rwl <- read.tucson(here::here("data/raw/interspecific_workshop_A_POTR.rwl"))
a_pigl_rwl <- read.tucson(here::here("data/raw/interspecific_workshop_A_PIGL.rwl"))


# CLEAN DATA ----

# process_tree_data function to pivot longer
process_tree_data <- function(df) {
  df %>% 
    dplyr::select(-Year) %>%
    pivot_longer(everything(), names_to = "tree_id", values_to = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(tree_id) %>% 
    summarise(num_rings = n(), .groups = "drop") %>%
    mutate(
      decade = str_sub(as.character(tree_id), 1, 2),
      site = str_extract(tree_id, "^[^T]+")
    )
}

# pivot 4 dataframes longer
c_potr_longer <- c_potr %>%
  dplyr::rename("80F35T33C" = "80F35T33C_REDO") %>%
  process_tree_data() %>%
  mutate(species = "POTR", core = "basal")
#View(c_potr_longer)

a_potr_longer <- a_potr %>%
  process_tree_data() %>%
  mutate(species = "POTR", core = "breast")
#View(a_potr_longer)

c_pigl_longer <- c_pigl %>%
  process_tree_data() %>%
  mutate(species = "PIGL", core = "basal")
#View(c_pigl_longer)

a_pigl_longer <- a_pigl %>% 
  process_tree_data() %>%
  mutate(species = "PIGL", core = "breast")
#View(a_potr_longer)

# bind the 4 tables together 
ring_counts <- bind_rows(c_potr_longer, a_potr_longer, c_pigl_longer, a_pigl_longer)
view(ring_counts)

# PLOT RING COUNTS ----

# make basic plot
ring_counts %>%
  ggplot(aes(x = site, y = num_rings, shape = core, colour = species)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(basal = 17, breast = 16)) +
  coord_flip()

# make ggridges plot - breast height samples 
ring_counts %>%
  dplyr::filter(core == "breast") %>%
  ggplot(aes(x = num_rings, y = site, fill = species)) +
  geom_density_ridges(alpha = 0.5, scale = 1, color = "black", rel_min_height = 0.01) +
  # raw points underneath
  geom_point(aes(color = species), 
             position = position_jitter(height = 0.05, width = 0), 
             size = 2, alpha = 0.8) +
  scale_fill_manual(values = c(PIGL = "darkgreen", POTR = "orange")) +
  scale_color_manual(values = c(PIGL = "darkgreen", POTR = "orange")) +
  scale_x_continuous(breaks = seq(0, max(ring_counts$num_rings, na.rm = TRUE), by = 10)) +
  theme_classic(base_size = 20) +
  xlab("Number of tree rings") +
  ylab("Site") +
  ggtitle("Breast height samples")

# Save as PNG in working directory
ggsave("code/figures/breast_plot.png", width = 15, height = 12, dpi = 300)


# make ggridges plot - basal height samples 
ring_counts %>%
  dplyr::filter(core == "basal") %>%
  ggplot(aes(x = num_rings, y = site, fill = species)) +
  geom_density_ridges(alpha = 0.5, scale = 1, color = "black", rel_min_height = 0.01) +
  # raw points underneath
  geom_point(aes(color = species), 
             position = position_jitter(height = 0.05, width = 0), 
             size = 2, alpha = 0.8) +
  scale_fill_manual(values = c(PIGL = "darkgreen", POTR = "orange")) +
  scale_color_manual(values = c(PIGL = "darkgreen", POTR = "orange")) +
  scale_x_continuous(breaks = seq(0, max(ring_counts$num_rings, na.rm = TRUE), by = 10)) +
  theme_classic(base_size = 20) +
  xlab("Number of tree rings") +
  ylab("Site") +
  ggtitle("Basal height samples")

# Save as PNG in working directory
ggsave("code/figures/basal_plot.png", width = 15, height = 12, dpi = 300)


# make ggridges plot - basal PIGL breast POTR height samples 
ring_counts %>%
  dplyr::filter(case_when (species == "POTR" ~ core == "breast",species == "PIGL" ~ core == "basal")) %>%
  ggplot(aes(x = num_rings, y = site, fill = species)) +
  geom_density_ridges(alpha = 0.5, scale = 1, color = "black", rel_min_height = 0.01) +
  # raw points underneath
  geom_point(aes(color = species), 
             position = position_jitter(height = 0.05, width = 0), 
             size = 2, alpha = 0.8) +
  scale_fill_manual(values = c(PIGL = "darkgreen", POTR = "orange")) +
  scale_color_manual(values = c(PIGL = "darkgreen", POTR = "orange")) +
  scale_x_continuous(breaks = seq(0, max(ring_counts$num_rings, na.rm = TRUE), by = 10)) +
  theme_classic(base_size = 20) +
  xlab("Number of tree rings") +
  ylab("Site") +
  ggtitle("basal PIGL breast POTR height samples")

# Save as PNG in working directory
ggsave("code/figures/basal_breast_plot.png", width = 15, height = 12, dpi = 300)

# BUILD CHRONOLOGY ----

# BASAL POTR
# detrend all series at once - once you know which option is best for your data
grow.rwi <- detrend(rwl = c_potr_rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
spag.plot(rwl = grow.rwi, zfac = 1, useRaster = FALSE, res = 300)  # now see reverse pattern
rwi_stats <- rwi.stats(c_potr_rwl) # stats for entire chronology
rwi_stats # rbar.tot is average correlation across segments
rwi_stats_run <- rwi.stats.running(c_potr_rwl) # running stats - time periods can be adjusted

# build chronology with auto-regressive modelling (AR), this produces RESIDUAL crn
# i.e., minimizes effect of one year on the next (aka stored reserves)
grow.crn <- chron(x = c_potr_rwl, prefix = "", biweight = TRUE, prewhiten = TRUE) # NOT STANDARDIZED
plot.crn(grow.crn, add.spline = TRUE) #plot crn

# BASAL PIGL
# detrend all series at once - once you know which option is best for your data
grow.rwi <- detrend(rwl = c_pigl_rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
spag.plot(rwl = grow.rwi, zfac = 1, useRaster = FALSE, res = 300)  # now see reverse pattern
rwi_stats <- rwi.stats(c_pigl_rwl) # stats for entire chronology
rwi_stats # rbar.tot is average correlation across segments
rwi_stats_run <- rwi.stats.running(c_pigl_rwl) # running stats - time periods can be adjusted

# build chronology with auto-regressive modelling (AR), this produces RESIDUAL crn
# i.e., minimizes effect of one year on the next (aka stored reserves)
grow.crn <- chron(x = c_pigl_rwl, prefix = "", biweight = TRUE, prewhiten = TRUE) # NOT STANDARDIZED
plot.crn(grow.crn, add.spline = TRUE) #plot crn

# BREAST POTR
# detrend all series at once - once you know which option is best for your data
grow.rwi <- detrend(rwl = a_potr_rwl, method = c("Spline"), nyrs = NULL, f = 0.5, pos.slope = FALSE)
spag.plot(rwl = grow.rwi, zfac = 1, useRaster = FALSE, res = 300)  # now see reverse pattern
rwi_stats <- rwi.stats(a_potr_rwl) # stats for entire chronology
rwi_stats # rbar.tot is average correlation across segments
rwi_stats_run <- rwi.stats.running(a_potr_rwl) # running stats - time periods can be adjusted

# build chronology with auto-regressive modelling (AR), this produces RESIDUAL crn
# i.e., minimizes effect of one year on the next (aka stored reserves)
grow.crn <- chron(x = a_potr_rwl, prefix = "", biweight = TRUE, prewhiten = TRUE) # NOT STANDARDIZED
plot.crn(grow.crn, add.spline = TRUE) #plot crn


# POTR BASAL AREA INCREMENT ----

# ring_counts includes 311 POTR trees and 355 PIGL trees

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
  mutate(unique_id = paste(decade, site_rep, tree, sep = "-")) %>% # make a unique_id column that is SideID-TreeNumber to be used later to join with ring width data
  mutate(tree_rep = str_sub(tree, 1, 2)) %>%
  dplyr::filter(!tree_rep > 30)
  View(bai_a_potr_long)

## make plot with individual trees
bai_a_potr_long %>%
  ggplot(aes(x = year, y = area, group = unique_id)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## calculate site-level means:
bai_a_potr_long_summary <- bai_a_potr_long %>%
  group_by(year, site) %>%
  summarise(
    mean_area = mean(area, na.rm = TRUE),
    n = n(),
    sd = sd(area, na.rm = TRUE),
    se = sd / sqrt(n),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(species = "POTR", core = "breast")
View(bai_a_potr_long_summary)

## make plot with means instead of individual trees
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


# PIGL BASAL AREA INCREMENT - BREAST HEIGHT CORES ----

# ring_counts includes 311 POTR trees and 355 PIGL trees

# Calculate BAI using the inside-out method (pith to bark)
# converts ring-width series (mm) to ring-area series (mm^2)
bai_a_pigl <- bai.in(rwl = a_pigl_rwl) %>%
  as.data.frame() %>%
  rownames_to_column(var = "year")
View(bai_a_pigl)

# convert to long format 
bai_a_pigl_long <- bai_a_pigl %>%
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
View(bai_a_pigl_long)

## make plot with individual trees
bai_a_pigl_long %>%
  dplyr::filter(year > 1979) %>%
  ggplot(aes(x = year, y = area, group = unique_id)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## calculate site-level means:
bai_a_pigl_long_summary <- bai_a_pigl_long %>%
  group_by(year, site) %>%
  summarise(
    mean_area = mean(area, na.rm = TRUE),
    n = n(),
    sd = sd(area, na.rm = TRUE),
    se = sd / sqrt(n),
    ci = 1.96 * se,
    .groups = "drop"
  )

## make plot with means instead of individual trees
bai_a_pigl_long_summary %>%
  dplyr::filter(year > 1979) %>%
  ggplot(aes(x = year, y = mean_area, group = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  facet_wrap(~site)

## make plot with means instead of individual trees
bai_a_pigl_long_summary %>%
  mutate(year = as.numeric(year)) %>%
  filter(!site %in% c("60M6", "70F09", "50U6")) %>% # FILTER OUT UNBURNED SITES 
  ggplot(aes(x = year, y = mean_area, group = site)) +
  #geom_ribbon(aes(ymin = mean_area - ci, ymax = mean_area + ci), alpha = 0.25, colour = NA) +
  geom_line(size = 2, color = "darkgreen") +
  geom_vline(xintercept = 1990, color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~site, scales = "free_y") +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020))

# PIGL BASAL AREA INCREMENT - BASAL HEIGHT CORES ----

# ring_counts includes 311 POTR trees and 355 PIGL trees

# Calculate BAI using the inside-out method (pith to bark)
# converts ring-width series (mm) to ring-area series (mm^2)
bai_c_pigl <- bai.in(rwl = c_pigl_rwl) %>%
  as.data.frame() %>%
  rownames_to_column(var = "year")
View(bai_c_pigl)

# convert to long format 
bai_c_pigl_long <- bai_c_pigl %>%
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
#View(bai_c_pigl_long)

## make plot with individual trees
bai_c_pigl_long %>%
  #dplyr::filter(year > 1979) %>%
  ggplot(aes(x = year, y = area, group = unique_id)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

## calculate site-level means:
bai_c_pigl_long_summary <- bai_c_pigl_long %>%
  group_by(year, site) %>%
  summarise(
    mean_area = mean(area, na.rm = TRUE),
    n = n(),
    sd = sd(area, na.rm = TRUE),
    se = sd / sqrt(n),
    ci = 1.96 * se,
    .groups = "drop"
  ) %>%
  mutate(species = "PIGL", core = "basal")


## make plot with means instead of individual trees
bai_c_pigl_long_summary %>%
  #dplyr::filter(year > 1979) %>%
  ggplot(aes(x = year, y = mean_area, group = site)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + 
  facet_wrap(~site)

## make plot with means instead of individual trees
bai_c_pigl_long_summary %>%
  mutate(year = as.numeric(year)) %>%
  filter(!site %in% c("60M6", "70F09", "50U6")) %>% # FILTER OUT UNBURNED SITES 
  ggplot(aes(x = year, y = mean_area, group = site)) +
  #geom_ribbon(aes(ymin = mean_area - ci, ymax = mean_area + ci), alpha = 0.25, colour = NA) +
  geom_line(size = 2, color = "darkgreen") +
  #geom_vline(xintercept = 1990, color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~site, scales = "free_y") +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020))

# JOIN POTR BREAST AND PIGL BASAL INTO ONE PLOT

interspecific <- bind_rows(bai_a_potr_long_summary, bai_c_pigl_long_summary)
view(interspecific)

## make plot with both species means
interspecific %>%
  mutate(year = as.numeric(year)) %>%
  filter(!site %in% c("60M6", "70F09", "50U6")) %>% # FILTER OUT UNBURNED SITES 
  ggplot(aes(x = year, y = mean_area, group = species)) +
  #geom_ribbon(aes(ymin = mean_area - ci, ymax = mean_area + ci), alpha = 0.25, colour = NA) +
  geom_line(size = 2) +
  #geom_vline(xintercept = 1990, color = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(~site, scales = "free_y") +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020))

interspecific %>%
  mutate(year = as.numeric(year)) %>%
  filter(!site %in% c("60M6", "70F09", "50U6")) %>%
  ggplot(aes(x = year, y = mean_area, group = species, color = species)) +
  geom_line(linewidth = 1) +
  facet_wrap(~site, scales = "free_y") +
  #facet_wrap(~site) +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_color_manual(values = c(
    "POTR" = "orange",
    "PIGL" = "darkgreen"
  )) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(
    legend.position = c(0.8, 0),
    legend.direction = "horizontal"
  ) + 
  xlab("") + ylab("Mean Basal Area Increment")
  
# Save as PNG in working directory
ggsave("code/figures/interspecific_site.png", width = 10, height = 6, dpi = 300)

## calculate species-level means:
interspecific_summary <- interspecific %>%
  group_by(year, species) %>%
  summarise(
    species_area = mean(mean_area, na.rm = TRUE),
    n = n(),
    sd = sd(mean_area, na.rm = TRUE),
    se = sd / sqrt(n),
    ci = 1.96 * se,
    .groups = "drop"
  )
  

interspecific_summary %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::filter(year > 1959) %>%
 # filter(!site %in% c("60M6", "70F09", "50U6")) %>%
  ggplot(aes(x = year, y = species_area, group = species, color = species)) +
  geom_ribbon(aes(ymin = species_area - ci, ymax = species_area + ci), alpha = 0.25, colour = NA) +
  geom_line(linewidth = 1) +
  #facet_wrap(~site, scales = "free_y") +
  #facet_wrap(~site) +
  scale_x_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
  scale_color_manual(values = c(
    "POTR" = "orange",
    "PIGL" = "darkgreen"
  )) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal"
  ) + 
  xlab("") + ylab("Mean Basal Area Increment")

# Save as PNG in working directory
ggsave("code/figures/interspecific_species.png", width = 10, height = 6, dpi = 300)


