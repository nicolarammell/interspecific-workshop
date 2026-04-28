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




















