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

# C_POTR
c_potr_longer <- c_potr %>% 
  dplyr::select(-Year) %>%
  dplyr::rename(  "80F35T33C" = "80F35T33C_REDO") %>%
  #dplyr::select(ends_with("C")) %>%
  pivot_longer(everything(), names_to = "tree_id", values_to = "value") %>%
  dplyr::filter(!is.na(value)) %>%
  group_by(tree_id) %>% summarise(num_rings = n()) %>%
  mutate(decade = str_sub(as.character(tree_id), 1, 2)) %>% 
  mutate(site = str_extract(tree_id, "^[^T]+")) %>%
  mutate(species = "POTR") %>%
  mutate(core = "basal")
#View(c_potr_longer)

process_tree_data <- function(df, species_name = "POTR", core_type = "basal") {
  df %>% 
    dplyr::select(-Year) %>%
    dplyr::rename("80F35T33C" = "80F35T33C_REDO") %>%
    pivot_longer(everything(), names_to = "tree_id", values_to = "value") %>%
    dplyr::filter(!is.na(value)) %>%
    group_by(tree_id) %>% 
    summarise(num_rings = n(), .groups = "drop") %>%
    mutate(
      decade = str_sub(as.character(tree_id), 1, 2),
      site = str_extract(tree_id, "^[^T]+"),
      species = species_name,
      core = core_type
    )
}







