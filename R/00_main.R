# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  COSTLOOC data paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

# renv::install("MilesMcBain/breakerofchains")
# renv::install("mcguinlu/pathformatr")
# renv::install("ropensci/rnaturalearthdata")
# renv::install("ropensci/rnaturalearthhires")
# renv::install("PMassicotte/ggpmthemes")
# renv::install(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# renv::install("Enchufa2/rspm")

# renv::repair()

# %% ---- setup
library(tidyverse)
library(stars)
library(sf)
library(here)
library(MBA)
library(assertr)
library(glue)
library(tabulizer)
library(patchwork)
library(ggpmthemes)
library(terra)
library(arrow)
library(googlesheets4)
library(gt)

# Set default ggplot2 font size and font family
theme_set(theme_light_modified(base_family = "Montserrat", base_size = 10))

theme_update(
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 10, color = "#4c4a4a")
)
# %%

# Scripts -----------------------------------------------------------------

# %% ----
source(here("R", "01_tidy_data.R"))
source(here("R", "02_tidy_acdom_spectra.R"))
source(here("R", "03_tidy_spmr_vertical_profiles.R"))
source(here("R", "04_visualize_absorption_spectra_by_area.R"))
source(here("R", "05_extract_ferrari_2000_extra_data.R"))
source(here("R", "06_clean_nutrient.R"))
source(here("R", "07_clean_irradiance.R"))
source(here("R", "08_clean_reflectance.R"))
source(here("R", "09_clean_ac9.R"))
source(here("R", "10_valente_2019.R"))
source(here("R", "11_average_absorption_spectra_by_area.R"))
source(here("R", "12_boxplot_doc_by_area.R"))
source(here("R", "13_boxplot_acdom_by_area.R"))
source(here("R", "14_pigments_vs_absorption.R"))
source(here("R", "15_calculate_station_distance_to_shore.R"))
source(here("R", "16_absorption_partition.R"))
source(here("R", "17_calculate_snap.R"))
source(here("R", "18_extract_bathymetry.R"))
# %%

# Figures for the manuscript ----------------------------------------------

# %% ----
source(here("R", "fig01.R"))
source(here("R", "fig02.R"))
source(here("R", "fig03.R"))
source(here("R", "fig04.R"))
source(here("R", "fig05.R"))
source(here("R", "fig06.R"))
source(here("R", "fig07.R"))
source(here("R", "fig08.R"))
# %%
