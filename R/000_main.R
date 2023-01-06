# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  COSTLOOC data paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# renv::install("MilesMcBain/breakerofchains")
# renv::install("mcguinlu/pathformatr")
# renv::install("ropensci/rnaturalearthdata")
# renv::install("ropensci/rnaturalearthhires")
# renv::install("PMassicotte/ggpmthemes")
# renv::install(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# renv::install("Enchufa2/rspm")
# renv::install("bspm")
# renv::install("languageserver") # For vscode
# renv::install("httpgd") # For vscode
# renv::install("clauswilke/ggisoband")

# renv::record("MilesMcBain/breakerofchains")
# renv::record("mcguinlu/pathformatr")
# renv::record("ropensci/rnaturalearthdata")
# renv::record("ropensci/rnaturalearthhires")
# renv::record("PMassicotte/ggpmthemes")
# renv::record(c("ropensci/tabulizerjars", "ropensci/tabulizer"))
# renv::record("Enchufa2/rspm")
# renv::record("bspm")
# renv::record("languageserver") # For vscode
# renv::record("httpgd") # For vscode
# renv::record("clauswilke/ggisoband")

# renv::repair()

# %% ---- Setup
library(tidyverse)
library(sf)
library(here)
library(MBA)
library(assertr)
library(glue)
library(tabulizer)
library(patchwork)
library(ggpmthemes)
library(terra)
library(googlesheets4)
library(gt)
library(ggbeeswarm)
library(rnaturalearth)
library(kableExtra)
library(breakerofchains)
library(pathformatr)

# Set default ggplot2 font size and font family
theme_set(theme_light_modified(base_family = "Open Sans", base_size = 10))

theme_update(
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 10, color = "#4c4a4a")
)
# %%

# %% ---- Clean up the data

source(here("R", "001_tidy_raw_data.R"))
source(here("R", "002_tidy_stations.R"))
source(here("R", "003_tidy_absorption.R"))
source(here("R", "004_tidy_ac9.R"))
source(here("R", "005_tidy_irradiance.R"))
source(here("R", "006_tidy_reflectance.R"))
source(here("R", "007_tidy_pigments.R"))
source(here("R", "008_tidy_carbon_proxies.R"))
source(here("R", "009_extract_bathymetry.R"))
source(here("R", "010_tidy_data_eu_surface_extrapolation.R"))
source(here("R", "011_valente_2019.R"))
# %%

# %% ---- Basic tests to validate the data
source(here("R", "999_assert.R"))
# %%

# %% ---- Figures for the manuscript

# Figures 3 and 8 were made by Frank Fell.

source(here("R", "fig01.R"))
source(here("R", "fig02.R"))
source(here("R", "fig04.R"))
source(here("R", "fig05.R"))
source(here("R", "fig06.R"))
source(here("R", "fig07.R"))
source(here("R", "fig09.R"))

source(here("R", "appendix_001.R"))
source(here("R", "appendix_002.R"))

# %%
