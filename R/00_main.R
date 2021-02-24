# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  COSTLOOK data paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(MBA)
library(assertr)
library(glue)
library(tabulizer)
library(patchwork)

# library(GGally) # For ggpair

library(ggpmthemes)

# Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_light_modified(base_family = "Montserrat Alternates", base_size = 10))

theme_update(
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 10, color = "#4c4a4a")
)

# Scripts -----------------------------------------------------------------

source("R/01_01_tidy_data.R")
source("R/01_02_correction_absorption_spectra_background_values.R")
source("R/01_03_tidy_absorption_spectra.R")
source("R/02_geographic_map.R")
source("R/03_visualize_absorption_spectra_by_area.R")
source("R/04_extract_ferrari_2000_extra_data.R")
