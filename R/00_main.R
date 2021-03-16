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
library(corrr)

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

source("R/01_tidy_data.R")
source("R/02_geographic_map.R")
source("R/03_tidy_acdom_spectra.R")
source("R/04_visualize_absorption_spectra_by_area.R")
source("R/05_extract_ferrari_2000_extra_data.R")
source("R/06_clean_nutrient.R")
source("R/07_clean_irradiance.R")
source("R/08_clean_reflectance.R")
source("R/09_clean_ac9.R")
source("R/10_valente_2019.R")
source("R/11_average_absorption_spectra_by_area.R")
source("R/12_boxplot_doc_by_area.R")
source("R/13_boxplot_acdom_by_area.R")

