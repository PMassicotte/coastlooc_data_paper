# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Quick tests to make sure that the data is ok. For example, no
# negative values in relfectance.
#
# - Not checking for negative absorption values that can occur at longer
# wavelengths.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

all_positive <- function(x) {
  all(x > 0, na.rm = TRUE)
}

# %% ---- Reflectance
read_csv(here("data", "clean", "reflectance.csv")) |>
  assert(all_positive, measured_reflectance_percent)
# %%

# %% ---- Irradiance
read_csv(here("data", "clean", "irradiance.csv")) |>
  assert(all_positive, -station)
# %%

# %% ---- AC9
read_csv(here("data", "clean", "ac9.csv")) |>
  assert(all_positive, -station)
# %%

# %% ---- Slope of CDOM spectra
read_csv(here("data", "clean", "s_cdom.csv")) |>
  assert(all_positive, -station)
# %%

# %% ---- Slope of nap spectra
read_csv(here("data", "clean", "s_nap.csv")) |>
  assert(all_positive, -station)
# %%

# %% ---- Geographical coordinates
read_csv(here("data", "clean", "stations.csv")) |>
  verify(all(between(longitude, -180, 180), na.rm = TRUE)) |>
  verify(all(between(latitude, -90, 90), na.rm = TRUE))
# %%

# %% ---- All wavelength are the same
# TODO:

# What is the "official list" of wavelengths?
# - absorption
# - radiometry

# %%
