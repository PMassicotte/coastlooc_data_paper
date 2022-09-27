# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the reflectance data
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "ggspectral.R"))

stations <- read_csv(here("data", "raw", "tidied", "surface_data.csv"))

reflectance <- stations |>
  select(
    station,
    matches("^measured_r")
  ) |>
  pivot_longer(
    -station,
    names_pattern = c("(.*\\_r_)(\\d{3})(_.*)"),
    names_to = c(".value", "wavelength", "unit"),
    names_transform = list(wavelength = parse_integer)
  ) |>
  select(-unit) |>
  rename_with(~ str_replace_all(., "r_$", "reflectance")) |>
  mutate(across(c(measured_reflectance), ~ . / 100))

reflectance

# Add unit to reflectance variable
reflectance <- reflectance |>
  rename(measured_reflectance_percent = measured_reflectance)

reflectance

# %% ---- Only keep reflectance bwtween 0-1 (0-100%)
reflectance <- reflectance |>
  mutate(measured_reflectance_percent = case_when(
    between(measured_reflectance_percent, 0, 1) ~ measured_reflectance_percent,
    TRUE ~ NA_real_
  ))

reflectance
# %%

# %% ---- Visualization
stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

df_viz <- reflectance |>
  inner_join(stations, by = "station")

p_reflectance <- ggspectral(df_viz, measured_reflectance_percent, "Reflectance")

save_fun(p_reflectance, here("graphs", "006_reflectance_profiles_by_area.pdf"))
# %%

# %% ---- Export the data
reflectance |>
  semi_join(stations, by = "station") |>
  write_csv(here("data", "clean", "reflectance.csv"))
# %%
