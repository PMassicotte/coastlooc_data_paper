# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the AC9 data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "ggspectral.R"))

stations <- read_csv(here("data", "raw", "tidied", "surface_data.csv"))

names(stations)

# Do not use dissolved a and c from the AC9 data because there were problems
# with the filtering procedure during the sampling. These correspond to the ad
# and cd columns in the data.

ac9 <- stations |>
  select(
    station,
    matches("^a\\d{3}$"),
    matches("^c\\d{3}$"),
    matches("^c\\d{3}_or_"),
    matches("^a\\d{3}_or_"),
    matches("^bp_\\d{3}$")
  ) |>
  rename_with(~ str_remove(., "_or.*"), contains("_for_")) |>
  pivot_longer(
    -station,
    names_pattern = c("(.*)_?(\\d{3})"),
    names_to = c(".value", "wavelength"),
    names_transform = list(wavelength = parse_integer)
  ) |>
  rename_with(~ str_remove_all(., "_$")) |>
  rename_with(~ paste0(., "_m1"), c(a, c, bp))

ac9

# Should have only 1 observation per wavelength
ac9 |>
  count(station, wavelength) |>
  verify(n == 1)

# Some wavelengths were not exactly the same on the C6 cruise because the
# radiometer had a different configuration.

ac9 <- ac9 |>
  mutate(wavelength = case_when(
    str_starts(station, "C6") & wavelength == 555 ~ 532L,
    str_starts(station, "C6") & wavelength == 630 ~ 555L,
    str_starts(station, "C6") & wavelength == 650 ~ 630L,
    TRUE ~ wavelength
  ))

# Should have only 1 observation per wavelength
ac9 |>
  count(station, wavelength) |>
  verify(n == 1)

# %% ---- Set negative values to NA
ac9 <- ac9 |>
  mutate(across(
    c(a_m1:bp_m1),
    ~ case_when(
      . > 0 ~ .,
      TRUE ~ NA_real_
    )
  ))

ac9
# %%

# %% ---- Visualize spectra

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

df_viz <- ac9 |>
  inner_join(stations, by = "station")

p_a <- ggspectral(df_viz, a_m1, "a~(m^{-1})")
p_c <- ggspectral(df_viz, c_m1, "c~(m^{-1})")
p_bp <- ggspectral(df_viz, bp_m1, "b[p]~(m^{-1})")

save_fun(p_a, here("graphs", "004_ac9_a_spectral_profiles_by_area.pdf"))
save_fun(p_c, here("graphs", "004_ac9_c_spectral_profiles_by_area.pdf"))
save_fun(p_bp, here("graphs", "004_ac9_bp_spectral_profiles_by_area.pdf"))
# %%

# %% ---- Save the data
ac9 |>
  semi_join(stations, by = "station") |>
  write_csv(here("data", "clean", "ac9.csv"))
# %%
