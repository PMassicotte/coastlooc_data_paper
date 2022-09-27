# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the irradiance data
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "ggspectral.R"))

stations <- read_csv(here("data", "raw", "tidied", "surface_data.csv"))

stations

irradiance <- stations |>
  select(
    station,
    matches("eu_\\d{3}"),
    matches("ed_\\d{3}"),
    matches("ku_\\d{3}"),
    matches("kd_\\d{3}")
  ) |>
  pivot_longer(
    -station,
    names_pattern = c("(.*)_(\\d{3})"),
    names_to = c(".value", "wavelength"),
    names_transform = list(wavelength = parse_integer)
  )

irradiance

# Add units
irradiance <- irradiance |>
  rename_with(~ paste0(., "_w_m2_um"), c(ed, eu)) |>
  rename_with(~ paste0(., "_m1"), c(kd, ku))

irradiance |>
  pivot_longer(-c(station, wavelength)) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ wavelength + name, scales = "free")

# %% ---- Set negative values to NA
irradiance <- irradiance |>
  mutate(across(
    eu_w_m2_um:kd_m1,
    ~ case_when(
      . > 0 ~ .,
      TRUE ~ NA_real_
    )
  ))

# %% ---- Remove duplicates

irradiance |>
  drop_na(eu_w_m2_um) |>
  ggplot(aes(x = wavelength, y = eu_w_m2_um, group = station)) +
  geom_line()

duplicated <- irradiance |>
  janitor::get_dupes(-station)

duplicated

irradiance <- irradiance |>
  anti_join(duplicated)

# %% Visualize the data

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

df_viz <- irradiance |>
  inner_join(stations, by = "station")

p_eu <- ggspectral(df_viz, eu_w_m2_um, "E[u](0^'-')~'['~W~m^{-2}~um^{-1}~']'")
p_ed <- ggspectral(df_viz, ed_w_m2_um, "E[d](0^'-')~'['~W~m^{-2}~um^{-1}~']'")
p_ku <- ggspectral(df_viz, ku_m1, "K[u]~(m^{-1})")
p_kd <- ggspectral(df_viz, kd_m1, "K[d]~(m^{-1})")

save_fun(p_eu, here("graphs", "005_eu_spectral_profiles_by_area.pdf"))
save_fun(p_ed, here("graphs", "005_ed_spectral_profiles_by_area.pdf"))
save_fun(p_ku, here("graphs", "005_ku_spectral_profiles_by_area.pdf"))
save_fun(p_kd, here("graphs", "005_kd_spectral_profiles_by_area.pdf"))
# %%

# %% ---- Save the data
irradiance |>
  semi_join(stations, by = "station") |>
  write_csv(here("data", "clean", "irradiance.csv"))
# %%
