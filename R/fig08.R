# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show couple of vertical profiles of Ed and Eu (SPMR).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- read_csv(here("data", "clean", "stations.csv")) |>
  select(-depth_m)

df <- open_dataset(here("data", "clean", "spmr", "parquet")) |>
  left_join(metadata, by = "station") |>
  collect()

df

df |>
  count(station, depth_m, wavelength) |>
  assertr::verify(n == 1)

p <- df |>
  drop_na(ed_w_m2_um) |>
  ggplot(aes(
    x = ed_w_m2_um,
    y = depth_m,
    color = factor(wavelength),
    group = interaction(station, wavelength)
  )) +
  geom_path() +
  facet_wrap(~station, scales = "free") +
  theme(
    legend.title = element_blank()
  )

# %% ---- Wavelength concordence
# There are some mismatch across wavelengths for Ed and Eu
df |>
  select(station, wavelength, k_par_m1:kd_m1) |>
  pivot_longer(-c(station, wavelength)) |>
  drop_na() |>
  distinct(name, wavelength) |>
  mutate(available = name) |>
  complete(wavelength, name, fill = list(available = NA)) |>
  ggplot(aes(x = factor(wavelength), y = name, fill = available)) +
  geom_tile(color = "white", size = 2)
# %%

# %% ---- Spectral profiles by depth
df

df |>
  count(depth_m)

p <- df |>
  ggplot(aes(
    x = wavelength,
    y = ed_w_m2_um,
    group = depth_m,
    color = depth_m
  )) +
  geom_line() +
  scale_color_viridis_c() +
  facet_wrap(~station, scales = "free_y")

ggsave(
  here("graphs", "fig08_spectral_profiles.pdf"),
  device = cairo_pdf,
  width = 20,
  height = 10
)
# %%
