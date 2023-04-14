# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Irradiance spectra by area.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

reflectance <- read_csv(here("data", "clean", "reflectance.csv"))

reflectance

reflectance <- reflectance |>
  drop_na(measured_reflectance_percent) |>
  inner_join(stations, by = "station")

# There are NAs at some wavelengths due to the usage of different devices. For
# visualization, I will remove mean values that were computed with a limited
# number of observations.

irradiance_mean <- reflectance |>
  group_by(area, wavelength) |>
  summarise(measured_reflectance_percent = mean(measured_reflectance_percent, na.rm = TRUE), n = n()) |>
  ungroup() |>
  filter(n >= 10)

reflectance |>
  ggplot(aes(
    x = wavelength,
    y = measured_reflectance_percent,
    color = area,
    group = station
  )) +
  geom_line(linewidth = 0.1, alpha = 0.5) +
  geom_line(
    data = irradiance_mean,
    aes(x = wavelength, y = measured_reflectance_percent, color = area),
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (mm)",
    y = parse(text = "R(0^'-')")
  ) +
  facet_wrap(~area, scales = "free_y") +
  theme(legend.position = "none")

ggsave(
  here("graphs", "fig07.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)
