# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Irradiance spectra by area.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

irradiance <- read_csv(here("data", "clean", "irradiance.csv"))

irradiance

irradiance <- irradiance |>
  drop_na(ed_w_m2_um) |>
  inner_join(stations, by = "station")

# There are NAs at some wavelengths due to the usage of different devices. For
# visualization, I will remove mean values that were computed with a limited
# number of observations.

irradiance_mean <- irradiance |>
  group_by(area, wavelength) |>
  summarise(ed_w_m2_um = mean(ed_w_m2_um, na.rm = TRUE), n = n()) |>
  ungroup() |>
  filter(n >= 10)

irradiance |>
  ggplot(aes(
    x = wavelength,
    y = ed_w_m2_um,
    color = area,
    group = station
  )) +
  geom_line(size = 0.1, alpha = 0.5) +
  geom_line(
    data = irradiance_mean,
    aes(x = wavelength, y = ed_w_m2_um, color = area),
    size = 1.25,
    inherit.aes = FALSE
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (mm)",
    y = quote(E[d](0^"-") ~ "[" ~ W ~ m^{
      -2
    } ~ mu * m^{
      -1
    } ~ "]")
  ) +
  facet_wrap(~area) +
  theme(legend.position = "none")

ggsave(
  here("graphs", "fig06.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 140,
  units = "mm"
)
