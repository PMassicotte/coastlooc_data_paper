# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the particle scattering coefficients.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

# Particulate scattering --------------------------------------------------

stations <- read_csv(here("data", "clean", "stations.csv"))
ac9 <- read_csv(here("data", "clean", "ac9_negative_values_removed.csv"))
ac9 <- inner_join(stations, ac9, by = "station")

ac9

# Only 2 observations in the Adriatic Sea?
ac9 |>
  filter(wavelength == 440) |>
  drop_na(bp) |>
  count(area)

p1 <- ac9 |>
  filter(wavelength == 440) |>
  drop_na(bp) |>
  mutate(area = fct_reorder(area, bp)) |>
  ggplot(aes(x = area, y = bp, color = area)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = TRUE,
    aes(fill = area),
    size = 1,
    stroke = 0.5,
    pch = 21,
    alpha = 0.5
  ) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(italic(b)[p](440)(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

# Downward attenuation coefficient ----------------------------------------

irradiance <- read_csv(here("data", "clean", "irradiance_negative_values_removed.csv"))
stations <- read_csv(here("data", "clean", "stations.csv"))
surface <- read_csv(here("data", "clean", "surface.csv"))

irradiance <- irradiance |>
  inner_join(stations, by = "station") |>
  inner_join(surface, by = "station")

p2 <- irradiance |>
  filter(wavelength == 443) |>
  drop_na(kd_m1) |>
  mutate(area = fct_reorder(area, kd_m1)) |>
  ggplot(aes(x = area, y = kd_m1, color = area)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = TRUE,
    aes(fill = area),
    size = 1,
    stroke = 0.5,
    pch = 21,
    alpha = 0.5
  ) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(italic(K)[d](443) ~ (m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs", "fig05.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 220,
  units = "mm"
)

# Stats for the paper -----------------------------------------------------

ac9 |>
  filter(wavelength == 440) |>
  pull(bp) |>
  range(na.rm = TRUE)

ac9 |>
  filter(wavelength == 440) |>
  group_by(area) |>
  summarise(median_bp = median(bp, na.rm = TRUE)) |>
  arrange(median_bp)

irradiance |>
  filter(wavelength == 443) |>
  group_by(area) |>
  summarise(median_kd = median(kd_m1, na.rm = TRUE)) |>
  arrange(median_kd)
