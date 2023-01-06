# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Average absorption per area.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

station <- read_csv(here("data", "clean", "stations.csv"))

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  filter(wavelength >= 350) |>
  left_join(station, ., by = "station") |>
  group_by(area, wavelength) |>
  summarise(across(starts_with("a_"), ~ mean(., na.rm = TRUE))) |>
  ungroup()

df_viz <- absorption |>
  group_by(area, wavelength) |>
  summarise(across(starts_with("a_"), ~ mean(., na.rm = TRUE))) |>
  ungroup()

df_viz

# Plot --------------------------------------------------------------------

p1 <- df_viz |>
  ggplot(aes(x = wavelength, y = a_phy_m1, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = parse(text = "a[phi] ~ (m^{-1})")
  ) +
  theme(
    legend.position = "none"
  )

p2 <- df_viz |>
  ggplot(aes(x = wavelength, y = a_nap_m1, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = parse(text = "a[NAP] ~ (m^{-1})")
  ) +
  theme(
    legend.position = "none"
  )

p3 <- df_viz |>
  ggplot(aes(x = wavelength, y = a_p_m1, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = parse(text = "a[P] ~ (m^{-1})")
  ) +
  theme(
    legend.position = "none"
  )

p4 <- absorption |>
  ggplot(aes(x = wavelength, y = a_cdom_adjusted_m1, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      label.theme = element_text(
        size = 8,
        family = "Montserrat"
      ),
      override.aes = list(size = 1)
    )
  ) +
  labs(
    x = "Wavelength (nm)",
    y = parse(text = "a[CDOM] ~ (m^{-1})")
  ) +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.9, 0.9),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, "cm")
  )

# aCDOM gradient in the North Sea -----------------------------------------

absorption <- read_csv(here("data", "clean", "absorption.csv"))
stations <- read_csv(here("data", "clean", "stations.csv"))

df <- absorption |>
  filter(wavelength == 350) |>
  drop_na(a_cdom_adjusted_m1) |>
  inner_join(stations, by = "station")

df

df_north_sea <- df |>
  filter(area == "North Sea") |>
  filter(str_starts(station, "C1"))

# Visualize the C1 transect
df_north_sea |>
  ggplot(aes(x = longitude, y = latitude, label = station)) +
  geom_point() +
  geom_text(aes(label = station), hjust = "inward", size = 4) +
  coord_sf()

# What is the distance between the two most distanced points?
df_north_sea |>
  filter(station %in% c("C1001000", "C1008000")) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_distance()

p5 <- df_north_sea |>
  ggplot(aes(x = latitude, y = a_cdom_adjusted_m1)) +
  geom_line(color = "#3366CCFF") +
  geom_point(size = 3, color = "#3366CCFF") +
  geom_text(
    aes(label = station),
    check_overlap = TRUE,
    size = 2,
    nudge_y = -0.1,
    nudge_x = -0.005
  ) +
  scale_x_continuous(labels = ~ paste0(., "\u00b0")) +
  annotate("text",
    x = 52.205,
    y = 1.53,
    label = "Nearshore",
    size = 3
  ) +
  annotate(
    geom = "curve",
    x = 52.205,
    y = 1.58,
    xend = 52.187,
    yend = 1.6,
    curvature = 0.3,
    arrow = ggplot2::arrow(length = unit(2, "mm")),
    size = 0.2
  ) +
  annotate("text",
    x = 52.343,
    y = 0.47,
    label = "Open water",
    size = 3
  ) +
  annotate(
    geom = "curve",
    x = 52.355,
    y = 0.47,
    xend = 52.366,
    yend = 0.37,
    curvature = -0.4,
    arrow = ggplot2::arrow(length = unit(2, "mm")),
    size = 0.2
  ) +
  labs(
    x = "Latitude",
    y = parse(text = "a[CDOM] ~ (350) ~ (m^{-1})")
  )

# Combine plots -----------------------------------------------------------

p <- wrap_plots(p3, p2, p1, p4, ncol = 2) / p5 +
  plot_layout(heights = c(1.25, 0.75)) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold")
  )

ggsave(
  here("graphs", "fig05.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 220,
  units = "mm"
)

# Stats for the paper -----------------------------------------------------

## Range of sCDOM on the averaged spectra per area ----

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  filter(wavelength == 350) |>
  inner_join(station)

absorption |>
  summarise(across(starts_with("a_cdom"), list(
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  ))) |>
  pivot_longer(everything())
