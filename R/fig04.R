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
    y = quote(a[phi] ~ (m^{
      -1
    }))
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
    y = quote(a[NAP] ~ (m^{
      -1
    }))
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
    y = quote(a[P] ~ (m^{
      -1
    }))
  ) +
  theme(
    legend.position = "none"
  )

p4 <- absorption |>
  ggplot(aes(x = wavelength, y = a_cdom_measured_m1, color = area)) +
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
    y = quote(a[CDOM] ~ (m^{
      -1
    }))
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
  drop_na(a_cdom_measured_m1) |>
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
  # drop_na(a_cdom_measured) |>
  ggplot(aes(x = latitude, y = a_cdom_measured_m1)) +
  geom_line(color = "#3366CCFF") +
  geom_point(size = 3, color = "#3366CCFF") +
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
    y = quote(a[CDOM] ~ (350) ~ (m^{
      -1
    }))
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
  here("graphs", "fig04.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 220,
  units = "mm"
)

# Stats for the paper -----------------------------------------------------

## Range of sCDOM on the averaged spectra per area ----

df_viz

range(df_viz$wavelength, na.rm = TRUE)

df_scdom <- df_viz |>
  select(area, wavelength, a_cdom_modeled_m1) |>
  filter(between(wavelength, 350, 700)) |>
  group_nest(area) |>
  mutate(model = map(data, ~ nls(
    a_cdom_modeled_m1 ~ a0 * exp(-s * (wavelength - 400)) + k,
    data = .,
    start = list(a0 = 0.5, s = 0.02, k = 0)
  ))) |>
  mutate(tidied = map(model, broom::tidy))

df_scdom

df_scdom |>
  unnest(tidied)

df_scdom |>
  mutate(pred = map(model, broom::augment)) |>
  unnest(pred) |>
  ggplot(aes(x = wavelength, y = a_cdom_modeled_m1, color = area)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  )

df_scdom |>
  unnest(tidied) |>
  filter(term == "s") |>
  mutate(area = fct_reorder(area, estimate)) |>
  ggplot(aes(x = estimate, y = area)) +
  geom_col()

## Range of acdom(350) ----

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  filter(wavelength == 350) |>
  inner_join(station)

absorption |>
  summarise(across(starts_with("a_cdom"), list(
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  ))) |>
  pivot_longer(everything())
