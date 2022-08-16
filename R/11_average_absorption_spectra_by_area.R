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
    y = quote(a[phi] ~ (m^{-1}))
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
    y = quote(a[NAP] ~ (m^{-1}))
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
    y = quote(a[P] ~ (m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

p4 <- absorption |>
  ggplot(aes(x = wavelength, y = a_cdom_modeled_m1, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      label.theme = element_text(
        size = 6,
        family = "Montserrat Alternates"
      ),
      override.aes = list(size = 1)
    )
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM] ~ (m^{-1}))
  ) +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.9, 0.9),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, "cm")
  )

p <- wrap_plots(p1, p2, p3, p4, ncol = 2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold")
  )

file <- here("graphs", "11_average_absorption_spectra_by_area.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 8,
  height = 6
)
