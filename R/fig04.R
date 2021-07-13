# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Average absorption per area.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

station <- read_csv(here("data","clean","stations.csv"))

absorption <- vroom::vroom(here("data","clean","absorption.csv")) %>%
  filter(wavelength >= 350) %>%
  left_join(station, ., by = "station") %>%
  group_by(area, wavelength) %>%
  summarise(across(starts_with("a_"), ~ mean(., na.rm = TRUE))) %>%
  ungroup()

df_viz <- absorption %>%
  group_by(area, wavelength) %>%
  summarise(across(starts_with("a_"), ~ mean(., na.rm = TRUE))) %>%
  ungroup()

df_viz

# Plot --------------------------------------------------------------------

p1 <- df_viz %>%
  ggplot(aes(x = wavelength, y = a_phy, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[phi]~(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

p2 <- df_viz %>%
  ggplot(aes(x = wavelength, y = a_nap, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[NAP]~(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

p3 <- df_viz %>%
  ggplot(aes(x = wavelength, y = a_p, color = area)) +
  geom_line() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[P]~(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

p4 <- absorption %>%
  ggplot(aes(x = wavelength, y = a_cdom_modeled, color = area)) +
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
    y = quote(a[CDOM]~(m^{-1}))
  ) +
  theme(
    legend.justification = c(1, 1),
    legend.position = c(0.9, 0.9),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, "cm")
  )

p <- wrap_plots(p3, p2, p1, p4, ncol = 2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold")
  )

file <- here("graphs","fig04.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 8,
  height = 6
)


# Stats for the paper -----------------------------------------------------

## Range of acdom(350) ----

acdom <- vroom::vroom("data/clean/absorption.csv") %>%
  filter(wavelength == 350) %>%
  inner_join(stations)

acdom %>%
  pull(a_cdom_measured) %>%
  range()

