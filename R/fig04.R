# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Average absorption per area.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

station <- read_csv(here("data","clean","stations.csv"))

absorption <- read_csv(here("data","clean","absorption.csv")) %>%
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
  ggplot(aes(x = wavelength, y = a_cdom_measured, color = area)) +
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

ggsave(
  here("graphs","fig04.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)

# Stats for the paper -----------------------------------------------------

## Range of sCDOM on the averaged spectra per area ----

df_viz

range(df_viz$wavelength, na.rm = TRUE)

df_scdom <- df_viz %>%
  select(area, wavelength, a_cdom_modeled) %>%
  filter(between(wavelength, 350, 700)) %>%
  group_nest(area) %>%
  mutate(model = map(data, ~ nls(
    a_cdom_modeled ~ a0 * exp(-s * (wavelength - 400)) + k,
    data = .,
    start = list(a0 = 0.5, s = 0.02, k = 0)
  ))) %>%
  mutate(tidied = map(model, broom::tidy))

df_scdom

df_scdom %>%
  unnest(tidied)

df_scdom %>%
  mutate(pred = map(model, broom::augment)) %>%
  unnest(pred) %>%
  ggplot(aes(x = wavelength, y = a_cdom_modeled, color = area)) +
  geom_point() +
  geom_line(aes(y = .fitted)) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  )

df_scdom %>%
  unnest(tidied) %>%
  filter(term == "s") %>%
  mutate(area = fct_reorder(area, estimate)) %>%
  ggplot(aes(x = estimate, y = area)) +
  geom_col()

## Range of acdom(350) ----

absorption <- read_csv(here("data","clean","absorption.csv")) %>%
  filter(wavelength == 350) %>%
  inner_join(station)

absorption %>%
  summarise(across(starts_with("a_cdom"), list(
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(everything())
