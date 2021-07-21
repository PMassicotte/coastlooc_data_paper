# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figures on AOPs.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

irradiance <- read_csv(here("data","clean","irradiance_negative_values_removed.csv"))
stations <- read_csv(here("data","clean","stations.csv"))
surface <- read_csv(here("data","clean","surface.csv"))

df <- irradiance %>%
  inner_join(stations, by = "station") %>%
  inner_join(surface, by = "station")


# Plots -------------------------------------------------------------------

p1 <- df %>%
  filter(wavelength == 443) %>%
  drop_na(ed_wm2_nm1) %>%
  mutate(area = fct_reorder(area, ed_wm2_nm1)) %>%
  ggplot(aes(x = area, y = ed_wm2_nm1, color = area)) +
  geom_boxplot(size = 0.25, outlier.size = 1) +
  ggbeeswarm::geom_quasirandom(size = 0.5, groupOnX = TRUE) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(italic(E)[d](443)~(wm^{-2}))
  ) +
  theme(
    legend.position = "none"
  )

p2 <- df %>%
  filter(wavelength == 443) %>%
  drop_na(kd_m1) %>%
  mutate(area = fct_reorder(area, kd_m1)) %>%
  ggplot(aes(x = area, y = kd_m1, color = area)) +
  geom_boxplot(size = 0.25, outlier.size = 1) +
  ggbeeswarm::geom_quasirandom(size = 0.5, groupOnX = TRUE) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(italic(K)[d](443)~(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs/fig06.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 6
)
