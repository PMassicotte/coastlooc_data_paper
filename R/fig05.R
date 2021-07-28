# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the particle scattering coefficients.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

stations <- read_csv(here("data","clean","stations.csv"))
ac9 <- read_csv(here("data","clean","ac9_negative_values_removed.csv"))

df <- inner_join(stations, ac9, by = "station")

df

# Only 2 observations in the Adriatic Sea?
df %>%
  filter(wavelength == 440) %>%
  drop_na(bp) %>%
  count(area)

p1 <- df %>%
  filter(wavelength == 440) %>%
  drop_na(bp) %>%
  mutate(area = fct_reorder(area, bp)) %>%
  ggplot(aes(x = area, y = bp, color = area)) +
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
    y = quote(italic(b)[p](440)(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

# Boxplot of absorption at 443 nm -----------------------------------------

absorption <- read_csv("data/clean/absorption.csv") %>%
  filter(wavelength == 443) %>%
  inner_join(stations, by = "station")

absorption

# p2 <- absorption %>%
#   drop_na(a_phy_specific) %>%
#   mutate(area = fct_reorder(area, a_phy_specific)) %>%
#   ggplot(aes(x = area, y = a_phy_specific, color = area)) +
#   geom_boxplot(size = 0.25, outlier.size = 1) +
#   ggbeeswarm::geom_quasirandom(size = 0.5, groupOnX = TRUE) +
#   scale_color_manual(
#     breaks = area_breaks,
#     values = area_colors,
#   ) +
#   scale_y_log10() +
#   scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
#   annotation_logticks(sides = "l", size = 0.1) +
#   labs(
#     x = NULL,
#     y = quote(italic(a)[phi]^"*"*(443)~(m^2~mg^{-1}))
#   ) +
#   theme(
#     legend.position = "none"
#   )

# Combine plots -----------------------------------------------------------

# p <- p1 / p2 +
#   plot_layout(heights = c(0.75, 1)) +
#   plot_annotation(tag_levels = "A") &
#   theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs", "fig05.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 3
)
