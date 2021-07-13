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
  ggbeeswarm::geom_quasirandom(size = 1, groupOnX = TRUE) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote("Particles scattering coefficient"~(italic(b)[p](440)~","~ m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

# bp VS POC ---------------------------------------------------------------

surface <- read_csv(here("data","clean","surface.csv"))

unique(df$wavelength)

p2 <- df %>%
  inner_join(surface, by = "station") %>%
  drop_na(poc_g_m_3, bp) %>%
  filter(wavelength %in% c(440, 510, 555, 715)) %>%
  ggplot(aes(x = poc_g_m_3, y = bp)) +
  geom_point(aes(color = area), size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      override.aes = list(alpha = 1, stroke = 1, size = 3)
    )
  ) +
  geom_smooth(method = "lm", size = 0.5, color = "#D7263D") +
  labs(
    x = quote("Particulate organic carbon" ~ (g~m^{-3})),
    y = quote("Particles scattering coefficient"~(italic(b)[p](lambda)~","~ m^{-1}))
  ) +
  facet_wrap(~glue("{wavelength} nm")) +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 16))

ggsave(
  here("graphs", "fig06.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 12
)
