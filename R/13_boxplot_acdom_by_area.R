
source(here("R", "zzz.R"))

station <- read_csv(here("data", "clean", "stations.csv"))

# acdom -------------------------------------------------------------------

acdom <- read_csv(here("data","clean","absorption.csv")) %>%
  left_join(station, ., by = "station")

p <- acdom %>%
  filter(wavelength == 443) %>%
  drop_na(a_cdom_modeled) %>%
  # mutate(area = str_wrap(area, 12)) %>%
  mutate(area = fct_reorder(area, a_cdom_modeled, .fun = mean)) %>%
  ggplot(aes(x = area, y = a_cdom_modeled, fill = area)) +
  geom_boxplot(size = 0.25, outlier.size = 1) +
  labs(
    x = NULL,
    y = quote(a[CDOM](443)~"["~m^{-1}~"]")
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  theme(
    legend.position = "none"
  )

file <- here("graphs","13_boxplot_acdom443_by_area.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 6.91,
  height = 5.2
)

# aphy --------------------------------------------------------------------

aphy <- read_csv(here("data","clean","absorption.csv")) %>%
  left_join(station, ., by = "station")

aphy %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = a_phy, y = a_nap, color = area)) +
  geom_point(size = 0.5) +
  facet_wrap(~area, scales = "free") +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.25, color = "gray50") +
  geom_smooth(method = "lm", size = 0.25) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote(a[phi](443)~"["~m^{-1}~"]"),
    y = quote(a[NAP](443)~"["~m^{-1}~"]")
  ) +
  theme(
    legend.position = "none"
  )

file <- here("graphs","13_aphy_vs_anap.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 6.91,
  height = 5.2
)
