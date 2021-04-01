# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Compare absorption measured by spectrophotometer and the AC9.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R/zzz.R"))

spectro <- vroom::vroom(here("data/clean/absorption.csv"))
ac9 <- vroom::vroom(here("data/clean/ac9.csv"))
station <- vroom::vroom(here("data/clean/stations.csv"))

df <- spectro %>%
  inner_join(ac9, by = c("station", "wavelength")) %>%
  inner_join(station, by = "station")

df

df %>%
  distinct(wavelength)

range(df$a_dissolved, na.rm = TRUE)
range(df$a_cdom_measured, na.rm = TRUE)
range(df$a_cdom_modeled, na.rm = TRUE)


# Scatterplot a(ac9) vs a(spectro) ----------------------------------------

p <- df %>%
  filter(a_dissolved > 0) %>%
  ggplot(aes(x = a_cdom_modeled, y = a_dissolved, color = area)) +
  geom_point() +
  scale_x_log10(labels = scales::label_number()) +
  scale_y_log10(labels = scales::label_number()) +
  geom_abline(lty = 2, color = "red") +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      override.aes = list(size = 3)
    )
  ) +
  labs(
    x = quote(a[CDOM]~(m^{-1})),
    y = quote(a[dissolved]~(m^{-1})),
    title = "Comparing absorption (AC9 vs spectrofluorometer)",
    subtitle = "The dashed red line is the 1:1 line."
  ) +
  facet_wrap(~glue("{wavelength} nm"), scales = "free") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
  )

ggsave(
  here("graphs/14_compare_spectrofluorimeter_and_ac9_absorption_scatterplot.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 7
)

# Boxplot -----------------------------------------------------------------

p <- df %>%
  select(station, area, wavelength, a_cdom_modeled, a_dissolved) %>%
  drop_na() %>%
  filter(a_dissolved > 0 & a_cdom_modeled > 0) %>%
  pivot_longer(starts_with("a_")) %>%
  ggplot(aes(x = glue("{wavelength} nm"), y = value, color = name)) +
  # geom_boxplot(size = 0.1, outlier.size = 0.25) +
  ggforce::geom_sina(size = 0.5) +
  scale_y_log10(labels = scales::label_number()) +
  facet_wrap(~glue("{wavelength} nm"), scales = "free") +
  annotation_logticks(sides = "l", size = 0.1) +
  paletteer::scale_color_paletteer_d(
    "yarrr::appletv",
    breaks = c("a_cdom_modeled", "a_dissolved"),
    labels = c(quote(a[CDOM]~(spectro)), a[dissolved]~(AC9)),
    guide = guide_legend(
      override.aes = list(size = 3)
    )
  ) +
  labs(
    x = NULL,
    y = quote("Dissolved absorption"~(m^{-1}))
  ) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(
  here("graphs/14_compare_spectrofluorimeter_and_ac9_absorption_boxplot.pdf"),
  device = cairo_pdf,
  width = 9,
  height = 7
)
