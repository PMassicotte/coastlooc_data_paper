# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Overview of the temporal sampling.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

stations <- read_csv(here("data", "clean", "stations.csv"))
bathymetry <- read_csv(here("data", "clean", "bathymetry.csv"))

bathymetry |>
  filter(is.na(bathymetry_m))

df <- stations |>
  left_join(bathymetry, by = "station") |>
  mutate(bathymetry_m = -bathymetry_m) |>
  mutate(area = fct_reorder(
    area,
    bathymetry_m,
    .fun = mean,
    na.rm = TRUE,
    .desc = TRUE
  ))

df |>
  group_by(area) |>
  summarise(mean(bathymetry_m, na.rm = TRUE))

# Plot number of observation per area -------------------------------------

df_viz <- df |>
  mutate(
    date_month = clock::date_group(date, precision = "month"),
    .after = date
  ) |>
  mutate(cruise = str_sub(station, 1, 2)) |>
  count(area, cruise, date_month)

df_viz

df_line <- df_viz |>
  group_by(area) |>
  summarise(across(date_month, .fns = list("min" = min, "max" = max)))

p1 <- df_viz |>
  ggplot(aes(x = date_month, y = area)) +
  geom_segment(
    data = df_line,
    aes(
      x = date_month_min,
      y = area,
      xend = date_month_max,
      yend = area,
      color = area
    )
  ) +
  geom_point(aes(size = n, color = area)) +
  geom_text(aes(label = n), color = "white", size = 3) +
  geom_text(aes(label = cruise, color = area), size = 2.5, vjust = -2.2) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_size(range = c(4, 10)) +
  scale_x_date(
    date_breaks = "3 months",
    # date_labels = "%Y-%b",
    labels = scales::label_date_short()
  ) +
  labs(
    x = "Sampling date",
    y = NULL
  ) +
  theme(
    legend.position = "none"
  )

# Plot average bathymetry per area ----------------------------------------

p2 <- df |>
  drop_na(bathymetry_m) |>
  ggplot(aes(x = bathymetry_m, y = area, color = area)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(
    aes(fill = area),
    size = 1,
    groupOnX = FALSE,
    alpha = 0.5,
    stroke = 0.1,
    pch = 21
  ) +
  scale_x_log10() +
  annotation_logticks(sides = "b", size = 0.) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Bathymetry (m)",
    y = NULL
  ) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Combine and save plots --------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 2, widths = c(1, 0.5)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(face = "bold")
  )

ggsave(
  here("graphs", "fig02.pdf"),
  device = cairo_pdf,
  width = 200,
  height = 120,
  units = "mm"
)
