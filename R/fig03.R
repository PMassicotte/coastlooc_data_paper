# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure showing the inland-ocean gradient using some key
# variables such as DOC, and Chla.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

station <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

surface <- read_csv(here("data", "clean", "pigments.csv"))

surface

surface <- surface |>
  inner_join(station, by = "station")

# Total chla --------------------------------------------------------------

p1 <- surface |>
  drop_na(chlorophyll_a_mg_m3) |>
  mutate(area = fct_reorder(area, chlorophyll_a_mg_m3)) |>
  ggplot(aes(x = area, y = chlorophyll_a_mg_m3)) +
  geom_boxplot(aes(color = area), size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = TRUE,
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_y_log10() +
  scale_x_discrete(
    labels = function(x) {
      str_wrap(x, 10)
    }
  ) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote("Total chlorophyll-a" ~ (mg ~ m^{
      -3
    }))
  ) +
  theme(legend.position = "none")

p1

# POC ---------------------------------------------------------------------

nutrients <- read_csv(here("data", "clean", "nutrients.csv")) |>
  inner_join(station, by = "station")

p2 <- nutrients |>
  drop_na(particulate_organic_carbon_g_m3) |>
  filter(particulate_organic_carbon_g_m3 >= 0.01) |>
  mutate(area = fct_reorder(area, particulate_organic_carbon_g_m3)) |>
  ggplot(aes(x = area, y = particulate_organic_carbon_g_m3)) +
  geom_boxplot(aes(color = area), size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = TRUE,
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote("Particulate organic carbon" ~ (g ~ m^{
      -3
    }))
  ) +
  theme(
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold", margin = margin(r = 5))
  )

file <- here("graphs", "fig03.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)
