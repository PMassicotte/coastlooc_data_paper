# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Overview of the temporal sampling.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

stations <- read_csv(here("data","clean","stations.csv"))

stations

stations <- stations %>%
  mutate(
    date_month = clock::date_group(date, precision = "month"),
    .after = date
  ) %>%
  count(area, date_month) %>%
  assertr::verify(sum(n) == 424)

stations

# Plot --------------------------------------------------------------------

df_line <- stations %>%
  group_by(area) %>%
  summarise(across(date_month, .fns = list("min" = min, "max" = max)))

p <- stations %>%
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
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_size(range = c(4, 10)) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%Y-%b"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_blank()
  )

ggsave(
  here("graphs","fig02.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 5
)
