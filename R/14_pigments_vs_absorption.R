# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationships between nutrients and absorption.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

absorption <- read_csv(here("data", "clean", "absorption.csv"))
pigments <- read_csv(here("data", "clean", "surface.csv"))
stations <- read_csv(here("data", "clean", "stations.csv"))

df <- absorption |>
  inner_join(pigments, by = "station") |>
  inner_join(stations, by = "station")

df

p <- df |>
  filter(wavelength == 443) |>
  ggplot(aes(x = a_phy_m1, y = total_chl_a, color = area)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", show.legend = FALSE, size = 0.25) +
  scale_y_log10() +
  scale_x_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  facet_wrap(~area, scales = "free") +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote(a[phi] ~ ("443 nm") ~ (m^{
      -1
    })),
    y = quote("TChla" ~ (mg ~ m^{
      -3
    }))
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs", "14_total_chla_vs_aphy443.pdf"),
  device = cairo_pdf,
  width = 6.9,
  height = 4.45
)
