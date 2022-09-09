# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show bivariate relationships for various variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

poc <- read_csv(here("data", "clean", "surface.csv")) |>
  select(station, total_chl_a, poc_g_m3)

df <- inner_join(stations, poc, by = "station")

# Chla vs poc -------------------------------------------------------------

p1 <- df |>
  drop_na() |>
  ggplot(aes(x = total_chl_a, y = poc_g_m3)) +
  geom_point(
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      label.theme = element_text(
        size = 6,
        family = "Montserrat"
      ),
      override.aes = list(size = 2)
    )
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c") +
  labs(
    x = quote("Total chlorophyll-a" ~ (mg ~ m^{
      -3
    })),
    y = quote("Particulate organic carbon" ~ (g ~ m^{
      -3
    }))
  ) +
  ggpmisc::stat_correlation(
    label.y = 0.12,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_correlation(
    aes(label = ..n.label..),
    label.y = 0.05,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.02, 0.99),
    legend.key.size = unit(0.4, "cm"),
    legend.background = element_blank()
  )

# Find out interesting correlations to show -------------------------------

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

surface <- read_csv(here("data", "clean", "surface.csv"))

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  filter(wavelength == 443)

df <- surface |>
  inner_join(stations, by = "station") |>
  inner_join(absorption, by = "station")

df

# Because CDOM and SPM do not necessary co-variate with chlorophyll-a and can
# mask the signal from the phytoplankton (Sathyendranath2000).

# Total chla vs aphy ------------------------------------------------------

p2 <- df |>
  ggplot(aes(x = total_chl_a, y = a_phy_m1)) +
  geom_point(
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c") +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote("Total chlorophyll-a" ~ (mg ~ m^{
      -3
    })),
    y = quote(a[phi](443) ~ (m^{
      -1
    }))
  ) +
  ggpmisc::stat_correlation(
    label.y = 0.12,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_correlation(
    aes(label = ..n.label..),
    label.y = 0.05,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  theme(
    legend.position = "none"
  )

# POC vs Kd ---------------------------------------------------------------

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

poc <- read_csv(here("data", "clean", "surface.csv")) |>
  select(station, total_chl_a, poc_g_m3)

irradiance <- read_csv(here("data", "clean", "irradiance.csv"))

df <- stations |>
  inner_join(poc, by = "station") |>
  inner_join(irradiance, by = "station") |>
  filter(wavelength == 443)

p3 <- df |>
  ggplot(aes(x = kd_m1, y = poc_g_m3)) +
  geom_point(
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c") +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote("Particulate organic carbon" ~ (g ~ m^{
      -3
    })),
    y = quote(K[d](443) ~ (m^{
      -1
    }))
  ) +
  ggpmisc::stat_correlation(
    label.y = 0.12,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_correlation(
    aes(label = ..n.label..),
    label.y = 0.05,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  theme(
    legend.position = "none"
  )

# POC vs bp ---------------------------------------------------------------

stations <- read_csv(here("data", "clean", "stations.csv"))
ac9 <- read_csv(here("data", "clean", "ac9.csv")) |>
  filter(wavelength == 440)

surface <- read_csv(here("data", "clean", "surface.csv"))

df <- inner_join(stations, ac9, by = "station") |>
  inner_join(surface, by = "station") |>
  drop_na(poc_g_m3, bp_m1)

unique(df$wavelength)

p4 <- df |>
  ggplot(aes(x = poc_g_m3, y = bp_m1)) +
  geom_point(
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#3c3c3c") +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      override.aes = list(alpha = 1, stroke = 1, size = 3)
    )
  ) +
  labs(
    x = quote("Particulate organic carbon" ~ (g ~ m^{
      -3
    })),
    y = quote(italic(b)[p](440) ~ (m^{
      -1
    }))
  ) +
  ggpmisc::stat_correlation(
    label.y = 0.12,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_correlation(
    aes(label = ..n.label..),
    label.y = 0.05,
    label.x = 0.97,
    size = 2.5,
    family = "Montserrat"
  ) +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- wrap_plots(p1, p2, p3, p4, ncol = 2) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", margin = margin(r = 5)))

ggsave(
  here("graphs", "fig07.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)
