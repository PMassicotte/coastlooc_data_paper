# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Ternary plot of absorption.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

absorption <- data.table::fread(here("data", "clean", "absorption.csv")) %>%
  as_tibble() %>%
  select(station, wavelength, a_nap, a_phy, a_cdom_modeled)

stations <- data.table::fread(here("data", "clean", "stations.csv")) %>%
  select(station, area)

absorption <- absorption %>%
  inner_join(stations, by = "station")

absorption

df <- absorption %>%
  filter(wavelength == 443) %>%
  drop_na(starts_with("a_")) %>%
  mutate(a_tot = a_nap + a_phy + a_cdom_modeled, .after = a_cdom_modeled) %>%
  mutate(across(c(a_nap, a_phy, a_cdom_modeled), ~ . / a_tot))

# Plot --------------------------------------------------------------------

p <- ggtern(df, aes(x = a_nap, y = a_phy, z = a_cdom_modeled, color = area)) +
  geom_point() +
  xlab(parse(text = "a[NAP](443)")) +
  ylab(parse(text = "a[phi](443)")) +
  zlab(expression("a[CDOM](443)")) +
  scale_L_continuous(breaks = seq(0, 1, by = 0.10)) +
  scale_R_continuous(breaks = seq(0, 1, by = 0.10)) +
  scale_T_continuous(breaks = seq(0, 1, by = 0.10)) +
  # weight_percent() +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85, 0.6)
  )

filename <- here("graphs", "19_ternary_plot_absorption.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 8,
  height = 8
)

knitr::plot_crop(filename)
