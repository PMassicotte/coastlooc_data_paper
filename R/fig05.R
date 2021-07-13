# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show bivariate relationships for various variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

stations <- read_csv(here("data","clean","stations.csv")) %>%
  select(station, area)

poc <- read_csv(here("data","clean","surface.csv")) %>%
  select(station, total_chl_a, poc_g_m_3)

df <- inner_join(stations, poc, by = "station")

# Chla vs poc -------------------------------------------------------------

p1 <- df %>%
  filter(poc_g_m_3 >= 0.01) %>%
  drop_na() %>%
  ggplot(aes(x = total_chl_a, y = poc_g_m_3)) +
  geom_point(aes(color = area), size = 1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      label.theme = element_text(
        size = 8,
        family = "Montserrat"
      ),
      override.aes = list(size = 1)
    )
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#D7263D") +
  labs(
    x = quote("Total chlorophyll-a"~(mg~m^{-3})),
    y = quote("Particulate organic carbon"~(g~m^{-3}))
  ) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.02, 0.99),
    legend.key.size = unit(0.4, "cm")
  )

# Find out interesting correlations to show -------------------------------

stations <- read_csv(here("data","clean","stations.csv")) %>%
  select(station, area)

surface <- read_csv(here("data","clean","surface.csv"))

absorption <- vroom::vroom(here("data","clean","absorption.csv")) %>%
  filter(wavelength == 443)

df <- surface %>%
  inner_join(stations, by = "station") %>%
  inner_join(absorption, by = "station")

df

df %>%
  select(where(is.numeric)) %>%
  correlate() %>%
  stretch() %>%
  drop_na() %>%
  arrange(desc(abs(r)))

df %>%
  select(where(is.numeric)) %>%
  correlate() %>%
  stretch() %>%
  drop_na() %>%
  filter(abs(r) >= 0.5) %>%
  retract() %>%
  rplot(print_cor = TRUE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Because CDOM and SPM do not necessary co-variate with chlorophyll-a and can
# mask the signal from the phytoplankton (Sathyendranath2000).

# Total chla vs aphy ------------------------------------------------------

p2 <- df %>%
  ggplot(aes(x = total_chl_a, y = a_phy)) +
  geom_point(aes(color = area), size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.1) +
  geom_smooth(method = "lm", color = "#D7263D") +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = quote("Total chlorophyll-a"~(mg~m^{-3})),
    y = quote(a[phi](443)~(m^{-1}))
  ) +
  theme(
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", margin = margin(r = 5)))

ggsave(
  here("graphs", "fig05.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 7
)
