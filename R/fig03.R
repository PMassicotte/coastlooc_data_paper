# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure showing the inland-ocean gradient using some key
# variables such as DOC, and Chla.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

station <- read_csv(here("data","clean","stations.csv")) %>%
  select(station, area)

surface <- read_csv(here("data","clean","surface.csv"))

surface

surface <- surface %>%
  inner_join(station, by = "station")

surface %>%
  ggplot(aes(x = chl_a, y = total_chl_a)) +
  geom_point() +
  facet_wrap(~area)

# Total chla --------------------------------------------------------------

p1 <- surface %>%
  drop_na(total_chl_a) %>%
  mutate(area = fct_reorder(area, total_chl_a)) %>%
  ggplot(aes(x = area, y = total_chl_a, color = area)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(size = 0.5, groupOnX = TRUE) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote("Total chlorophyll-a"~(mg~m^{-3}))
  ) +
  theme(
    legend.position = "none"
  )

p1

# POC ---------------------------------------------------------------------

p2 <- surface %>%
  drop_na(poc_g_m_3) %>%
  filter(poc_g_m_3 >= 0.01) %>%
  mutate(area = fct_reorder(area, poc_g_m_3)) %>%
  ggplot(aes(x = area, y = poc_g_m_3, color = area)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(size = 0.5, groupOnX = TRUE) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote("Particulate organic carbon"~(g~m^{-3}))
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

file <- here("graphs","fig03.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)

# Stats for the paper -----------------------------------------------------

surface %>%
  group_by(area) %>%
  summarise(across(c(total_chl_a, poc_g_m_3), median, na.rm = TRUE)) %>%
  arrange(total_chl_a)

## Trophic status ----

# Antoine, David; André, Jean-Michel; Morel, André (1996). Oceanic primary
# production: 2. Estimation at global scale from satellite (Coastal Zone Color
# Scanner) chlorophyll. Global Biogeochemical Cycles, 10(1), 57–69.
# doi:10.1029/95gb02832

surface %>%
  drop_na(total_chl_a) %>%
  mutate(zone = case_when(
    total_chl_a <= 0.1 ~ "oligotrophic",
    between(total_chl_a, 0.1, 1) ~ "mesotrophic",
    total_chl_a > 1 ~ "eutrophic"
  )) %>%
  count(zone)

## POC and distance to shore ----

# Baltic sea has the highest POC and Med. Sea (case 2) the lowest POC. Check
# what is the median distance of the sampled station to the nearest shoreline.

distances <- read_csv(here("data","clean","distances_to_shore.csv")) %>%
  inner_join(station, by = "station")

distances

# Only few km difference. I do not think that distance is a factor. Maybe some
# large tributaries draining the Baltic watershed?

distances %>%
  group_by(area) %>%
  summarise(across(distance_to_shore_m, .fns = list(mean = mean, median = median)))

## Range of presented variables ----

range(surface$total_chl_a, na.rm = TRUE)
range(surface$poc_g_m_3, na.rm = TRUE)

surface %>%
  ggplot(aes(x = total_chl_a, y = poc_g_m_3)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

surface %>%
  select(total_chl_a, poc_g_m_3) %>%
  mutate(across(everything(), log10)) %>%
  correlate()
