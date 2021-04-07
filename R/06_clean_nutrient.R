# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize nutrients and flag/remove outliers if any.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")

station <- read_csv(here("data","clean","stations.csv")) %>%
  select(station, area)

station %>%
  distinct(area)

nutrient <- read_csv(here("data","clean","surface.csv"))

nutrient

nutrient %>%
  anti_join(station, by = "station") %>%
  distinct(station)

nutrient <- nutrient %>%
  inner_join(station, by = "station") %>%
  relocate(area, .after = station)

# There are a lot of nutrient parameters that have values of zero. Are they true
# zero or indicate missing values?

nutrient %>%
  pivot_longer(-c(station, area)) %>%
  drop_na() %>%
  filter(value != 0) %>%
  add_count(name) %>%
  mutate(name = glue("{name} (n = {n})")) %>%
  mutate(name = fct_reorder(name, n)) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free")

# Test for negative values ------------------------------------------------

nutrient <- nutrient %>%
  assert(
    within_bounds(0, Inf),
    c(
      contains("chl"),
      contains("xanthin"),
      poc_g_m_3,
      spm:poc_g_m_3,
      al:micro,
      doc_um
    )
  )

# Number of observation per variable --------------------------------------

names(nutrient)

p <- nutrient %>%
  select(
    station,
    area,
    contains("chl"),
    contains("xanthin"),
    poc_g_m_3,
    spm:poc_g_m_3,
    al:micro,
    doc_um
  ) %>%
  pivot_longer(-c(station, area)) %>%
  drop_na() %>%
  filter(value != 0) %>%
  group_by(area, name) %>%
  summarise(n = n()) %>%
  # mutate(name = fct_rev(factor(name))) %>%
  ggplot(aes(x = n, y = name, fill = area)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.5, color = "#3c3c3c", size = 2.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    y = NULL,
    x = "Number of observation",
    title = "Number of available observations per variable",
    subtitle = "These numbers do not count observations with a value of 0."
  ) +
  facet_wrap(~area) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 6),
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )

file <- here("graphs","06_number_observation_nutrient_by_area.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 8,
  height = 10
)
