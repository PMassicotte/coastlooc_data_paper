# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Relationships between DOC and aCDOM at different wavelengths.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

doc <- read_csv(here("data","clean","surface.csv")) %>%
  select(station, doc_um) %>%
  drop_na()

acdom <- vroom::vroom(here("data","clean","absorption.csv"))

stations <- read_csv(here("data","clean","stations.csv")) %>%
  select(station, area)

df <- doc %>%
  inner_join(acdom, by = "station") %>%
  inner_join(stations, by = "station")

df %>%
  filter(wavelength == 355) %>%
  drop_na(doc_um, a_cdom_measured) %>%
  ggplot(aes(x = doc_um, y = a_cdom_measured)) +
  geom_point(aes(color = area)) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl") +
  geom_smooth(method = "lm")

df %>%
  drop_na(doc_um, a_cdom_measured) %>%
  filter(wavelength %in% seq(350, 600, by = 10)) %>%
  filter(a_cdom_measured > 0) %>%
  ggplot(aes(x = doc_um, y = a_cdom_measured)) +
  geom_point(aes(color = area)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm") +
  annotation_logticks(sides = "bl") +
  facet_wrap(~wavelength, scales = "free")

df_lm <- df %>%
  drop_na(doc_um, a_cdom_measured) %>%
  filter(wavelength <= 500) %>%
  filter(a_cdom_measured > 0) %>%
  group_nest(wavelength) %>%
  mutate(n = map_int(data, nrow)) %>%
  filter(n >= 50) %>%
  mutate(model = map(data, ~lm(log10(a_cdom_measured) ~ log10(doc_um), data = .))) %>%
  mutate(r2 = map_dbl(model, ~summary(.)["r.squared"][[1]]))

# R2 as a function of wavelength ------------------------------------------

p <- df_lm %>%
  ggplot(aes(x = wavelength, y = r2)) +
  geom_point(color = "#3c3c3c") +
  scale_y_continuous(limits = c(0, NA)) +
  geom_smooth(method = "gam", size = 0.5, color = "#D7263D") +
  labs(
    x = "Wavelength (nm)",
    y = quote("Determination coefficient" ~ (R^2))
  )

ggsave(
  here("graphs/fig07.pdf"),
  device = cairo_pdf,
  width = 5,
  height = 4
)

# Confidence intervals of estimated slopes --------------------------------

df_lm %>%
  mutate(tidied = map(model, ~broom::tidy(., conf.int = TRUE))) %>%
  unnest(tidied) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = wavelength, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5)
