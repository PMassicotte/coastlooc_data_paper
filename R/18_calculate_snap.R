# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate the slope of the anap spectra (s_nap) following the
# procedure in: Babin, Variations in the Light Absorption Coefficients of
# Phytoplankton, Nonalgal Particles, and Dissolved Organic Matter in Coastal
# Waters around Europe.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

anap <- vroom::vroom(here("data","clean","absorption.csv")) %>%
  select(station, wavelength, a_nap)

anap

anap <- anap %>%
  filter(between(wavelength, 380, 730)) %>%
  filter(!between(wavelength, 400, 480) & !between(wavelength, 620, 710)) %>%
  drop_na()

anap %>%
  distinct(station)

anap %>%
  ggplot(aes(x = wavelength, y = a_nap, group = station)) +
  geom_point(size = 0.25)

# Fit the exponential models ----------------------------------------------

anap

df <- anap %>%
  group_nest(station) %>%
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      a_nap ~ a443 * exp(-s * (wavelength - 500)),
      data = .,
      start = c(a443 = 0.2, s = 0.03),
      lower =  c(a443 = 0.001, s = 0.001)
    )
  ))

df

df <- df %>%
  mutate(tidied = map(mod, broom::tidy)) %>%
  mutate(augmented = map(mod, broom::augment))

# Visualize some fits -----------------------------------------------------

df %>%
  slice_sample(n = 49) %>%
  select(station, augmented) %>%
  unnest(augmented) %>%
  ggplot(aes(x = wavelength, y = a_nap)) +
  geom_point() +
  geom_line(aes(y = .fitted), color = "red") +
  facet_wrap(~station, scales = "free_y")

# Export s_nap ------------------------------------------------------------

res <- df %>%
  select(station, tidied) %>%
  unnest(tidied) %>%
  filter(term == "s") %>%
  select(station, s_nap = estimate)

res %>%
  ggplot(aes(x = s_nap)) +
  geom_histogram()

write_csv(res, here("data","clean","s_nap.csv"))
