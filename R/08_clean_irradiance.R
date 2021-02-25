# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Remove outliers in the irradiance data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

irradiance <- read_csv(here("data/clean/irradiance.csv"))

irradiance

# Histogram of irradiance data --------------------------------------------

df_viz <- irradiance %>%
  pivot_longer(-c(station, wavelength))

df_viz %>%
  filter(value < 0) %>%
  count(wavelength, name)

df_viz %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name} ({wavelength} nm)"), scales = "free", ncol = 15) +
  geom_vline(xintercept = 0, color = "red", lty = 2, size = 1) +
  labs(
    title = "Histograms of raw irradiance data",
    subtitle = "There are negative values in this data. We can also see that there are a common value of -95 in the Ku data."
  )

ggsave(
  here("graphs/08_histogram_raw_irradiance.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Remove negative values --------------------------------------------------

irradiance_clean <- irradiance %>%
  mutate(across(
    eu:kd,
    ~ case_when(
      . >= 0 ~ .,
      TRUE ~ NA_real_
    )
  ))

p <- irradiance_clean %>%
  pivot_longer(-c(station, wavelength)) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name} ({wavelength} nm)"), scales = "free", ncol = 15) +
  labs(
    title = "Histograms of irradiance data",
    subtitle = "Negative values were set to NA."
  )

ggsave(
  here("graphs/08_histogram_irradiance_negative_values_removed.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Export clean data -------------------------------------------------------

irradiance_clean %>%
  write_csv(here("data/clean/irradiance.csv"))

# Test --------------------------------------------------------------------

# TODO: Should we keep only station where there are all positive radiometric
# values? Even if a station ends up with only a subset of wavelengths (i.e. not
# all the 15 wavelengths?). See station A2008000 for example.

irradiance_clean %>%
  group_nest(station) %>%
  mutate(n = map_int(data, nrow))
