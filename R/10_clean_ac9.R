# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Remove AC9 outliers.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

ac9 <- read_csv(here("data/clean/ac9.csv"))

# Histogram of raw data ---------------------------------------------------

df_viz <- ac9 %>%
  select(-remote_sensed_vertical_layer_meter) %>%
  pivot_longer(-c(station, wavelength))

df_viz %>%
  count(name, wavelength)

p <- df_viz %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name} ({wavelength} nm)"), scales = "free", ncol = 10) +
  geom_vline(xintercept = 0, color = "red", lty = 2, size = 1) +
  labs(
    title = "Histograms of raw AC9 data",
    subtitle = "There are negative values in this data. We can also see that a(715) is always 0."
  )

ggsave(
  here("graphs/10_histogram_raw_ac9.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Remove negative values --------------------------------------------------

ac9_clean <- ac9 %>%
  mutate(across(
    a:c_dissolved,
    ~ case_when(
      . >= 0 ~ .,
      TRUE ~ NA_real_
    )
  ))

p <- ac9_clean %>%
  pivot_longer(-c(station, wavelength, remote_sensed_vertical_layer_meter)) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name} ({wavelength} nm)"), scales = "free", ncol = 10) +
  labs(
    title = "Histograms of ac9 data",
    subtitle = "Negative values were set to NA."
  )

ggsave(
  here("graphs/10_histogram_ac9_negative_values_removed.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Export clean data -------------------------------------------------------

ac9_clean %>%
  write_csv(here("data/clean/ac9.csv"))
