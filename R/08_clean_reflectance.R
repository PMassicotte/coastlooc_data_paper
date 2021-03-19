# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Remove reflectance outliers.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")
source("R/ggspectral.R")

reflectance <- read_csv(here("data/clean/reflectance.csv"))

# Histogram of raw reflectance --------------------------------------------

p <- reflectance %>%
  ggplot(aes(x = measured_reflectance)) +
  geom_histogram() +
  facet_wrap(~ glue("{wavelength} nm"), scales = "free_x") +
  geom_vline(xintercept = 0, color = "red", lty = 2, size = 0.25) +
  geom_vline(xintercept = 1, color = "red", lty = 2, size = 0.25) +
  scale_x_continuous(
    labels = scales::label_percent()
  ) +
  labs(
    title = "Histograms of raw reflectance data",
    subtitle = "There are values outside the 0-1 range.",
    x = "Measured reflectance"
  )

ggsave(
  here("graphs/08_histogram_raw_reflectance.pdf"),
  width = 7,
  height = 5
)

# Remove values outside the 0-1 range -------------------------------------

reflectance

reflectance_clean <- reflectance %>%
  mutate(measured_reflectance = case_when(
    between(measured_reflectance, 0, 1) ~ measured_reflectance,
    TRUE ~ NA_real_
  ))

p <- reflectance_clean %>%
  ggplot(aes(x = measured_reflectance)) +
  geom_histogram() +
  facet_wrap(~ glue("{wavelength} nm"), scales = "free_x") +
  scale_x_continuous(
    labels = scales::label_percent(),
    breaks = scales::breaks_pretty(n = 3),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  labs(
    title = "Histograms of reflectance data",
    subtitle = "Values lower than 0% or higher than 100% have been removed.",
    x = "Measured reflectance"
  )

ggsave(
  here("graphs/08_histogram_reflectance_values_outside_0_1_removed.pdf"),
  width = 7,
  height = 5
)

# Export clean data -------------------------------------------------------

reflectance_clean %>%
  write_csv(here("data/clean/reflectance_negative_values_removed.csv"))

# Visualize the data ------------------------------------------------------

stations <- read_csv(here("data/clean/stations.csv")) %>%
  select(station, area)

df_viz <- reflectance_clean %>%
  inner_join(stations, by = "station")

p_reflectance <- ggspectral(df_viz, measured_reflectance, "Reflectance")

save_fun(p_reflectance, here("graphs", "08_reflectance_profiles_by_area.pdf"))
