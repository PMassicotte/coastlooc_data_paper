# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Remove outliers in the irradiance data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "ggspectral.R"))

irradiance <- read_csv(here("data", "raw", "tidied", "irradiance.csv"))

irradiance

# Histogram of irradiance data --------------------------------------------

df_viz <- irradiance |>
  pivot_longer(-c(station, wavelength))

df_viz |>
  filter(value < 0) |>
  count(wavelength, name)

p <- df_viz |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name}\n({wavelength} nm)"), scales = "free", ncol = 15) +
  geom_vline(xintercept = 0, color = "red", lty = 2, size = 1) +
  labs(
    title = "Histograms of raw irradiance data",
    subtitle = "There are negative values in this data. We can also see that there are a common value of -95 in the Ku data."
  )

ggsave(
  here("graphs", "07_histogram_raw_irradiance.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Remove negative values --------------------------------------------------

irradiance_clean <- irradiance |>
  mutate(across(
    eu_w_m2_um:kd_m1,
    ~ case_when(
      . > 0 ~ .,
      TRUE ~ NA_real_
    )
  ))

p <- irradiance_clean |>
  pivot_longer(-c(station, wavelength)) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name}\n({wavelength} nm)"), scales = "free", ncol = 15) +
  labs(
    title = "Histograms of irradiance data",
    subtitle = "Negative values were set to NA."
  )

ggsave(
  here("graphs", "07_histogram_irradiance_negative_values_removed.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Export clean data -------------------------------------------------------

irradiance_clean |>
  write_csv(here("data", "clean", "irradiance.csv"))

# Visualize the data ------------------------------------------------------

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

df_viz <- irradiance_clean |>
  left_join(stations, by = "station")

p_eu <- ggspectral(df_viz, eu_w_m2_um, "E[u](0^'-')~'['~W~m^{-2}~um^{-1}~']'")
p_ed <- ggspectral(df_viz, ed_w_m2_um, "E[d](0^'-')~'['~W~m^{-2}~um^{-1}~']'")
p_ku <- ggspectral(df_viz, ku_m1, "K[u]~(m^{-1})")
p_kd <- ggspectral(df_viz, kd_m1, "K[d]~(m^{-1})")

save_fun(p_eu, here("graphs", "07_eu_spectral_profiles_by_area.pdf"))
save_fun(p_ed, here("graphs", "07_ed_spectral_profiles_by_area.pdf"))
save_fun(p_ku, here("graphs", "07_ku_spectral_profiles_by_area.pdf"))
save_fun(p_kd, here("graphs", "07_kd_spectral_profiles_by_area.pdf"))
