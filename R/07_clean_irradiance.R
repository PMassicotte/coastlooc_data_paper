# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Remove outliers in the irradiance data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# TODO: Add units to measurements: Wm2 nm1

rm(list = ls())

source("R/zzz.R")

irradiance <- read_csv(here("data/clean/irradiance.csv"))

irradiance

# Rename variables to add units -------------------------------------------

irradiance <- irradiance %>%
  rename_with(~glue("{.}_wm2_nm1"), c(eu, ed)) %>%
  rename_with(~glue("{.}_m1"), c(ku, kd))

# Histogram of irradiance data --------------------------------------------

df_viz <- irradiance %>%
  pivot_longer(-c(station, wavelength))

df_viz %>%
  filter(value < 0) %>%
  count(wavelength, name)

p <- df_viz %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name}\n({wavelength} nm)"), scales = "free", ncol = 15) +
  geom_vline(xintercept = 0, color = "red", lty = 2, size = 1) +
  labs(
    title = "Histograms of raw irradiance data",
    subtitle = "There are negative values in this data. We can also see that there are a common value of -95 in the Ku data."
  )

ggsave(
  here("graphs/07_histogram_raw_irradiance.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Remove negative values --------------------------------------------------

irradiance_clean <- irradiance %>%
  mutate(across(
    eu_wm2_nm1:kd_m1,
    ~ case_when(
      . >= 0 ~ .,
      TRUE ~ NA_real_
    )
  ))

p <- irradiance_clean %>%
  pivot_longer(-c(station, wavelength)) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ glue("{name}\n({wavelength} nm)"), scales = "free", ncol = 15) +
  labs(
    title = "Histograms of irradiance data",
    subtitle = "Negative values were set to NA."
  )

ggsave(
  here("graphs/07_histogram_irradiance_negative_values_removed.pdf"),
  device = cairo_pdf,
  width = 24,
  height = 10
)

# Export clean data -------------------------------------------------------

irradiance_clean %>%
  write_csv(here("data/clean/irradiance_negative_values_removed.csv"))

# Visualize the data ------------------------------------------------------

stations <- read_csv(here("data/clean/stations.csv")) %>%
  select(station, area)

df_viz <- irradiance_clean %>%
  left_join(stations, by = "station")

ggirradiance <- function(df, variable, ylab) {

  p <- df_viz %>%
    ggplot(aes(x = wavelength, y = {{ variable }}, color = area, group = station)) +
    geom_line(size = 0.1) +
    scale_color_manual(
      breaks = area_breaks,
      values = area_colors
    ) +
    facet_wrap(~area, scales = "free_y") +
    labs(
      x = "Wavelength (nm)",
      y = parse(text = ylab),
      subtitle = "There are some wavelengths missing among missions. Note that negative values have been removed."
    ) +
    theme(
      legend.position = "none"
    )

}

# quote(E[u](0^"-")~"["~W~m^{-2}~nm^{-1}~"]")

p_eu <- ggirradiance(df_viz, eu_wm2_nm1, "E[u](0^'-')~'['~W~m^{-2}~nm^{-1}~']'")
p_ed <- ggirradiance(df_viz, ed_wm2_nm1, "E[d](0^'-')~'['~W~m^{-2}~nm^{-1}~']'")
p_ku <- ggirradiance(df_viz, ku_m1, "K[u]~(m^{-1})")
p_kd <- ggirradiance(df_viz, kd_m1, "K[d]~(m^{-1})")

save_fun <- function(p) {

  fname <- deparse(substitute(p)) %>%
    str_remove("p_")

  ggsave(
    here(glue("graphs/07_{fname}_spectral_profiles_by_area.pdf")),
    plot = p,
    device = cairo_pdf,
    height = 6,
    width = 10
  )

  ggsave(
    here(glue("graphs/07_{fname}_spectral_profiles_by_area.png")),
    plot = p,
    dpi = 300,
    height = 6,
    width = 10
  )
}

save_fun(p_eu)
save_fun(p_ed)
save_fun(p_ku)
save_fun(p_kd)


#
# outfile <- here("graphs/07_eu_spectral_profiles_per_area.pdf")
#
# ggsave(outfile,
#   device = cairo_pdf,
#   width = 8,
#   height = 5
# )
#
# pdftools::pdf_convert(
#   outfile,
#   format = "png",
#   filenames = fs::path_ext_set(outfile, "png"),
#   dpi = 300
# )
