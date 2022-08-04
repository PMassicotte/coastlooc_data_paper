# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Absorption partition.
#
# Oubelkheir et al., Partitioning Total Spectral Absorption in Phytoplankton and
# Colored Detrital Material Contributions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  select(-a_phy_specific_m1)

# Select the same three stations as in Oubelkheir 2007.
df <- absorption |>
  filter(station %in% c("C6024000", "C3011000", "C6148000")) |>
  filter(between(wavelength, 400, 700))

df |>
  distinct(station)

df

# Plot absorption spectra -------------------------------------------------

df_viz <- df |>
  pivot_longer(starts_with("a_"),
    names_to = "absorption_type",
    values_to = "absorption"
  )

p <- df_viz |>
  ggplot(aes(x = wavelength, y = absorption, color = absorption_type)) +
  geom_line() +
  facet_wrap(~station, scales = "free", ncol = 1) +
  labs(
    x = "Wavelength (nm)",
    y = quote(Absorption ~ (m^{
      -1
    }))
  ) +
  theme(
    legend.title = element_blank()
  )

ggsave(
  here("graphs", "17_absorption_partition_for_three_stations.pdf"),
  device = cairo_pdf,
  width = 7,
  height = 10
)

# How many spectra where a_tot is lower than any of its constituent -------

absorption |>
  filter(between(wavelength, 400, 400)) |>
  drop_na(a_p_m1) |>
  group_by(station) |>
  filter(if_any(c(a_phy_m1, a_nap_m1), ~ . > a_p_m1))

# Check absorption variability --------------------------------------------

#  In some spectral domains, the variability in the shape of the phytoplankton
#  absorption coefficient spectrum is low whereas the variability in the
#  corresponding particulate absorption coefficient is large.

# The difference, therefore, is attributed to the variability in non-algal
# particle absorption coefficient. Two pairs of wavelengths (505/380 and 580/692
# or λ2/λ1 and λ3/λ4) have been identified for which absorption coefficient
# ratios are relatively stable in phytoplankton, because they are only weakly
# affected by the spectral variations due to pigment composition (i.e., minimal
# absorption by accessory pigments) and the package effect (ratios must be close
# to 1.0).

df_viz <- absorption |>
  filter(wavelength %in% c(380, 505, 580, 692)) |>
  drop_na() |>
  group_by(station) |>
  summarise(across(c(a_phy_m1, a_nap_m1),
    .fns = list(
      r1 = ~ .[wavelength == 505] / .[wavelength == 380],
      r2 = ~ .[wavelength == 580] / .[wavelength == 692]
    )
  ))

df_viz

df_viz |>
  pivot_longer(-station) |>
  ggplot(aes(x = value)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~name, scales = "free_y")
