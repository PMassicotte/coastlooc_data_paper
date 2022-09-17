# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the absorption spectra (a_cdom, a_nap, a_tot, etc.).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))
source(here("R", "ggspectral.R"))

absorption <- read_csv(here("data", "clean", "absorption.csv"))

stations <- read_csv(here("data", "clean", "stations.csv"))

absorption <- absorption |>
  full_join(stations, by = "station") |>
  add_count(station, area, wavelength) |>
  assertr::verify(n == 1) |>
  select(-n)

p_a_phy <- ggspectral(drop_na(absorption, a_phy_m1), a_phy_m1, "a[phy]~(m^{-1})")
p_a_nap <- ggspectral(drop_na(absorption, a_nap_m1), a_nap_m1, "a[nap]~(m^{-1})")
p_a_p <- ggspectral(drop_na(absorption, a_p_m1), a_p_m1, "a[p]~(m^{-1})")
p_a_cdom <- ggspectral(drop_na(absorption, a_cdom_modeled_m1), a_cdom_modeled_m1, "a[cdom]~(m^{-1})")

save_fun(p_a_phy, here("graphs", "09_aphy_spectral_profiles_by_area.pdf"))
save_fun(p_a_nap, here("graphs", "09_anap_spectral_profiles_by_area.pdf"))
save_fun(p_a_p, here("graphs", "09_ap_spectral_profiles_by_area.pdf"))
save_fun(p_a_cdom, here("graphs", "09_acdom_spectral_profiles_by_area.pdf"))
