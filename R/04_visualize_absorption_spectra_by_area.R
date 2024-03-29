# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the absorption spectra (a_cdom, a_nap, a_tot, etc.).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")
source("R/ggspectral.R")

absorption <- read_csv(here("data","clean","absorption.csv"))

stations <- read_csv(here("data","clean","stations.csv"))

absorption <- absorption %>%
  full_join(stations, by = "station") %>%
  add_count(station, area, wavelength) %>%
  assertr::verify(n == 1) %>%
  select(-n)

p_a_phy <- ggspectral(drop_na(absorption, a_phy), a_phy, "a[phy]~(m^{-1})")
p_a_nap <- ggspectral(drop_na(absorption, a_nap), a_nap, "a[nap]~(m^{-1})")
p_a_p <- ggspectral(drop_na(absorption, a_p), a_p, "a[p]~(m^{-1})")
p_a_cdom <- ggspectral(drop_na(absorption, a_cdom_modeled), a_cdom_modeled, "a[cdom]~(m^{-1})")

save_fun(p_a_phy, here("graphs", "04_aphy_spectral_profiles_by_area.pdf"))
save_fun(p_a_nap, here("graphs", "04_anap_spectral_profiles_by_area.pdf"))
save_fun(p_a_p, here("graphs", "04_ap_spectral_profiles_by_area.pdf"))
save_fun(p_a_cdom, here("graphs", "04_acdom_spectral_profiles_by_area.pdf"))
