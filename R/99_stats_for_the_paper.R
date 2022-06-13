# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Various stats for the paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R","zzz.R"))

# Pearson correlation between aphi and total chl-a ------------------------

station <- read_csv(here("data","clean","stations.csv"))

surface <- read_csv(here("data","clean","surface.csv"))

absorption <- read_csv(here("data","clean","absorption.csv")) %>%
  filter(wavelength %in% c(440, 675))

df <- absorption %>%
  inner_join(station, ., by = "station") %>%
  inner_join(surface, ., by = "station")

df

df %>%
  ggplot(aes(x = a_phy, y = total_chl_a)) +
  geom_point(aes(color = area)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  facet_wrap(~wavelength)

df %>%
  group_nest(wavelength) %>%
  mutate(correlation = map_dbl(data, ~ cor(
    log10(.$total_chl_a), log10(.$a_phy),
    use = "complete.obs"
  )))


# AC9 normalized spectra? -------------------------------------------------

ac9 <- read_csv(here("data","clean","ac9_negative_values_removed.csv"))

ac9 %>%
  filter(str_starts(station, "C1")) %>%
  drop_na(bp) %>%
  ggplot(aes(x = wavelength, y = bp, group = station)) +
  geom_line() +
  facet_wrap(~station, scales = "free_y")


ac9 %>%
  # filter(str_starts(station, "C4")) %>%
  drop_na(bp) %>%
  group_by(station) %>%
  mutate(bp = bp / pracma::trapz(wavelength, bp)) %>%
  ggplot(aes(x = wavelength, y = bp, group = station)) +
  geom_line()


# Kd vs bp ----------------------------------------------------------------

ac9 <- read_csv(here("data", "clean", "ac9_negative_values_removed.csv")) %>%
  filter(wavelength == 440)

kd <- read_csv(here("data", "clean", "irradiance_negative_values_removed.csv")) %>%
  filter(wavelength == 443)

df <- ac9 %>%
  inner_join(kd, by = "station") %>%
  drop_na(bp, kd_m1)

df %>%
  ggplot(aes(x = bp, y = measured_reflectance)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

cor(log10(df$bp), log10(df$kd_m1))

df %>%
  ggplot(aes(x = bp, y = ed_wm2_nm1)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

cor(log10(df$bp), log10(df$ed_wm2_nm1))

## Relation between kd and Ed ----

# No relation

kd %>%
  ggplot(aes(x = ed_wm2_nm1, y = kd_m1)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()
