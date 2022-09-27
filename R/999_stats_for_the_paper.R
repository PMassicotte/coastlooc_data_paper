# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Various stats for the paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

# Pearson correlation between aphi and total chl-a ------------------------

station <- read_csv(here("data", "clean", "stations.csv"))

surface <- read_csv(here("data", "clean", "surface.csv"))

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  filter(wavelength %in% c(440, 675))

df <- absorption |>
  inner_join(station, ., by = "station") |>
  inner_join(surface, ., by = "station")

df

df |>
  ggplot(aes(x = a_phy_m1, y = total_chl_a)) +
  geom_point(aes(color = area)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", color = "black") +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  facet_wrap(~wavelength)

df |>
  group_nest(wavelength) |>
  mutate(correlation = map_dbl(data, ~ cor(
    log10(.$total_chl_a), log10(.$a_phy_m1),
    use = "complete.obs"
  )))


# AC9 normalized spectra? -------------------------------------------------

ac9 <- read_csv(here("data", "clean", "ac9.csv"))

ac9 |>
  filter(str_starts(station, "C1")) |>
  drop_na(bp_m1) |>
  ggplot(aes(x = wavelength, y = bp_m1, group = station)) +
  geom_line() +
  facet_wrap(~station, scales = "free_y")


ac9 |>
  # filter(str_starts(station, "C4"))  |>
  drop_na(bp_m1) |>
  group_by(station) |>
  mutate(bp_m1 = bp_m1 / pracma::trapz(wavelength, bp_m1)) |>
  ggplot(aes(x = wavelength, y = bp_m1, group = station)) +
  geom_line()

# %% ---- Chla classification (Antoine1996)

surface

surface |>
  mutate(trophic_status = case_when(
    total_chl_a <= 0.1 ~ "oligo",
    between(total_chl_a, 0.1, 1) ~ "meso",
    total_chl_a > 1 ~ "eutro",
    TRUE ~ NA_character_
  )) |>
  count(trophic_status)

# %%

# %% ---- POC
surface |>
  inner_join(station, by = "station") |>
  group_by(area) |>
  summarise(across(poc_g_m3, median, na.rm = TRUE)) |>
  mutate(across(poc_g_m3, round, digits = 1)) |>
  ungroup() |>
  arrange(poc_g_m3)

range(surface$poc_g_m3, na.rm = TRUE)
# %%
