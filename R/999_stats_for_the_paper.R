# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Various stats for the paper.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

# Pearson correlation between aphi and total chl-a ------------------------

station <- read_csv(here("data", "clean", "stations.csv"))

surface <- read_csv(here("data", "clean", "pigments.csv"))

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  filter(wavelength %in% c(440, 675))

df <- absorption |>
  inner_join(station, ., by = "station") |>
  inner_join(surface, ., by = "station")

df

df |>
  ggplot(aes(x = a_phy_m1, y = chlorophyll_a_mg_m3)) +
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
    log10(.$chlorophyll_a_mg_m3), log10(.$a_phy_m1),
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
    chlorophyll_a_mg_m3 <= 0.1 ~ "oligo",
    between(chlorophyll_a_mg_m3, 0.1, 1) ~ "meso",
    chlorophyll_a_mg_m3 > 1 ~ "eutro",
    TRUE ~ NA_character_
  )) |>
  count(trophic_status)

# %%

# %% ---- POC
poc <- read_csv(here("data", "clean", "nutrients.csv"))

poc |>
  inner_join(station, by = "station") |>
  group_by(area) |>
  summarise(across(particulate_organic_carbon_g_m3, median, na.rm = TRUE)) |>
  mutate(across(particulate_organic_carbon_g_m3, round, digits = 1)) |>
  ungroup() |>
  arrange(particulate_organic_carbon_g_m3)

range(poc$particulate_organic_carbon_g_m3, na.rm = TRUE)
# %%

# %% ---- Chlorophyll-a
range(surface$chlorophyll_a_mg_m3, na.rm = TRUE)
# %%
