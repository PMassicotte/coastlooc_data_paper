# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the absorption spectral data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

# %% ---- aphy, anap, ap
absorption <- data.table::fread(here("data", "raw", "all_abs_transpose.txt"),
  na.strings = "9.999900000"
) |>
  as_tibble() |>
  pivot_longer(
    -wavelength,
    names_pattern = c("(.*)_(.*)"),
    names_to = c(".value", "station")
  ) |>
  arrange(station, wavelength) |>
  relocate(wavelength, .after = station) |>
  mutate(station = str_to_upper(station))

absorption

# Remove cdom data, will use the "raw" data with the complete range of
# wavelengths.
absorption <- absorption |>
  select(-contains("cdom"))

# Should have 371 wavelengths per station
absorption |>
  count(station) |>
  verify(n == 371)

# How many station do we have?
absorption |>
  distinct(station)

absorption |>
  filter(is.na(toa)) |>
  distinct(station)

# Rename absorption variables

absorption

absorption <- absorption |>
  rename(
    a_phy_m1 = pga,
    a_phy_specific_m2_mg_chla_m1 = aph_spe,
    a_nap_m1 = dta,
    a_p_m1 = toa
  ) |>
  relocate(a_p_m1, .after = wavelength) |>
  relocate(starts_with("a_phy"), .after = contains("a_phy"))

absorption
# %%

# %% ---- aCDOM Tidy and model aCDOM spectra.

# After discussion with Marcel, he provided the original (i.e. un-corrected
# aCDOM spectra) files. Here I am converting these from absorbance to
# absorption. Then I remove the average background (683 - 687 nm). Finally, I
# model the spectra using a simple exponential function (see Babin 2003 GJR).

files <- fs::dir_ls(here("data", "raw", "CDOM"), recurse = TRUE, glob = "*.YSA")

acdom <- files |>
  enframe(name = NULL, value = "filename") |>
  mutate(filesize = fs::file_size(filename)) |>
  filter(fs::as_fs_bytes(filesize) > 9000) |>
  select(-filesize) |>
  mutate(station = fs::path_ext_remove(fs::path_file(filename)))

acdom

acdom <- acdom |>
  mutate(absorbance = map(filename, ~ data.table::fread(
    .,
    skip = 86,
    col.names = c("wavelength", "absorbance")
  ))) |>
  unnest(absorbance) |>
  select(-filename)

acdom

acdom |>
  count(station)

# Most CDOM spectra start at 350 nm, some at 300 nm.
acdom |>
  group_by(station) |>
  filter(wavelength == min(wavelength)) |>
  ggplot(aes(x = wavelength)) +
  geom_histogram()

# Convert to absorption ---------------------------------------------------

# 10 cm cuvette (0.1 m)

acdom <- acdom |>
  mutate(absorption_m1 = (2.303 * absorbance) / 0.1) |>
  select(-absorbance)

# Visualize ---------------------------------------------------------------

p1 <- acdom |>
  filter(wavelength <= 750) |>
  ggplot(aes(x = wavelength, y = absorption_m1, group = station)) +
  geom_line(size = 0.1) +
  geom_hline(yintercept = 0, lty = 2, size = 0.25, color = "blue") +
  facet_wrap(~ str_sub(station, 1, 2), scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM] ~ (m^{
      -1
    })),
    title = quote(bold(Raw ~ a[CDOM] ~ spectra))
  )

# Baseline correction -----------------------------------------------------

# Calculate the average between 683 and 687 nm and subtract it from the spectra.
acdom <- acdom |>
  group_by(station) |>
  mutate(
    background_a_cdom_average_683_687_m1 =
      mean(absorption_m1[between(wavelength, 683, 687)])
  ) |>
  mutate(
    absorption_background_corrected_m1 =
      absorption_m1 - background_a_cdom_average_683_687_m1,
    .after = absorption_m1
  ) |>
  ungroup()

p2 <- acdom |>
  filter(wavelength <= 750) |>
  ggplot(
    aes(
      x = wavelength,
      y = absorption_background_corrected_m1,
      group = station
    )
  ) +
  geom_line(size = 0.1) +
  geom_hline(yintercept = 0, lty = 2, size = 0.25, color = "blue") +
  facet_wrap(~ str_sub(station, 1, 2), scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM] ~ (m^{
      -1
    })),
    title = quote(bold(Baseline ~ corrected ~ a[CDOM] ~ spectra)),
    subtitle = "Average aCDOM value between 683-687 nm has been removed."
  )

p <- p1 / p2

ggsave(
  here("graphs", "003_raw_and_background_corrected_acdom_spectra.pdf"),
  device = cairo_pdf,
  height = 8,
  width = 10
)

# Model acdom spectra -----------------------------------------------------

acdom <- acdom |>
  group_nest(station) |>
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      absorption_background_corrected_m1 ~ a443 * exp(-s * (wavelength - 443)),
      data = .,
      start = c(a443 = 0.2, s = 0.03),
      lower =  c(a443 = 0.001, s = 0.001),
      subset = between(wavelength, 350, 500)
    )
  ))

acdom <- acdom |>
  mutate(mod_augmented = map(mod, broom::augment)) |>
  mutate(mod_pred = map2(data, mod, modelr::add_predictions)) |>
  mutate(r2 = map_dbl(
    mod_augmented,
    ~ cor(.$absorption_background_corrected_m1, .$.fitted)^2
  ))

acdom

# Visualize fitted spectra ------------------------------------------------

df_viz <- acdom |>
  # slice_sample(n = 10)  |>
  select(station, mod_pred, r2) |>
  unnest(mod_pred) |>
  group_nest(station, r2, keep = TRUE) |>
  arrange(desc(r2))

df_viz

plot_acdom <- function(acdom) {
  p <- acdom |>
    ggplot(aes(x = wavelength, y = absorption_background_corrected_m1)) +
    geom_point(color = "gray50") +
    geom_line(aes(y = pred), color = "red") +
    geom_vline(xintercept = c(350, 500), lty = 2, color = "#3c3c3c") +
    scale_x_continuous(breaks = seq(200, 800, by = 50)) +
    labs(
      x = "Wavelength (nm)",
      y = quote(a[CDOM] ~ (lambda)),
      title = glue("{unique(acdom$station)} (R2 = {round(unique(acdom$r2), digits = 4)})"),
      subtitle = "Fits have been performed between 350 and 500 nm."
    )

  print(p)
}

cairo_pdf(
  here("graphs", "003_fitted_acdom_spectra_ordered_from_best_to_worst_fits.pdf"),
  width = 7,
  height = 5,
  onefile = TRUE
)

walk(df_viz$data, plot_acdom)

dev.off()

# Filter bad spectra ------------------------------------------------------

acdom |>
  select(r2) |>
  ggplot(aes(x = r2)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = 0.95, lty = 2, size = 0.25, color = "red")

min_r2 <- 0.95

# Only keep the best fits
df_filtered <- acdom |>
  filter(r2 >= min_r2) |>
  unnest(mod_pred) |>
  select(
    station,
    wavelength,
    a_cdom_measured_m1 = absorption_background_corrected_m1,
    a_cdom_modeled_m1 = pred,
    r2
  ) |>
  arrange(station, wavelength)

df_filtered

# Visualize the worst remaining fits.
p <- df_filtered |>
  group_nest(station, r2) |>
  top_n(n = 36, wt = -r2) |>
  unnest(data) |>
  ggplot(aes(x = wavelength, y = a_cdom_measured_m1)) +
  geom_point(size = 0.5) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  geom_line(aes(y = a_cdom_modeled_m1), color = "red") +
  facet_wrap(~ glue("{station}"), scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM] ~ (m^{
      -1
    })),
    title = "Worst fitted aCDOM spectra",
    subtitle = "I filtered to keep only fits with R2 >= 0.95. These are the worst 36 fits after the filter."
  )

ggsave(
  here("graphs", "003_worst_fitted_acdom_spectra.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 10
)
# %%

# %% ---- Merge all type of absorption

df_filtered

df_filtered <- df_filtered |>
  select(-r2)

absorption_merged <- absorption |>
  full_join(df_filtered, by = c("station", "wavelength")) |>
  add_count(station, wavelength) |>
  assertr::verify(n == 1) |> # make sure there is only 1 obs per station/wl
  select(-n)

absorption_merged

# %%

# %% ---- Remove outliers

absorption_merged |>
  arrange(station, wavelength)

# Remove any spectra if there are negative values below 500 nm

absorption_clean <- absorption_merged |>
  pivot_longer(starts_with("a_")) |>
  drop_na(value) |>
  group_by(station, name) |>
  filter(all(value[wavelength <= 500] >= 0, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(names_from = name, values_from = value) |>
  arrange(station, wavelength)

# %%

# %% ---- Export data
stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area)

absorption_clean |>
  semi_join(stations, by = "station") |>
  write_csv(here("data", "clean", "absorption.csv"))
# %%

# %% ---- SCDOM

s_cdom <- acdom |>
  semi_join(absorption_clean, by = "station") |>
  select(station, mod) |>
  mutate(tidied = map(mod, broom::tidy)) |>
  select(-mod) |>
  unnest(tidied) |>
  filter(term == "s") |>
  select(station, s_cdom_nm1 = estimate)

s_cdom

s_cdom |>
  ggplot(aes(x = s_cdom_nm1)) +
  geom_histogram() +
  geom_vline(xintercept = 0.012) #

# %%

# %% ---- Snap
anap <- absorption_clean |>
  select(station, wavelength, a_nap_m1)

anap

anap <- anap |>
  filter(between(wavelength, 380, 730)) |>
  filter(!between(wavelength, 400, 480) & !between(wavelength, 620, 710)) |>
  drop_na()

anap |>
  distinct(station)

anap |>
  ggplot(aes(x = wavelength, y = a_nap_m1, group = station)) +
  geom_point(size = 0.25)

# Fit the exponential models ----------------------------------------------

anap

s_nap <- anap |>
  group_nest(station) |>
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      a_nap_m1 ~ a443 * exp(-s * (wavelength - 500)),
      data = .,
      start = c(a443 = 0.2, s = 0.03),
      lower =  c(a443 = 0.001, s = 0.001)
    )
  ))

s_nap

s_nap <- s_nap |>
  mutate(tidied = map(mod, broom::tidy)) |>
  mutate(augmented = map(mod, broom::augment)) |>
  mutate(r2 = map_dbl(
    augmented,
    ~ cor(.$a_nap_m1, .$.fitted)^2
  ))

s_nap

s_nap |>
  ggplot(aes(x = r2)) +
  geom_histogram(binwidth = 0.001)

# Filter out bad spectra --------------------------------------------------

# Use the same r2 criterion as for s_cdom
min_r2 <- 0.95

s_nap <- s_nap |>
  filter(r2 >= min_r2)

# Visualize some fits -----------------------------------------------------

s_nap |>
  slice_sample(n = 49) |>
  select(station, augmented) |>
  unnest(augmented) |>
  ggplot(aes(x = wavelength, y = a_nap_m1)) +
  geom_point() +
  geom_line(aes(y = .fitted), color = "red") +
  facet_wrap(~station, scales = "free_y")

# Export s_nap ------------------------------------------------------------

s_nap <- s_nap |>
  select(station, tidied) |>
  unnest(tidied) |>
  filter(term == "s") |>
  select(station, s_nap_nm1 = estimate)

s_nap |>
  ggplot(aes(x = s_nap_nm1)) +
  geom_histogram()
# %%

# %% ---- Save scdom and s_nap
s_cdom
s_nap

s_cdom |>
  anti_join(s_nap, by = "station")

s_nap |>
  anti_join(s_cdom, by = "station")

spectral_slope <- full_join(s_cdom, s_nap, by = "station")

write_csv(spectral_slope, here("data", "clean", "spectral_slopes.csv"))
# %%
