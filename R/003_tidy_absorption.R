# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the absorption spectral data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

# %% ---- Tidy anap and ap

files <- fs::dir_ls(
  here("data", "raw", "absorption"),
  glob = "*.dta|*.toa",
  ignore.case = TRUE
)

anap_ap <- map(
  files,
  data.table::fread,
  col.names = c("wavelength", "absorption"),
  drop = 3
) |>
  bind_rows(.id = "filename") |>
  as_tibble() |>
  mutate(filename = basename(filename)) |>
  mutate(type = ifelse(
    str_detect(filename, regex("\\.dta", ignore_case = TRUE)),
    "a_nap_m1",
    "a_p_m1"
  )) |>
  mutate(
    station = tools::file_path_sans_ext(filename),
    .keep = "unused",
    .before = 1
  ) |>
  mutate(station = str_to_upper(station))

anap_ap <- anap_ap |>
  pivot_wider(names_from = type, values_from = absorption)

anap_ap

anap_ap |>
  distinct(station)

anap_ap |>
  count(station) |>
  assertr::verify(n == 371)
# %%

# %% ---- Tidy aCDOM and convert absorbance to absorption

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

# Convert to absorption: 10 cm cuvette (0.1 m)

acdom <- acdom |>
  mutate(a_cdom_m1 = (2.303 * absorbance) / 0.1) |>
  select(-absorbance)
# %%

# %% ---- Merge all absorption (anap ap and acdom)

anap_ap
acdom

absorption <- full_join(anap_ap, acdom, by = c("station", "wavelength"))

absorption
# %%

# %% ---- Visualize a few absorption spectra

absorption |>
  pivot_longer(starts_with("a_")) |>
  ggplot(aes(x = wavelength, y = value, group = station)) +
  geom_line(linewidth = 0.1) +
  facet_wrap(~name, scales = "free_y")

set.seed(1234)

absorption |>
  pivot_longer(starts_with("a_")) |>
  group_nest(station) |>
  sample_n(size = 9) |>
  unnest(data) |>
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_line() +
  geom_hline(yintercept = 0, lty = 2, color = "red") +
  facet_wrap(~station, scale = "free_y")

# %%

# %% ---- Model absorption spectra

spectral_slopes <- absorption |>
  select(-a_p_m1) |>
  mutate(a_cdom_m1 = ifelse(wavelength > 700, NA, a_cdom_m1)) |>
  mutate(a_nap_m1 = ifelse(
    between(wavelength, 400, 480) |
      between(wavelength, 620, 710), NA, a_nap_m1
  )) |>
  pivot_longer(starts_with("a_")) |>
  drop_na(value) |>
  group_nest(station, name) |>
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      value ~ a443 * exp(-s * (wavelength - 443)) + k,
      data = .,
      start = c(a443 = 0.2, s = 0.03, k = 0)
    )
  ))

spectral_slopes <- spectral_slopes |>
  mutate(mod_augmented = map(mod, broom::augment)) |>
  mutate(mod_pred = map2(data, mod, modelr::add_predictions)) |>
  mutate(mod_tidied = map(mod, broom::tidy)) |>
  mutate(r2 = map_dbl(
    mod_augmented,
    ~ cor(.$value, .$.fitted)^2
  ))

spectral_slopes

spectral_slopes |>
  unnest(mod_augmented) |>
  mutate(mission = str_sub(station, 1, 2), .after = station) |>
  ggplot(aes(x = wavelength, y = value, group = station)) +
  geom_point(alpha = 0.5, size = 0.25) +
  geom_line(aes(y = .fitted), color = "red", linewidth = 0.1) +
  facet_wrap(vars(name, mission), scales = "free_y", ncol = 6)

ggsave(
  here("graphs", "003_fitted_absorption_spectra.pdf"),
  device = cairo_pdf,
  width = 14,
  height = 6
)

mod_coef <- spectral_slopes |>
  unnest(mod_tidied) |>
  select(station, name, term, estimate)

mod_coef

anap_acdom_background <- mod_coef |>
  filter(term == "k") |>
  select(station, name, k = estimate)

# %%

# %% ---- Calculate background for ap

# Given that we are not able to model ap (not with an exponential model), lets
# calculate the average between 746 and 750 nm.

ap_background <- absorption |>
  filter(station %in% unique(mod_coef$station)) |>
  select(station, wavelength, a_p_m1) |>
  drop_na() |>
  pivot_longer(starts_with("a_")) |>
  filter(between(wavelength, 746, 750)) |>
  group_by(station, name) |>
  summarise(k = mean(value), .groups = "drop")

ap_background

# %%

# %% ---- Remove background from spectra (acdom, anap and ap)

ap_background

anap_acdom_background

background <- bind_rows(ap_background, anap_acdom_background) |>
  pivot_wider(
    names_from = name,
    values_from = k,
    names_glue = "background_{name}"
  )

background

absorption_corrected <- absorption |>
  inner_join(background) |>
  mutate(a_cdom_adjusted_m1 = a_cdom_m1 - background_a_cdom_m1) |>
  mutate(
    a_nap_adjusted_m1 =
      a_nap_m1 - background_a_nap_m1 + background_a_p_m1
  ) |>
  mutate(a_phy_m1 = a_p_m1 - a_nap_adjusted_m1)

absorption_corrected

# %%

# %% ---- Visualize the corrected spectra
p <- absorption_corrected |>
  select(station, wavelength, starts_with("a_")) |>
  pivot_longer(starts_with("a_")) |>
  group_nest(station, keep = TRUE) |>
  # slice(1:20) |>
  mutate(p = map(data, \(data) {
    data |>
      ggplot(aes(x = wavelength, y = value, color = name)) +
      geom_line() +
      geom_hline(yintercept = 0, lty = 2) +
      scale_color_brewer(palette = "Set2") +
      labs(
        title = unique(data$station),
        x = NULL,
        y = parse(text = "Absorption~(m^{-1})"),
        color = NULL
      )
  }))

cairo_pdf(
  here("graphs", "003_absorption_spectra_clean.pdf"),
  width = 7,
  height = 5,
  onefile = TRUE
)

walk(p$p, print)

dev.off()
# %%

# %% ---- Merge spectral snap and scdom

spectral_slopes <- spectral_slopes |>
  select(station, name, mod_tidied) |>
  unnest(mod_tidied) |>
  filter(term == "s") |>
  select(station, name, s = estimate) |>
  pivot_wider(names_from = name, values_from = s) |>
  rename(
    s_cdom_nm1 = a_cdom_m1,
    s_nap_nm1 = a_nap_m1
  )

mean(spectral_slopes$s_cdom_nm1, na.rm = TRUE)
mean(spectral_slopes$s_nap_nm1, na.rm = TRUE)

write_csv(spectral_slopes, here("data", "clean", "spectral_slopes.csv"))

# %%


# %% ---- Export data

absorption_corrected |>
  count(station, wavelength) |>
  assertr::verify(n == 1)

absorption_corrected |>
  relocate(starts_with("a_"), .after = wavelength) |>
  write_csv(here("data", "clean", "absorption.csv"))
# %%
