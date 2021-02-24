# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy and model aCDOM spectra.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

files <- fs::dir_ls(here("data/raw/CDOM/"), recurse = TRUE, glob = "*.YSA")

df <- files %>%
  enframe(name = NULL, value = "filename") %>%
  mutate(filesize = fs::file_size(filename)) %>%
  filter(fs::as_fs_bytes(filesize) > 9000) %>%
  select(-filesize) %>%
  mutate(station = fs::path_ext_remove(fs::path_file(filename)))

df

df <- df %>%
  mutate(cruise = str_sub(station, 1, 2))

df <- df %>%
  mutate(absorbance = map(filename, ~ data.table::fread(
    .,
    skip = 86,
    col.names = c("wavelength", "absorbance")
  ))) %>%
  unnest(absorbance) %>%
  select(-filename)

df

# Convert to absorption ---------------------------------------------------

# 10 cm cuvette (0.1 m)

df <- df %>%
  mutate(absorption = (2.303 * absorbance) / 0.1) %>%
  select(-absorbance)

# Visualize ---------------------------------------------------------------

p1 <- df %>%
  filter(wavelength <= 750) %>%
  ggplot(aes(x = wavelength, y = absorption, group = station)) +
  geom_line(size = 0.1) +
  geom_hline(yintercept = 0, lty = 2, size = 0.25, color = "blue") +
  facet_wrap(~cruise, scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM]~(m^{-1})),
    title = quote(bold(Raw~a[CDOM]~spectra))
  )

# Baseline correction -----------------------------------------------------

df <- df %>%
  group_by(station) %>%
  mutate(background_a_cdom_average_683_687 = mean(absorption[between(wavelength, 683, 687)])) %>%
  mutate(
    absorption_background_corrected = absorption - background_a_cdom_average_683_687,
    .after = absorption
  ) %>%
  ungroup()

p2 <- df %>%
  filter(wavelength <= 750) %>%
  ggplot(aes(x = wavelength, y = absorption_background_corrected, group = station)) +
  geom_line(size = 0.1) +
  geom_hline(yintercept = 0, lty = 2, size = 0.25, color = "blue") +
  facet_wrap(~cruise, scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM]~(m^{-1})),
    title = quote(bold(Baseline~corrected~a[CDOM]~spectra))
  )

p <- p1 / p2

ggsave(
  here("graphs/01_03_raw_absorption_spectra.pdf"),
  device = cairo_pdf,
  height = 8,
  width = 10
)

# Model acdom spectra -----------------------------------------------------

df <- df %>%
  # filter(between(wavelength, 350, 500)) %>%
  group_nest(station, cruise) %>%
  mutate(mod = map(
    data,
    ~ minpack.lm::nlsLM(
      absorption_background_corrected ~ a443 * exp(-s * (wavelength - 443)),
      data = .,
      start = c(a443 = 0.2, s = 0.03),
      lower =  c(a443 = 0.001, s = 0.001),
      subset = between(wavelength, 350, 500)
    )
  ))

df <- df %>%
  mutate(mod_augmented = map(mod, broom::augment)) %>%
  mutate(mod_pred = map2(data, mod, modelr::add_predictions)) %>%
  mutate(r = map_dbl(
    mod_augmented,
    ~ cor(.$absorption_background_corrected, .$.fitted)
  ))

df

# Visualize fitted models -------------------------------------------------

set.seed(2021)

df_pred <- df %>%
  group_by(cruise) %>%
  slice_sample(n = 10) %>%
  unnest(mod_pred)

p <- df_pred %>%
  filter(between(wavelength, 350, 500)) %>%
  ggplot(aes(x = wavelength, y = absorption_background_corrected, group = station)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = pred), size = 0.25, color = "red") +
  geom_hline(yintercept = 0, lty = 2, size = 0.25, color = "blue") +
  facet_wrap(~station, scales = "free_y", ncol = 10) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM]~(m^{-1})),
    title = quote(bold(Baseline~corrected~a[CDOM]~spectra)),
    subtitle = str_wrap("Random selection of 10 spectra per cruise. The red line is the fitted model between 350-500 nm.", 120)
  )

ggsave(
  here("graphs/01_03_fitted_acdom.pdf"),
  device = cairo_pdf,
  width = 18,
  height = 10
)

# Compare with Marcel's acdom spectra -------------------------------------

df_viz <- df %>%
  unnest(mod_pred) %>%
  select(station, wavelength, a_cdom_modeled = pred, absorption_background_corrected, r) %>%
  ungroup()

df_viz

absorption <- vroom::vroom(here("data/clean/absorption_background_corrected.csv")) %>%
  select(station, wavelength, a_cdom)

# Seems there are more stations included in the complete acdom spectra compared
# to the data included in all_abs_transpose.txt

# 381 stations
df_viz %>%
  distinct(station)

# 311 stations
absorption %>%
  distinct(station)

df_viz <- df_viz %>%
  left_join(absorption, by = c("station", "wavelength"))

df_viz

# This is one of the station where we do not have a good match between Marcel
# fits and my fits.

lab <- df_viz %>%
  filter(station == "C5029000") %>%
  filter(wavelength == 443)

p1 <- df_viz %>%
  # filter(r > 0.95) %>%
  filter(wavelength == 443) %>%
  ggplot(aes(x = a_cdom_modeled, y = a_cdom, label = station)) +
  geom_point(size = 0.5, color = "#3c3c3c") +
  geom_abline(size = 0.25, color = "red", lty = 2) +
  ggforce::geom_mark_circle(
    data = lab,
    aes(label = station),
    expand = unit(1, "mm"),
    color = "red"
  ) +
  labs(
    title = bquote(bold("Comparing fitted"~a[CDOM]~(443))),
    subtitle = "There are only few mismatches.",
    x = quote(a[CDOM]~(Phil)),
    y = quote(a[CDOM]~(Marcel))
  )

p2 <- df_viz %>%
  filter(station == "C5029000") %>%
  pivot_longer(contains("cdom")) %>%
  ggplot(aes(x = wavelength, y = value, color = name)) +
  geom_point(aes(y = absorption_background_corrected), size = 0.5, color = "#3c3c3c") +
  geom_line() +
  labs(
    title = "A closer look at the station C5029000",
    x = "Wavelength (nm)",
    y = quote(a[CDOM]~(m^{-1}))
  ) +
  scale_color_discrete(
    breaks = c("a_cdom", "a_cdom_modeled"),
    labels = c(quote(a[CDOM]~(Marcel)), quote(a[CDOM]~(Phil)))
  ) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

p <- p1 / p2 +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    plot.tag = element_text(face = "bold")
  )

ggsave(
  here("graphs/01_03_comparing_acdom_spectra.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 8
)

# Export the acdom spectra ------------------------------------------------

df %>%
  select(r) %>%
  ggplot(aes(x = r)) +
  geom_histogram(binwidth = 0.001)

# Only keep the best fits (R2 >= 0.99)
df_viz <- df %>%
  filter(r * r >= 0.99) %>%
  unnest(mod_augmented) %>%
  select(station,
    wavelength,
    a_cdom_measured = absorption_background_corrected,
    a_cdom_modeled = .fitted,
    r
  ) %>%
  arrange(station, wavelength)

df_viz

# Visualize the worst remaining fits.
p <- df_viz %>%
  group_nest(station, r) %>%
  top_n(n = 49, wt = -r) %>%
  unnest(data) %>%
  ggplot(aes(x = wavelength, y = a_cdom_measured)) +
  geom_point(size = 0.5) +
  geom_line(aes(y = a_cdom_modeled), color = "red") +
  facet_wrap(~station, scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM]~(m^{-1})),
    title = "Worst fitted aCDOM spectra",
    subtitle = "I filtered to keep only fits with R2 >= 0.99. These are the worst 49 fits after the filter."
  )

ggsave(
  here("graphs/01_03_worst_fitted_acdom.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 10
)

# Export
df %>%
  unnest(mod_pred) %>%
  select(station,
    wavelength,
    a_cdom_measured = absorption_background_corrected,
    a_cdom_modeled = pred
  ) %>%
  write_csv(here("data/clean/a_cdom.csv"))
