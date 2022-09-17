# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy and model aCDOM spectra. After discussion with Marcel, he
# provided the original (i.e. un-corrected aCDOM spectra) files. Here I am
# converting these from absorbance to absorption. Then I remove the average
# background (683 - 687 nm). Finally, I model the spectra using a simple
# exponential function (see Babin 2003 GJR).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

files <- fs::dir_ls(here("data", "raw", "CDOM"), recurse = TRUE, glob = "*.YSA")

df <- files |>
  enframe(name = NULL, value = "filename") |>
  mutate(filesize = fs::file_size(filename)) |>
  filter(fs::as_fs_bytes(filesize) > 9000) |>
  select(-filesize) |>
  mutate(station = fs::path_ext_remove(fs::path_file(filename)))

df

df <- df |>
  mutate(cruise = str_sub(station, 1, 2))

df <- df |>
  mutate(absorbance = map(filename, ~ data.table::fread(
    .,
    skip = 86,
    col.names = c("wavelength", "absorbance")
  ))) |>
  unnest(absorbance) |>
  select(-filename)

df

# Most CDOM spectra start at 350 nm, some at 300 nm.
df |>
  group_by(station) |>
  filter(wavelength == min(wavelength)) |>
  ggplot(aes(x = wavelength)) +
  geom_histogram()

# Attach metadata ---------------------------------------------------------

stations <- read_csv(here("data", "raw", "tidied", "stations.csv")) |>
  select(station, area)

df <- stations |>
  inner_join(df, by = "station") |>
  arrange(station, wavelength)

# Convert to absorption ---------------------------------------------------

# 10 cm cuvette (0.1 m)

df <- df |>
  mutate(absorption_m1 = (2.303 * absorbance) / 0.1) |>
  select(-absorbance)

# Visualize ---------------------------------------------------------------

p1 <- df |>
  filter(wavelength <= 750) |>
  ggplot(aes(x = wavelength, y = absorption_m1, group = station)) +
  geom_line(size = 0.1) +
  geom_hline(yintercept = 0, lty = 2, size = 0.25, color = "blue") +
  facet_wrap(~cruise, scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM] ~ (m^{
      -1
    })),
    title = quote(bold(Raw ~ a[CDOM] ~ spectra))
  )

# Baseline correction -----------------------------------------------------

# Calculate the average between 683 and 687 nm and subtract it from the spectra.
df <- df |>
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

p2 <- df |>
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
  facet_wrap(~cruise, scales = "free_y") +
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
  here("graphs", "08_raw_and_background_corrected_acdom_spectra.pdf"),
  device = cairo_pdf,
  height = 8,
  width = 10
)

# Model acdom spectra -----------------------------------------------------

df <- df |>
  group_nest(station, cruise, area) |>
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

df <- df |>
  mutate(mod_augmented = map(mod, broom::augment)) |>
  mutate(mod_pred = map2(data, mod, modelr::add_predictions)) |>
  mutate(r2 = map_dbl(
    mod_augmented,
    ~ cor(.$absorption_background_corrected_m1, .$.fitted)^2
  ))

df

# Visualize fitted spectra ------------------------------------------------

df_viz <- df |>
  # slice_sample(n = 10)  |>
  select(station, area, cruise, mod_pred, r2) |>
  unnest(mod_pred) |>
  group_nest(station, r2, keep = TRUE) |>
  arrange(desc(r2))

df_viz

plot_acdom <- function(df) {
  p <- df |>
    ggplot(aes(x = wavelength, y = absorption_background_corrected_m1)) +
    geom_point(color = "gray50") +
    geom_line(aes(y = pred), color = "red") +
    geom_vline(xintercept = c(350, 500), lty = 2, color = "#3c3c3c") +
    scale_x_continuous(breaks = seq(200, 800, by = 50)) +
    labs(
      x = "Wavelength (nm)",
      y = quote(a[CDOM] ~ (lambda)),
      title = glue("{unique(df$station)} ({unique(df$area)}) (R2 = {round(unique(df$r2), digits = 4)})"),
      subtitle = "Fits have been performed between 350 and 500 nm."
    )

  print(p)
}

cairo_pdf(
  here("graphs", "08_fitted_acdom_spectra_ordered_from_best_to_worst_fits.pdf"),
  width = 7,
  height = 5,
  onefile = TRUE
)

walk(df_viz$data, plot_acdom)

dev.off()

# Filter bad spectra ------------------------------------------------------

df |>
  select(area, r2) |>
  ggplot(aes(x = r2)) +
  geom_histogram(bins = 50) +
  facet_wrap(~area, scales = "free") +
  geom_vline(xintercept = 0.95, lty = 2, size = 0.25, color = "red")

min_r2 <- 0.95

# Only keep the best fits
df_filtered <- df |>
  filter(r2 >= min_r2) |>
  unnest(mod_pred) |>
  select(
    station,
    area,
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
  facet_wrap(~ glue("{station}\n{area}"), scales = "free_y") +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM] ~ (m^{
      -1
    })),
    title = "Worst fitted aCDOM spectra",
    subtitle = "I filtered to keep only fits with R2 >= 0.95. These are the worst 36 fits after the filter."
  )

ggsave(
  here("graphs", "08_worst_fitted_acdom_spectra.pdf"),
  device = cairo_pdf,
  width = 12,
  height = 10
)

# Merge with other absorption data ----------------------------------------

df_filtered

df_filtered <- df_filtered |>
  select(-area, -r2)

# This file contains aphy, anap and ap, so let's add acdom to it.
absorption <- read_csv(here("data", "raw", "tidied", "absorption.csv"))

absorption_merged <- absorption |>
  full_join(df_filtered, by = c("station", "wavelength")) |>
  add_count(station, wavelength) |>
  assertr::verify(n == 1) |> # make sure there is only 1 obs per station/wl
  select(-n)

absorption_merged

# Clean absorption spectra ------------------------------------------------

# Remove any spectra if there are negative values below 500 nm

absorption_merged |>
  arrange(station, wavelength)

absorption_clean <- absorption_merged |>
  pivot_longer(starts_with("a_")) |>
  drop_na(value) |>
  group_by(station, name) |>
  filter(all(value[wavelength <= 500] >= 0, na.rm = TRUE)) |>
  ungroup() |>
  pivot_wider(names_from = name, values_from = value) |>
  arrange(station, wavelength)

# Drop stations where no measured absorption. Sometimes, acdom was modeled and
# kept as good (R2 > 0.95), but the measured acdom have negative values below
# 500 nm. We have to remove these as well.

absorption_clean <- absorption_clean |>
  filter(!if_all(a_p_m1:a_cdom_measured_m1, is.na))

write_csv(absorption_clean, here("data", "clean", "absorption.csv"))

# Export s_cdom -----------------------------------------------------------

# Use absorption_clean to keep only good models.

s_cdom <- df |>
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

write_csv(s_cdom, here("data", "clean", "s_cdom.csv"))
