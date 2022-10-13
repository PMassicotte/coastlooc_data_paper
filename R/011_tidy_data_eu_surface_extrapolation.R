# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  This data was provided as is in an email. This is my attempt to
# tidy it to produce an appendix showing to process of extrapolating measured Eu
# to the surface (Eu0-).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

dat <- read_lines(here("data", "raw", "appendix1")) |>
  str_squish() |>
  str_split("\\s+")

dat

dat <- dat[dat != ""]
dat <- dat[map_int(dat, length) != 2]

n <- paste(
  rep(c("eu", "depth", "c"), times = 4),
  rep(c(443, 559, 665, 779), each = 3),
  sep = "_"
)

dat <- set_names(dat, n)

df <- map(dat, enframe) |>
  bind_rows(.id = "wave") |>
  separate(wave, into = c("var", "wavelength"), sep = "_") |>
  pivot_wider(names_from = var, values_from = value)

params <- df |>
  select(wavelength, c) |>
  drop_na() |>
  group_by(wavelength) |>
  mutate(param = paste0("c", 1:n())) |>
  pivot_wider(names_from = param, values_from = c)

df <- df |>
  left_join(params) |>
  select(-c) |>
  mutate(across(where(is.character), parse_number)) |>
  mutate(depth = -depth) |>
  select(-name) |>
  mutate(station = "C3004000", .before = 1) |>
  relocate(depth, .after = wavelength) |>
  arrange(wavelength, depth)

df

# The fit function id of the form: y = c1 * EXP (-c2*z) + c3 * EXP (-c4*z)
# EU (0-) = c1 + c3 (with c1 indicating the Raman contribution).

df <- df |>
  mutate(
    predicted_eu = c1 * exp(-c2 * depth) + c3 * exp(-c4 * depth),
    .after = eu
  )

df

write_csv(df, here("data", "clean", "appendix01_eu_surface_extrapolation.csv"))
