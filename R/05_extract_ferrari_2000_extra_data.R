# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract data from Ferrari 2000.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

file <- here("man", "Ferrari_2000.pdf")

df <- tabulizer::extract_tables(file,
  pages = 14:18,
  method = "decide"
)

df

# Calculate how many columns each table has.

i <- map_int(df, ncol)

# Tidy every table with 7 columns.

df1 <- df[i == 7] |>
  do.call(rbind, args = _) |>
  as_tibble() |>
  filter(str_detect(V1, "VH\\d{2}|The\\d{2}")) |>
  set_names(c(
    "station",
    "depth_m",
    "a_cdom_350",
    "s_cdom",
    "aqy",
    "doc_um",
    "salinity"
  )) |>
  mutate(across(where(is.character), ~ na_if(., "n.m."))) |>
  type_convert()

# Tidy every table with 6 columns.

df2 <- df[i == 6] |>
  do.call(rbind, args = _) |>
  as_tibble() |>
  filter(str_detect(V1, "VH\\d{2}|The\\d{2}")) |>
  set_names(c(
    "station",
    "depth_m",
    "a_cdom_350",
    "s_cdom",
    "doc_um",
    "salinity"
  )) |>
  mutate(across(where(is.character), ~ na_if(., "n.m."))) |>
  type_convert()

# Keep only the surface values because we only have such data in the main
# dataset.

massimo <- bind_rows(df1, df2) |>
  filter(depth_m == 0) |>
  select(-depth_m)

massimo

# Export data -------------------------------------------------------------

surface <- read_csv(here("data", "clean", "surface.csv"))

surface

# Rename the stations found in Ferrari so they match those of Marcel's data.
massimo <- massimo |>
  mutate(station = str_replace(station, "VH", "C10")) |>
  mutate(station = str_replace(station, "The", "C40")) |>
  mutate(station = str_pad(
    station,
    width = 8,
    side = "right",
    pad = "0"
  ))

massimo

# Remove variables that are already included in Marcel's data.
massimo <- massimo |>
  select(-contains("cdom"))

massimo |>
  anti_join(surface, by = "station") |>
  verify(nrow(.) == 0)

df <- surface |>
  left_join(massimo, by = "station")

df

df |>
  write_csv(here("data", "clean", "surface.csv"))
