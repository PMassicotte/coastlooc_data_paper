# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  The data that was provided is really in bad shape. I will
# rearrange it so I can better work with it across all scripts.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

file <- here("data", "raw", "SurfaceData5(C4corr).txt")

header_names <- read_lines(file, n_max = 1) |>
  str_split(",") |>
  map(.x = _, ~ str_replace_all(., "/", "_divided_")) |>
  map(.x = _, ~ str_replace_all(., "\\+", "_plus_")) |>
  map(.x = _, ~ str_replace_all(., "\\(([:alpha:]\\d)", "_or_\\1")) |>
  map(.x = _, ~ janitor::make_clean_names(.)) |>
  unlist()

header_names

stations <- data.table::fread(file, col.names = header_names) |>
  as_tibble() |>
  mutate(station = str_to_upper(station)) |>
  mutate(across(where(is.character), ~ noquote(.))) |>
  mutate(date = lubridate::parse_date_time(date, orders = "dmy")) |>
  mutate(date = as.Date(date)) |>
  type_convert()

stations

# Remove Med. Sea (Case 1) stations as they were sampled during Almofront 2.
# According to Frank: ALmofront was a different project, different funding,
# different PI, so I think to remember we decided to leave the Almofront data
# out.
stations <- stations |>
  filter(!str_detect(station, "^A2"))

write_csv(stations, here("data", "raw", "tidied", "surface_data.csv"))
