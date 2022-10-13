# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the SPMR vertical profiles data given by Frank Fell. The
# data is exported into arrow/parquet format for speed.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz_radiometer.R"))

files <- fs::dir_ls(here("data", "raw", "spmr_vertical_profiles", "v6"),
  glob = "*.mrg"
)

destdir <- here("data", "clean", "spmr", "parquet")

if (fs::dir_exists(destdir)) {
  fs::dir_delete(destdir)
}

export_parquet <- function(file) {
  read_mrg(file) |>
    tidy_mrg() |>
    write_dataset(destdir, partitioning = c("station", "wavelength"))
}

walk(files, export_parquet)

# %% ---- Export to CSVs (for the database)
ds <- open_dataset(destdir)

destdir_csv <- here("data", "clean", "spmr", "csv")

if (!fs::dir_exists(destdir_csv)) {
  fs::dir_create(destdir_csv)
}

ds |>
  group_by(station) |>
  collect() |>
  relocate(wavelength, .before = eu_w_m2_um) |>
  arrange(-depth_m, wavelength) |>
  group_walk(~ write_csv(.x,
    file = file.path(
      destdir_csv,
      paste0(.y$station, ".csv")
    )
  ))
# %%

# %% ---- Depth ranges visualization

# Get the min/max depth sampled at each station
df <- ds |>
  group_by(station) |>
  collect() |>
  drop_na(ed_w_m2_um) |>
  select(station, depth_m) |>
  filter(depth_m == min(depth_m) | depth_m == max(depth_m)) |>
  distinct() |>
  mutate(position = case_when(
    depth_m == min(depth_m) ~ "depth",
    TRUE ~ "surface"
  )) |>
  ungroup() |>
  pivot_wider(names_from = position, values_from = depth_m) |>
  mutate(station = fct_reorder(station, depth))

df

# Segment plot
df |>
  ggplot(aes(y = surface, yend = depth, x = station, xend = station)) +
  geom_segment(size = 3, lineend = "round") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 20)) +
  scale_x_discrete(position = "top") +
  labs(
    x = NULL,
    y = "Depth (m)",
    title = "Depth ranges of SPMR vertical profiles"
  )

# %%

ds

ds |>
  collect() |>
  ggplot(aes(x = salin_psu)) +
  geom_histogram()

# TODO: Looks like the NA value for salinity is... -66... wth...
ds |>
  collect() |>
  filter(salin_psu != -66) |>
  pull(salin_psu) |>
  range(na.rm = TRUE)
