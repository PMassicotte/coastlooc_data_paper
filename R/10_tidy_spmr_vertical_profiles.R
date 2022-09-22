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

destdir_csv <- here("data/clean/spmr/csv/")

ds |>
  group_by(station) |>
  collect() |>
  relocate(wavelength, .before = eu_w_m2_um) |>
  arrange(-depth_m, wavelength) |>
  group_walk(~ write_csv(.x, file = file.path(destdir_csv, paste0(.y$station, ".csv"))))
# %%
