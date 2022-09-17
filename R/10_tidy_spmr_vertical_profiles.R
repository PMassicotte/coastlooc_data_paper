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

export_parquet <- function(file) {
  read_mrg(file) |>
    tidy_mrg() |>
    write_dataset(here("data", "clean", "spmr"),
      partitioning = c("station", "wavelength")
    )
}

walk(files, export_parquet)

open_dataset(here("data", "clean", "spmr")) |>
  head(1) |>
  collect()
