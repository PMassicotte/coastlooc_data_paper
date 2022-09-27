# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract the bathymetry at each station.
#               Bathymetry data: https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

stations <- read_csv(here("data", "clean", "stations.csv")) |>
  drop_na(longitude, latitude) |>
  select(station, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  vect()

r <- rast(
  here(
    "data",
    "raw",
    "bathymetry",
    "GEBCO_2020_10_Feb_2021_68bf33d96a7a",
    "gebco_2020_n70.0_s20.0_w-30.0_e30.0.tif"
  )
)

bathy <- terra::extract(r, stations, xy = TRUE) |>
  as_tibble() |>
  select(
    longitude = x,
    latitude = y,
    bathymetry_m = 2
  )

bathymetry <- bind_cols(as_tibble(stations), bathy)

bathymetry

bathymetry |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  ggplot() +
  geom_sf(aes(color = as.vector(bathymetry_m) >= 0))

# Remove positive bathymetry values

bathymetry <- bathymetry |>
  mutate(bathymetry_m = na_if(bathymetry_m, bathymetry_m > 0))

write_csv(bathymetry, here("data", "clean", "bathymetry.csv"))
