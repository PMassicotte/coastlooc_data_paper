# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a file with all station metadata.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

stations <- read_csv(here("data", "raw", "tidied", "surface_data.csv"))

stations <- stations |>
  select(date:gm_ttime, solar_zenith_angle = theta_s) |>
  rename(
    depth_m = depth,
    longitude = lon,
    latitude = lat,
    gmt_time = gm_ttime
  )

# Remove stations without geographical coordinates.
stations <- stations |>
  drop_na(longitude, latitude)

stations

# Basic tests to ensure that some of the data is within ranges.
stations <- stations |>
  assert(within_bounds(0, 90), latitude) |>
  assert(within_bounds(-20, 20), longitude) |>
  verify(between(lubridate::year(date), 1997, 1998)) |>
  verify(depth_m == 0)

stations

# %% ---- Look for stations located on land
stations_sf <- stations |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

stations_sf

wm <- ne_countries(scale = "large", returnclass = "sf") |>
  st_crop(stations_sf)

# Find a proper crs for the area
proj <- crsuggest::suggest_crs(stations_sf) |>
  pull(crs_code) |>
  head(1) |>
  as.numeric()

stations_sf <- stations_sf |>
  st_transform(crs = proj)

wm <- wm |>
  st_transform(crs = proj)

ggplot() +
  geom_sf(data = wm) +
  geom_sf(data = stations_sf)

# Find the stations that overlap with the land
i <- st_contains(wm, stations_sf)

stations_on_land <- stations_sf[unlist(i), ]

wm2 <- wm |>
  st_crop(stations_on_land |> st_buffer(1e5))

# It is obvious that some stations have bad geographical coordinates. However, I
# am not sure for some others. For example, check the C6xxx series. Are they
# located on the land or on small rivers?
ggplot() +
  geom_sf(data = wm2) +
  geom_sf(data = stations_on_land, color = "red") +
  geom_sf_text(data = stations_on_land, aes(label = station))

# Using mapview, these two are really on land. Others seems to be located close
# to the those. I will keep them.
# - C6004000
# - C3037000 mapview::mapview(stations_on_land)

stations_clean <- stations |>
  filter(!station %in% c("C6004000", "C3037000"))
# %%

# %% ---- Save the cleaned data
stations_clean |>
  write_csv(here("data", "clean", "stations.csv"))
# %%
