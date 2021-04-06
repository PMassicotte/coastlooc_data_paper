# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate the distance of each station to the nearest land.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# https://stackoverflow.com/questions/66968669/how-to-calculate-the-distance-to-the-closest-polygon-in-r-with-the-sf-package/66968905?noredirect=1#comment118377299_66968905

rm(list = ls())

stations <- read_csv(here("data/clean/stations.csv")) %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

stations

ne_land <- rnaturalearth::ne_download(
    category = "physical",
    type = "land",
    returnclass = "sf",
    scale = "medium"
  ) %>%
  st_union()

# This is a workaround because some distances were calculated as 0. See the
# stackoverflow question for more information.

# st_nearest_points(stations, ne_land) %>%
#   st_length()

# ggplot() +
#   geom_sf(data = st_nearest_points(stations, ne_land)) +
#   geom_sf(data = ne_land) +
#   geom_sf(data = stations) +
#   coord_sf(xlim = c(2, 5), ylim = c(50, 54))

# ggplot() +
#   geom_sf(data = st_union(ne_land)) +
#   geom_sf(data = stations) +
#   coord_sf(xlim = c(2, 5), ylim = c(50, 54))

# Calculate the distances -------------------------------------------------

stations <- stations %>%
  mutate(distance_to_shore_m = as.vector(st_distance(stations, ne_land)))

# TODO: Some ditances are calculated as 0. Try to figure out why (see
# stackoverflow).

stations %>%
  filter(distance_to_shore_m == 0)

stations %>%
  as_tibble() %>%
  select(date, station, distance_to_shore_m) %>%
  write_csv(here("data/clean/distances_to_shore.csv"))
