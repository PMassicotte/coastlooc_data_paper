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

# Visualize bathymetry differences among area -----------------------------

stations <- read_csv(here("data", "clean", "stations.csv"))
bathymetry <- read_csv(here("data", "clean", "bathymetry.csv"))

df_viz <- bathymetry |>
  inner_join(stations, by = "station")

df_viz

p <- df_viz |>
  mutate(area = fct_reorder(area, bathymetry_m)) |>
  ggplot(aes(x = area, y = -bathymetry_m, fill = area)) +
  geom_boxplot(size = 0.1, outlier.size = 1) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  annotation_logticks(sides = "l", size = 0.25) +
  labs(
    x = NULL,
    y = "Bathymetry (m)"
  ) +
  theme(legend.position = "none")

ggsave(
  here("graphs", "17_boxplot_bathymetry_by_area.pdf"),
  device = cairo_pdf,
  width = 6.91,
  height = 5.2
)

# Average depth across all the station
mean(df_viz$bathymetry_m, na.rm = TRUE)
range(df_viz$bathymetry_m[df_viz$bathymetry_m < 0], na.rm = TRUE)

df_viz |>
  group_by(area) |>
  summarise(bathymetry = round(mean(bathymetry_m, na.rm = TRUE))) |>
  arrange(bathymetry)
