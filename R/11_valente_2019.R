# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the data published in Valente 2019 to make sure that
# COASTLOOC data is not present.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <-
  data.table::fread(
    here(
      "data",
      "raw",
      "valente_2019",
      "Orginal_files_including_metadata",
      "insitudb_iopskdtsm_2019-02.csv"
    )
  ) |>
  as_tibble() |>
  distinct(time, lat, lon)

df

df_sf <- df |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

df_sf |>
  ggplot() +
  geom_sf()

bbox_4326 <- st_bbox(c(
  xmin = -20,
  xmax = 25,
  ymax = 25,
  ymin = 60
), crs = st_crs(4326))

df_sf <- df_sf |>
  st_crop(bbox_4326)

df_sf |>
  ggplot() +
  geom_sf()

ne_land <-
  rnaturalearth::ne_download(
    category = "physical",
    type = "land",
    returnclass = "sf",
    scale = "large"
  ) |>
  st_crop(bbox_4326)

map_crs <- 3035

# Get the COASTLOOC data --------------------------------------------------

coastlooc <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station, area, longitude, latitude) |>
  drop_na(longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(map_crs)


# Plot --------------------------------------------------------------------

p <- df_sf |>
  ggplot() +
  geom_sf(data = ne_land, size = 0.1) +
  geom_sf(data = coastlooc, size = 0.1, color = "red") +
  geom_sf(size = 0.5) +
  coord_sf(
    xlim = c(1680000, 5100000),
    ylim = c(979000, 3750000),
    expand = FALSE,
    crs = map_crs
  ) +
  facet_wrap(~ lubridate::year(time)) +
  labs(
    title = "Map with Valente et al. (2019)",
    subtitle = "There are no overlaps with the COASTLOOC data (in red)."
  )

file <- here("graphs", "11_map_valente_et_al.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 8,
  height = 8
)

knitr::plot_crop(file)
