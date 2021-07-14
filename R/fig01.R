# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Map of the sampling locations.
# Bathymetry data: https://download.gebco.net/
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")

stations <- read_csv(here("data", "clean", "stations.csv"))

stations

map_crs <- 3035

stations_sf <- stations %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(map_crs)

st_bbox(stations_sf)

bbox_4326 <- st_bbox(c(
  xmin = -20,
  xmax = 25,
  ymax = 60,
  ymin = 25
), crs = st_crs(4326))

stations_sf %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf()

# Land data ---------------------------------------------------------------

ne_land <-
  rnaturalearth::ne_download(
    category = "physical",
    type = "land",
    returnclass = "sf",
    scale = "large"
  ) %>%
  st_crop(bbox_4326) %>%
  st_transform(map_crs)

# Country data ------------------------------------------------------------

ne_country <-
  rnaturalearth::ne_countries(
    returnclass = "sf",
    scale = "large"
  ) %>%
  st_crop(bbox_4326) %>%
  st_transform(map_crs)

# River network -----------------------------------------------------------

ne_river <- rnaturalearth::ne_download(
  scale = "large",
  category = "physical",
  type = "rivers_lake_centerlines",
  returnclass = "sf"
) %>%
  st_crop(bbox_4326) %>%
  st_transform(map_crs)

# Prepare bathymetry data -------------------------------------------------

bathy <- raster::raster(
  "data/raw/bathymetry/GEBCO_2020_10_Feb_2021_68bf33d96a7a/gebco_2020_n70.0_s20.0_w-30.0_e30.0.tif"
) %>%
  raster::sampleRegular(size = 1e5, asRaster = TRUE) %>%
  raster::projectRaster(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") %>%
  raster::rasterToPoints() %>%
  as_tibble() %>%
  rename(z = 3)

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z)) %>%
  drop_na()

range(bathy_interpolated$xyz.est.z, na.rm = TRUE)

# Plot --------------------------------------------------------------------

# Projection: https://epsg.io/2154

st_bbox(stations_sf)

p1 <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated,
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 20, color = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-4000, 0),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = "Depth (m)",
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(face = "bold", size = 8, family = "Open Sans"),
      label.theme = element_text(size = 6, family = "Open Sans"),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.75, "cm"),
      byrow = TRUE,
      nrow = 1
    ),
    breaks = -seq(0, 8000, by = 500)
  ) +
  geom_sf(data = ne_land, size = 0.1, fill = "gray85") +
  geom_sf(data = ne_river, size = 0.1, color = "gray70") +
  geom_sf(data = ne_country, size = 0.1, color = "#3c3c3c", fill = NA) +
  geom_sf(
    data = stations_sf,
    size = 0.5,
    key_glyph = "rect",
    aes(color = area)
  ) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors,
    guide = guide_legend(
      title = element_blank(),
      label.position = "top",
      label.theme = element_text(
        size = 6,
        margin = margin(b = unit(-2, "cm")),
        family = "Open Sans"
      ),
      nrow = 1,
      keywidth = unit(2, "cm"),
      keyheight = unit(0.2, "cm")
    )
  ) +
  ggspatial::annotation_north_arrow(
    which_north = "true",
    location = "tl",
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      text_size = 6,
      line_width = 0.1,
      line_col = "white",
      text_col = "white",
      text_family = "Open Sans"
    )
  ) +
  ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.25,
    height = unit(0.1, "cm"),
    line_width = 0.25,
    text_family = "Open Sans"
  ) +
  coord_sf(
    xlim = c(1680000, 5100000),
    ylim = c(979000, 3750000),
    expand = FALSE,
    crs = map_crs
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_line(size = 0.1),
    legend.position = "top",
    legend.box = "vertical",
    legend.background = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "#B9DDF1"),
    axis.title = element_blank(),
    plot.margin = margin(b = 0)
  )

# Plot per area -----------------------------------------------------------

df_viz <- stations_sf %>%
  filter(area != "Atlantic Ocean") %>%
  group_by(area) %>%
  summarise(geometry = st_union(geometry)) %>%
  group_nest(area, keep = TRUE) %>%
  mutate(p = map(data, function(df, land = ne_land, river = ne_river) {

    bbox <- st_bbox(st_buffer(df, 50000))
    land <- st_crop(land, bbox)
    river <- st_crop(river, bbox)

    p <- ggplot() +
      geom_sf(data = land, size = 0.1, fill = "gray85", color = "gray85") +
      geom_sf(data = river, size = 0.1, color = "gray70") +
      geom_sf(
        data = df,
        size = 0.5,
        key_glyph = "rect",
        aes(color = area)
      ) +
      scale_color_manual(
        breaks = area_breaks,
        values = area_colors
      ) +
      labs(
        title = df$area
      ) +
      theme(
        legend.position = "none",
        aspect.ratio = 1,
        panel.grid = element_blank(),
        plot.title = element_text(size = 8, hjust = 0.5),
        axis.text = element_text(size = 4),
        panel.border = element_rect(size = 0.25, fill = NA)
      )
  }))

p2 <- wrap_plots(df_viz$p, ncol = 3)

p2

# Combine plots -----------------------------------------------------------

p <- p1 / p2

outfile <- here("graphs", "fig01.pdf")

ggsave(
  outfile,
  device = cairo_pdf,
  width = 12,
  height = 12
)

knitr::plot_crop(outfile)

