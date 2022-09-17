# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Show couple of vertical profiles of Ed and Eu (SPMR).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

metadata <- read_csv(here("data", "clean", "stations.csv")) |>
  select(-depth_m)

df <- open_dataset(here("data", "clean", "spmr")) |>
  left_join(metadata, by = "station") |>
  collect()

df

df |>
  count(station, depth_m, wavelength) |>
  assertr::verify(n == 1)

df |>
  drop_na(ed_w_m2_um) |>
  ggplot(aes(
    x = ed_w_m2_um,
    y = depth_m,
    color = factor(wavelength),
    group = interaction(station, wavelength)
  )) +
  geom_path() +
  facet_wrap(~station, scales = "free") +
  theme(
    legend.title = element_blank()
  )

ggsave(
  "~/Desktop/spmr.pdf",
  device = cairo_pdf,
  width = 12,
  height = 10
)
