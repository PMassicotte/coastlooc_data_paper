# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy pigments data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

stations <- read_csv(here("data", "raw", "tidied", "surface_data.csv"))

stations

pigments <- stations |>
  select(
    station,
    chlorophyll_a = tchla,
    pheopigment = tphaeo,
    matches("^t_chl([bc])"),
    peridinin = peri,
    caroten = car,
    lutein = lut,
    hexanoyloxyfucoxanthin_19 = x19hf,
    butanoyloxyfucoxanthin_19 = x19bf,
    fucoxanthin = fuco,
    alloxanthin = allo,
    zeaxanthin = zea,
    neoxanthin = neo,
    violaxanthin = viola,
    diatoxanthin = diato,
    diadinoxanthin = diadino,
    prasixanthin = prasi
  ) |>
  rename_with(
    .fn = ~ str_replace_all(., "t_chl([abc])", "chlorophyll_\\1"),
    everything()
  ) |>
  rename_with(
    .fn = ~ paste0(., "_mg_m3"),
    -station
  )

pigments

names(pigments)

pigments <- pigments |>
  relocate(contains("chloro"), .after = station) |>
  relocate(pheopigment_mg_m3, .after = station)

# %% ---- Export data
stations <- read_csv(here("data", "clean", "stations.csv")) |>
  select(station)

pigments <- pigments |>
  semi_join(stations, by = "station")

pigments

write_csv(pigments, here("data", "clean", "pigments.csv"))
# %%
