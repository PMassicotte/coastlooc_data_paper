# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the IOPs data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Absorption data ---------------------------------------------------------

absorption <- data.table::fread(here("data/raw/all_abs_transpose.txt"),
  na.strings = "9.999900000"
) %>%
  as_tibble() %>%
  pivot_longer(
    -wavelength,
    names_pattern = c("(.*)_(.*)"),
    names_to = c(".value", "station")
  ) %>%
  arrange(station, wavelength) %>%
  relocate(wavelength, .after = station) %>%
  mutate(station = str_to_upper(station))

absorption

# Should have 371 wavelengths per station
absorption %>%
  count(station) %>%
  verify(n == 371)

absorption %>%
  distinct(station)

absorption %>%
  filter(is.na(toa)) %>%
  distinct(station)

# Rename absorption variables

absorption

absorption <- absorption %>%
  rename(
    a_cdom = cdom,
    a_phy = pga,
    a_phy_specific = aph_spe,
    a_nap = dta,
    a_tot = toa
  ) %>%
  relocate(starts_with("a_phy"), .after = wavelength)

absorption

# Stations information ----------------------------------------------------

stations <- data.table::fread(here("data/raw/SurfaceData5(C4corr).txt")) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(station = str_to_upper(station))

stations

stations %>%
  distinct(station)

station_metadata <- stations %>%
  select(date:gm_ttime) %>%
  mutate(across(where(is.character), ~ noquote(.))) %>%
  mutate(date = lubridate::parse_date_time(date, orders = "dmy")) %>%
  mutate(date = as.Date(date)) %>%
  rename(
    depth_m = depth,
    longitude = lon,
    latitude = lat,
    gmt_time = gm_ttime
  ) %>%
  type_convert() %>%
  drop_na(longitude, latitude)

station_metadata %>%
  distinct(depth_m)

station_metadata %>%
  count(area)

station_metadata %>%
  count(area, system)

# Basic tests to ensure thate some of the data is within ranges.
station_metadata %>%
  verify(between(latitude, 0, 90)) %>%
  verify(between(longitude, -20, 20)) %>%
  verify(between(lubridate::year(date), 1997, 1998)) %>%
  verify(depth_m == 0)

# Export the clean metadata
station_metadata %>%
  write_csv(here("data/clean/stations.csv"))

stations <- stations %>%
  select(-c(date, depth, lat, lon, area, system, gm_ttime))

stations

# Other radiometric data --------------------------------------------------

# In this file, there are a lot of radiometric quantities that have been
# measured that need to be tidied. Remove absorption data that were already
# tidied

names(stations)
names(absorption)

stations <- stations %>%
  select(-c(
    contains("cdom"),
    contains("dta"),
    contains("toa"),
    contains("pga"),
    matches("^bp_\\d{3}_bp_\\d{3}$"),
    matches("^\\d{3}_bp_\\d{3}$"),
    contains("for"),
    contains("colonne"),
    matches("^a_nap_\\d{3}_a_nap_\\d{3}$")
  ))

stations

# AC9 data ----------------------------------------------------------------

names(stations)

ac9 <- stations %>%
  select(
    station,
    matches("^a\\d{3}$"),
    matches("^c\\d{3}$"),
    matches("^bp_\\d{3}$"),
    matches("^n\\d{3}$"),
    matches("^ad\\d{3}$"),
    matches("^z1_\\d{3}$"),
    matches("^cd\\d{3}$"),
    matches("^a_tot_10_\\d{3}$")
  ) %>%
  pivot_longer(
    -station,
    names_pattern = c("(.*)_?(\\d{3})"),
    names_to = c(".value", "wavelength"),
    names_transform = list(wavelength = parse_integer)
  ) %>%
  rename_with(~ str_remove_all(., "_$")) %>%
  rename(molecular_to_total_scattering_ratio = n) %>%
  arrange(station, wavelength)

ac9

# Remove ac9 variables, so we can continue to process remaining variables.

stations <- stations %>%
  select(-c(
    matches("^a\\d{3}$"),
    matches("^c\\d{3}$"),
    matches("^bp_\\d{3}$"),
    matches("^n\\d{3}$"),
    matches("^ad\\d{3}$"),
    matches("^z1_\\d{3}$"),
    matches("^cd\\d{3}$"),
    matches("^a_tot_10_\\d{3}$")
  ))

names(stations)

# Reflectance -------------------------------------------------------------

reflectance <- stations %>%
  select(
    station,
    matches("^measured_r"),
    matches("^calculated_r")
  ) %>%
  pivot_longer(
    -station,
    names_pattern = c("(.*\\_r_)(\\d{3})(_.*)"),
    names_to = c(".value", "wavelength", "unit"),
    names_transform = list(wavelength = parse_integer)
  ) %>%
  select(-unit) %>%
  rename_with(~ str_replace_all(., "r_$", "reflectance")) %>%
  mutate(across(c(measured_reflectance, calculated_reflectance), ~ . / 100))

reflectance

# Figure 77 in the final PDF report
reflectance %>%
  filter(wavelength == 705) %>%
  drop_na() %>%
  left_join(station_metadata, by = "station") %>%
  ggplot(aes(x = measured_reflectance, y = calculated_reflectance)) +
  geom_point(aes(color = area)) +
  scale_x_log10(
    labels = scales::label_percent(),
    limits = c(1e-4, 1),
    expand = c(0, 0)
  ) +
  scale_y_log10(
    labels = scales::label_percent(),
    limits = c(1e-4, 1),
    expand = c(0, 0)
  ) +
  annotation_logticks() +
  geom_abline() +
  labs(
    title = "Reflectance at 705 nm"
  )

stations <- stations %>%
  select(-c(
    matches("^measured_r"),
    matches("^calculated_r")
  ))

# Irradiance --------------------------------------------------------------

irradiance <- stations %>%
  select(
    station,
    matches("eu_\\d{3}"),
    matches("ed_\\d{3}"),
    matches("ku_\\d{3}"),
    matches("kd_\\d{3}")
  ) %>%
  pivot_longer(
    -station,
    names_pattern = c("(.*)_(\\d{3})"),
    names_to = c(".value", "wavelength"),
    names_transform = list(wavelength = parse_integer)
  )

irradiance

stations <- stations %>%
  select(-c(
    matches("eu_\\d{3}"),
    matches("ed_\\d{3}"),
    matches("ku_\\d{3}"),
    matches("kd_\\d{3}")
  ))

stations

# Other -------------------------------------------------------------------

# TODO: What to do with ay_443, sy_model, anap_443, b555_spm

# Remove empty rows -------------------------------------------------------

absorption <- absorption %>%
  filter(!if_all(-c(station, wavelength), ~is.na(.)))

ac9 <- ac9 %>%
  filter(!if_all(-c(station, wavelength), ~is.na(.)))

irradiance <- irradiance %>%
  filter(!if_all(-c(station, wavelength), ~is.na(.)))

reflectance <- reflectance %>%
  filter(!if_all(-c(station, wavelength), ~is.na(.)))

write_csv(absorption, here("data/clean/absorption.csv"))
write_csv(ac9, here("data/clean/ac9.csv"))
write_csv(irradiance, here("data/clean/irradiance.csv"))
write_csv(reflectance, here("data/clean/reflectance.csv"))

absorption
ac9
irradiance
reflectance

names(absorption)
names(ac9)
names(irradiance)
names(reflectance)

# Nutrients and phytoplankton ---------------------------------------------

nutrient <- stations %>%
  relocate(contains("chl"), .after = station) %>%
  rename(
    peridinin = peri,
    carotene = car,
    lutein = lut,
    hexanoyloxyfucoxanthin_19 = x19hf,
    butanoyloxyfucoxanthin_19 = x19bf
  ) %>%
  rename_with(
    .fn = ~ glue("{.x}", "xanthin"),
    .cols = c(fuco, allo, zea, neo, viola, diato, diadino, prasi)
  ) %>%
  rename_with(
    ~ str_replace(., "tot_", "total_")
  )

names(nutrient)

# Remove ratio variables

# TODO: Validate with Marcel
nutrient %>%
  names()
