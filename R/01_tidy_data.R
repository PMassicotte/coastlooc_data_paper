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

file <- here("data/raw/SurfaceData5(C4corr).txt")

header_names <- read_lines(file, n_max = 1) %>%
  str_split(",") %>%
  map(., ~ str_replace_all(., "/", "_divided_")) %>%
  map(., ~ str_replace_all(., "\\+", "_plus_")) %>%
  map(., ~ str_replace_all(., "\\(([:alpha:]\\d)", "_or_\\1")) %>%
  map(., ~ janitor::make_clean_names(.)) %>%
  unlist()

header_names

stations <- data.table::fread(file, col.names = header_names) %>%
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
  type_convert()

station_metadata %>%
  distinct(depth_m)

station_metadata %>%
  count(area)

station_metadata %>%
  count(area, system)

# Basic tests to ensure thate some of the data is within ranges.
station_metadata %>%
  assert(within_bounds(0, 90), latitude) %>%
  assert(within_bounds(-20, 20), longitude) %>%
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
    matches("cdom_\\d{3}$"),
    matches("dta_\\d{3}$"),
    matches("toa_\\d{3}$"),
    matches("pga_\\d{3}$"),
    matches("^n\\d{3}$"),
    contains("_divided_"),
    contains("_plus_back"),
    contains("_plus_pheo"),
    contains("colonne"),
    contains("ternary"),
    contains("trees"),
    contains("tress"),
    contains("total"),
    contains("calculated"),
    contains("massimo"),
    starts_with("tot_"),
    starts_with("a_tot_"),
    all_of(c("percent_pico", "percent_nano", "percent_micro"))
  ))

stations

# AC9 data ----------------------------------------------------------------

names(stations)

ac9 <- stations %>%
  select(
    station,
    matches("^a\\d{3}$"),
    matches("^c\\d{3}$"),
    matches("^c\\d{3}_or_"),
    matches("^a\\d{3}_or_"),
    matches("^bp_\\d{3}$"),
    matches("^ad\\d{3}$"),
    matches("^z1_\\d{3}$"),
    matches("^cd\\d{3}$")
  ) %>%
  rename_with(~str_remove(., "_or.*"), contains("_for_")) %>%
  pivot_longer(
    -station,
    names_pattern = c("(.*)_?(\\d{3})"),
    names_to = c(".value", "wavelength"),
    names_transform = list(wavelength = parse_integer)
  ) %>%
  rename_with(~ str_remove_all(., "_$")) %>%
  rename(
    a_dissolved = ad,
    c_dissolved = cd,
    remote_sensed_vertical_layer_meter = z1
  ) %>%
  relocate(c_dissolved, .after = a_dissolved)

ac9

# Should have only 1 observation per wavelength
ac9 %>%
  count(station, wavelength) %>%
  verify(n == 1)

# Some wavelengths were not exactly the same on the C6 cruise because the
# radiometer had a different configuration.

ac9 <- ac9 %>%
  mutate(wavelength = case_when(
    str_starts(station, "C6") & wavelength == 555 ~ 532L,
    str_starts(station, "C6") & wavelength == 630 ~ 555L,
    str_starts(station, "C6") & wavelength == 650 ~ 630L,
    TRUE ~ wavelength
  ))

# Should have only 1 observation per wavelength
ac9 %>%
  count(station, wavelength) %>%
  verify(n == 1)

# Remove ac9 variables, so we can continue to process remaining variables.
stations <- stations %>%
  select(-c(
    matches("^a\\d{3}$"),
    matches("^c\\d{3}$"),
    matches("^c\\d{3}_or_"),
    matches("^a\\d{3}_or_"),
    matches("^bp_\\d{3}$"),
    matches("^ad\\d{3}$"),
    matches("^z1_\\d{3}$"),
    matches("^cd\\d{3}$")
  ))

names(stations)

# Reflectance -------------------------------------------------------------

reflectance <- stations %>%
  select(
    station,
    matches("^measured_r")
  ) %>%
  pivot_longer(
    -station,
    names_pattern = c("(.*\\_r_)(\\d{3})(_.*)"),
    names_to = c(".value", "wavelength", "unit"),
    names_transform = list(wavelength = parse_integer)
  ) %>%
  select(-unit) %>%
  rename_with(~ str_replace_all(., "r_$", "reflectance")) %>%
  mutate(across(c(measured_reflectance), ~ . / 100))

reflectance

stations <- stations %>%
  select(-c(
    matches("^measured_r"),
    matches("^calculated_r")
  ))

names(stations)

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

names(stations)

# Other -------------------------------------------------------------------

# TODO: What to do with ay_443, sy_model, anap_443, b555_spm

# Remove empty rows -------------------------------------------------------

absorption <- absorption %>%
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

ac9 <- ac9 %>%
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

irradiance <- irradiance %>%
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

reflectance <- reflectance %>%
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

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

names(stations)

# pigments and phytoplankton ---------------------------------------------

pigment <- stations %>%
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
  rename_with(~str_replace(., "_pga$", "_phy")) %>%
  rename_with(~str_replace(., "_dta$", "_nap")) %>%
  rename_with(~str_replace(., "_toa$", "_tot")) %>%
  rename_with(everything(), .fn = ~ str_replace_all(., "chl([abc])", "chl_\\1")) %>%
  rename_with(everything(), .fn = ~ str_replace_all(., "tchl", "total_chl")) %>%
  rename_with(everything(), .fn = ~ str_replace_all(., "^t_", "total_")) %>%
  rename(
    a_cdom_443_model = ay_443_model,
    s_cdom_model = sy_model,
    average_baseline_cdom_model = y_model_intercept,
    a_nap_443_model = anap_443_model,
    s_nap_model = snap_model,
    average_baseline_nap_model = nap_model_intercept,
    fluorescence_line_height = flh,
    solar_zenith_angle = theta_s,
    total_phaeo = tphaeo
  )

names(pigment)

pigment <- pigment %>%
  relocate(contains("model"), .after = last_col()) %>%
  relocate(contains("total"), .after = contains("chl")) %>%
  relocate(contains("xanthin"), .after = contains("total"))

write_csv(pigment, here("data/clean/pigment.csv"))
