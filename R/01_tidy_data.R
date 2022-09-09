# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the IOPs data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Absorption data ---------------------------------------------------------

absorption <- data.table::fread(here("data", "raw", "all_abs_transpose.txt"),
  na.strings = "9.999900000"
) |>
  as_tibble() |>
  pivot_longer(
    -wavelength,
    names_pattern = c("(.*)_(.*)"),
    names_to = c(".value", "station")
  ) |>
  arrange(station, wavelength) |>
  relocate(wavelength, .after = station) |>
  mutate(station = str_to_upper(station))

absorption

# Should have 371 wavelengths per station
absorption |>
  count(station) |>
  verify(n == 371)

# How many station do we have?
absorption |>
  distinct(station)

absorption |>
  filter(is.na(toa)) |>
  distinct(station)

# Rename absorption variables

absorption

absorption <- absorption |>
  rename(
    a_phy_m1 = pga,
    a_phy_specific_m2_mg_chla_m1 = aph_spe,
    a_nap_m1 = dta,
    a_p_m1 = toa
  ) |>
  relocate(a_p_m1, .after = wavelength) |>
  relocate(starts_with("a_phy"), .after = contains("a_phy"))

absorption

# Remove cdom data, will use the "raw" data with the complete range of
# wavelengths.

absorption <- absorption |>
  select(-contains("cdom"))

# Stations information ----------------------------------------------------

file <- here("data", "raw", "SurfaceData5(C4corr).txt")

header_names <- read_lines(file, n_max = 1) |>
  str_split(",") |>
  map(.x = _, ~ str_replace_all(., "/", "_divided_")) |>
  map(.x = _, ~ str_replace_all(., "\\+", "_plus_")) |>
  map(.x = _, ~ str_replace_all(., "\\(([:alpha:]\\d)", "_or_\\1")) |>
  map(.x = _, ~ janitor::make_clean_names(.)) |>
  unlist()

header_names

stations <- data.table::fread(file, col.names = header_names) |>
  as_tibble() |>
  mutate(station = str_to_upper(station))

stations

stations |>
  distinct(station)

station_metadata <- stations |>
  select(date:gm_ttime) |>
  mutate(across(where(is.character), ~ noquote(.))) |>
  mutate(date = lubridate::parse_date_time(date, orders = "dmy")) |>
  mutate(date = as.Date(date)) |>
  rename(
    depth_m = depth,
    longitude = lon,
    latitude = lat,
    gmt_time = gm_ttime
  ) |>
  type_convert()

# Ok we only have surface data (i.e. depth = 0 meter)
station_metadata |>
  distinct(depth_m)

station_metadata |>
  count(area)

station_metadata |>
  count(area, system)

# Basic tests to ensure that some of the data is within ranges.
station_metadata |>
  assert(within_bounds(0, 90), latitude) |>
  assert(within_bounds(-20, 20), longitude) |>
  verify(between(lubridate::year(date), 1997, 1998)) |>
  verify(depth_m == 0)

# Export the clean metadata
station_metadata |>
  write_csv(here("data", "clean", "stations.csv"))

stations <- stations |>
  select(-c(date, depth, lat, lon, area, system, gm_ttime))

stations

# Other radiometric data --------------------------------------------------

# In this file, there are a lot of radiometric quantities that have been
# measured that need to be tidied. Remove absorption data that were already
# tidied.

names(stations)
names(absorption)

stations <- stations |>
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

ac9 <- stations |>
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
  ) |>
  rename_with(~ str_remove(., "_or.*"), contains("_for_")) |>
  pivot_longer(
    -station,
    names_pattern = c("(.*)_?(\\d{3})"),
    names_to = c(".value", "wavelength"),
    names_transform = list(wavelength = parse_integer)
  ) |>
  rename_with(~ str_remove_all(., "_$")) |>
  select(-ad, -cd) |>
  rename(remote_sensed_vertical_layer_m = z1) |>
  rename_with(~ paste0(., "_m1"), c(a, c, bp))

ac9

# Should have only 1 observation per wavelength
ac9 |>
  count(station, wavelength) |>
  verify(n == 1)

# Some wavelengths were not exactly the same on the C6 cruise because the
# radiometer had a different configuration.

ac9 <- ac9 |>
  mutate(wavelength = case_when(
    str_starts(station, "C6") & wavelength == 555 ~ 532L,
    str_starts(station, "C6") & wavelength == 630 ~ 555L,
    str_starts(station, "C6") & wavelength == 650 ~ 630L,
    TRUE ~ wavelength
  ))

# Should have only 1 observation per wavelength
ac9 |>
  count(station, wavelength) |>
  verify(n == 1)

# Remove ac9 variables, so we can continue to process remaining variables.
stations <- stations |>
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

reflectance <- stations |>
  select(
    station,
    matches("^measured_r")
  ) |>
  pivot_longer(
    -station,
    names_pattern = c("(.*\\_r_)(\\d{3})(_.*)"),
    names_to = c(".value", "wavelength", "unit"),
    names_transform = list(wavelength = parse_integer)
  ) |>
  select(-unit) |>
  rename_with(~ str_replace_all(., "r_$", "reflectance")) |>
  mutate(across(c(measured_reflectance), ~ . / 100))

reflectance

stations <- stations |>
  select(-c(
    matches("^measured_r"),
    matches("^calculated_r")
  ))

names(stations)

# Add unit to reflectance variable

reflectance <- reflectance |>
  rename(measured_reflectance_percent = measured_reflectance)

# Irradiance --------------------------------------------------------------

irradiance <- stations |>
  select(
    station,
    matches("eu_\\d{3}"),
    matches("ed_\\d{3}"),
    matches("ku_\\d{3}"),
    matches("kd_\\d{3}")
  ) |>
  pivot_longer(
    -station,
    names_pattern = c("(.*)_(\\d{3})"),
    names_to = c(".value", "wavelength"),
    names_transform = list(wavelength = parse_integer)
  )

irradiance

# Add units
irradiance <- irradiance |>
  rename_with(~ paste0(., "_w_m2_um"), c(ed, eu)) |>
  rename_with(~ paste0(., "_m1"), c(kd, ku))

stations <- stations |>
  select(-c(
    matches("eu_\\d{3}"),
    matches("ed_\\d{3}"),
    matches("ku_\\d{3}"),
    matches("kd_\\d{3}")
  ))

stations

names(stations)

# Remove empty rows -------------------------------------------------------

absorption <- absorption |>
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

ac9 <- ac9 |>
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

irradiance <- irradiance |>
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

reflectance <- reflectance |>
  filter(!if_all(-c(station, wavelength), ~ is.na(.)))

# Write these file for now. I will come back later for quality controle (ex.:
# removing/flagin negative values). At the moment, I just want to split the data
# in their own file.

write_csv(absorption, here("data", "raw", "tidied", "absorption.csv"))
write_csv(ac9, here("data", "raw", "tidied", "ac9.csv"))
write_csv(irradiance, here("data", "raw", "tidied", "irradiance.csv"))
write_csv(reflectance, here("data", "raw", "tidied", "reflectance.csv"))

absorption
ac9
irradiance
reflectance

names(absorption)
names(ac9)
names(irradiance)
names(reflectance)

names(stations)

# surface and phytoplankton ----------------------------------------------

surface <- stations |>
  relocate(contains("chl"), .after = station) |>
  rename(
    peridinin = peri,
    caroten = car,
    lutein = lut,
    hexanoyloxyfucoxanthin_19 = x19hf,
    butanoyloxyfucoxanthin_19 = x19bf
  ) |>
  rename_with(
    .fn = ~ glue("{.x}", "xanthin"),
    .cols = c(fuco, allo, zea, neo, viola, diato, diadino, prasi)
  ) |>
  rename(
    background_a_cdom_average_683_687_m1 = y_model_intercept,
    background_a_phy_average_746_750_m1 = back_pga,
    background_a_nap_average_746_750_m1 = back_dta,
    background_a_tot_average_746_750_m1 = back_toa
  ) |>
  rename_with(everything(),
    .fn = ~ str_replace_all(., "chl([abc])", "chl_\\1")
  ) |>
  rename_with(everything(),
    .fn = ~ str_replace_all(., "tchl", "total_chl")
  ) |>
  rename_with(everything(),
    .fn = ~ str_replace_all(., "^t_", "total_")
  ) |>
  rename(
    a_cdom_443_model_m1 = ay_443_model,
    s_cdom_350_500_model_m1 = sy_model,
    a_nap_443_model_m1 = anap_443_model,
    s_nap_model_nm1 = snap_model,
    fluorescence_line_height = flh,
    solar_zenith_angle = theta_s,
    total_phaeo = tphaeo
  )

names(surface)

# Found out that "nap_model_intercept" was very similar to "back_dta"
# ("background_a_nap_average_746_750"). I decided to remove it from the data to
# avoid possible confusion.

surface <- surface |>
  select(-nap_model_intercept)

# Let's remove all CDOM data because Marcel provided me with the original acdom
# files with a larger spectral range. I will use this data in the final data.
# Also remove absorption background average calculated between 746 and 750.
# These are not needed since I am computing them later when cleaning absorption
# data.

# I made a plot to compare s_nap from Marcel and the one I am calculating and
# they are very close.

surface <- surface |>
  select(
    -contains("cdom"),
    -contains("background"),
    -contains("a_nap"),
    -contains("s_nap")
  )

surface <- surface |>
  relocate(contains("model"), .after = last_col()) |>
  relocate(contains("background"), .after = last_col()) |>
  relocate(contains("total"), .after = contains("chl")) |>
  relocate(contains("xanthin"), .after = contains("total"))

names(surface)

# There is one very low POC value below 0.01, remove it

surface <- surface |>
  mutate(poc_g_m_3 = replace(poc_g_m_3, poc_g_m_3 < 0.01, NA_real_))

## ├ Rename units ----

surface <- surface |>
  rename_with(~ str_replace(., "m_3$", "m3"), ends_with("m_3"))

# TODO: add units to each parameter
surface |>
  rename(spm_g_m3 = spm) |>
  rename_with(~ paste0(., "_mg_m3"), matches("chl_[abc]")) |>
  rename_with(~ paste0(., "_mg_m3"), ends_with("xanthin")) |>
  rename_with(
    ~ paste0(., "_mg_m3"),
    any_of(c("caroten", "peridinin", "lutein"))
  ) |>
  names() |>
  sort()

write_csv(surface, here("data", "clean", "surface.csv"))
