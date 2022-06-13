absorption <- read_csv(here("data","clean","absorption_background_corrected.csv")) %>%
  select(-a_cdom)

acdom <- read_csv(here("data","clean","a_cdom.csv"))

ac9 <- read_csv(here("data","clean","ac9.csv")) %>%
  select(-remote_sensed_vertical_layer_meter)

irradiance <- read_csv(here("data","clean","irradiance.csv"))

reflectance <- read_csv(here("data","clean","reflectance.csv"))

df <- absorption %>%
  full_join(acdom, by = c("station", "wavelength")) %>%
  full_join(ac9, by = c("station", "wavelength")) %>%
  full_join(irradiance, by = c("station", "wavelength")) %>%
  full_join(reflectance, by = c("station", "wavelength"))

df %>%
  filter(wavelength == 532) %>%
  select(-station, -wavelength) %>%
  correlate() %>%
  stretch(na.rm = TRUE, remove.dups = TRUE) %>%
  arrange(desc(r))


df %>%
  drop_na(a_nap, ku) %>%
  ggplot(aes(x = a_nap, y = ku)) +
  geom_point() +
  facet_wrap(~wavelength, scales = "free")
