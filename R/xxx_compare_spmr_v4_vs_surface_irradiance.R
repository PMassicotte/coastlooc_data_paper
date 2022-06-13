df <-
  data.table::fread(here("data", "raw", "spmr_vertical_profiles", "v4", "C6003tot.mrg"),
    skip = 3,
    fill = TRUE,
    na.strings = "-9.900E+01"
  ) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(station = "C6003000", .before = 1)

df

spmr <- df %>%
  select(station, cast, depth, starts_with("ed")) %>%
  pivot_longer(starts_with("ed"), names_to = "wavelength", values_to = "ed_wm2_nm1") %>%
  mutate(wavelength = parse_number(wavelength))

surface_irradiance <- read_csv(here(
  "data",
  "clean",
  "irradiance_negative_values_removed.csv"
)) %>%
  filter(station == "C6003000") %>%
  select(station, wavelength, ed_wm2_nm1) %>%
  mutate(depth = 0)

surface_irradiance

unique(surface_irradiance$wavelength)
unique(spmr$wavelength)

surface_irradiance <- surface_irradiance %>%
  mutate(wavelength = case_when(
    wavelength == 411 ~ 412,
    wavelength == 509 ~ 510,
    wavelength == 559 ~ 560,
    wavelength == 665 ~ 664,
    wavelength == 705 ~ 706,
    wavelength == 779 ~ 780,
    TRUE ~ wavelength
  ))

spmr %>%
  ggplot(aes(
    x = ed_wm2_nm1,
    y = depth,
    color = factor(wavelength),
    group = factor(wavelength)
  )) +
  geom_path() +
  geom_point(data = surface_irradiance, aes(x = ed_wm2_nm1, y = depth)) +
  facet_wrap(~wavelength) +
  theme(
    legend.position = "none"
  )

# Check other radiometric data --------------------------------------------

df

df_viz <- df %>%
  select(station, cast, depth, starts_with("ed"), starts_with("eu")) %>%
  pivot_longer(
    c(starts_with("ed"), starts_with("eu")),
    names_to = c(".value", "wavelength"),
    names_pattern = "(.*)(\\d{3})",
    names_transform = list(wavelength = as.integer)
  )

df_viz

df_viz %>%
  ggplot(aes(x = eu, y = depth, color = factor(wavelength))) +
  geom_path()



