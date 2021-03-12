# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION: Have a look to the absorption data to determine if the background
# absorbance (i.e. the average absorption of a_tot between 745 and 750 nm) has
# been removed.
#
# Also, re-calculate the background values but between 746 and 750 nm.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

baseline <- read_csv(here("data/clean/surface.csv")) %>%
  select(station, contains("background"))

station <- read_csv(here("data/clean/stations.csv")) %>%
  select(station, area)

baseline %>%
  anti_join(station, by = "station")

baseline <- baseline %>%
  left_join(station, by = "station") %>%
  relocate(area, .after = station)

baseline

absorption <- vroom::vroom(here("data/clean/absorption.csv")) %>%
  drop_na()

absorption

absorption %>%
  anti_join(baseline, by = "station")

absorption <- absorption %>%
  left_join(baseline, by = "station")

absorption

# Extract few spectra in the North Sea area -------------------------------

set.seed(2021)

absorption_north_sea <- absorption %>%
  group_nest(station, area) %>%
  mutate(n = map_int(data, nrow)) %>%
  verify(n == 371) %>%
  filter(area == "North Sea") %>%
  slice_sample(n = 9)

absorption_north_sea <- absorption_north_sea %>%
  unnest(data) %>%
  select(-n)

absorption_north_sea

# Plot --------------------------------------------------------------------

absorption_north_sea %>%
  filter(station == "C5035000") %>%
  select(wavelength, a_phy, a_tot, background_a_tot_average_745_750)

absorption_north_sea

p <- absorption_north_sea %>%
  ggplot(aes(x = wavelength, y = a_tot)) +
  geom_line() +
  geom_hline(
    aes(yintercept = background_a_tot_average_745_750),
    color = "red",
    lty = 2,
    size = 0.25
  ) +
  geom_hline(yintercept = 0, color = "blue", lty = 2, size = 0.25) +
  facet_wrap(~ glue("{station} ({area})"), scales = "free_y") +
  labs(
    title = quote(bold("Examples of "~ a[tot] ~"spectra in the North Sea")),
    subtitle = "The dashed red line is the value of back_tot (745-750 nm).",
    y = quote(a[tot]~(m^{-1})),
    x = "Wavelength (nm)"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs/02_a_tot_spectra_north_sea_without_745_750_background.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)

# Adjust background values ------------------------------------------------

# Marcel wants to calculate the average background value between 746-750 nm
# rather than between 745-750 nm. First, I need to re-add the background values
# to the base spectra.

absorption

absorption <- absorption %>%
  mutate(
    a_phy = a_phy + background_a_phy_average_745_750,
    a_nap = a_nap + background_a_nap_average_745_750,
    a_tot = a_tot + background_a_tot_average_745_750,
  )

# Plot the North Sea spectra with the old background added ----------------

p <- absorption %>%
  semi_join(absorption_north_sea, by = "station") %>%
  ggplot(aes(x = wavelength, y = a_tot)) +
  geom_line() +
  geom_hline(
    aes(yintercept = background_a_tot_average_745_750),
    color = "red",
    lty = 2,
    size = 0.25
  ) +
  geom_hline(yintercept = 0, color = "blue", lty = 2, size = 0.25) +
  facet_wrap(~ glue("{station} ({area})"), scales = "free_y") +
  labs(
    title = quote(bold("Examples of "~ a[tot] ~"spectra in the North Sea")),
    subtitle = "The dashed red line is the value of back_tot (745-750 nm).",
    y = quote(a[tot]~(m^{-1})),
    x = "Wavelength (nm)"
  )

ggsave(
  here("graphs/02_a_tot_spectra_north_sea_with_745_750_background.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)

# Now I can calculate the new background between 746 and 750 nm.

absorption <- absorption %>%
  group_by(station) %>%
  mutate(across(c(a_phy, a_nap, a_tot),
    ~ mean(.[between(wavelength, 746, 750)], na.rm = TRUE),
    .names = "background_{.col}_average_746_750"
  )) %>%
  ungroup()

absorption

# Compare the new and the old background values ---------------------------

absorption %>%
  distinct(station, .keep_all = TRUE) %>%
  ggplot(aes(
    x = background_a_tot_average_745_750,
    y = background_a_tot_average_746_750
  )) +
  geom_point() +
  geom_abline(color = "red")

# Subtract the new background values --------------------------------------

absorption <- absorption %>%
  mutate(
    a_phy = a_phy - background_a_phy_average_746_750,
    a_nap = a_nap - background_a_nap_average_746_750,
    a_tot = a_tot - background_a_tot_average_746_750,
  )

absorption

# Plot the North Sea spectra with the old background added ----------------

p <- absorption %>%
  semi_join(absorption_north_sea, by = "station") %>%
  ggplot(aes(x = wavelength, y = a_tot)) +
  geom_line() +
  geom_hline(
    aes(yintercept = background_a_tot_average_746_750),
    color = "red",
    lty = 2,
    size = 0.25
  ) +
  geom_hline(yintercept = 0, color = "blue", lty = 2, size = 0.25) +
  facet_wrap(~ glue("{station} ({area})"), scales = "free_y") +
  labs(
    title = quote(bold("Examples of "~ a[tot] ~"spectra in the North Sea")),
    subtitle = "The dashed red line is the value of back_tot (746-750 nm).",
    y = quote(a[tot]~(m^{-1})),
    x = "Wavelength (nm)"
  )

ggsave(
  here("graphs/02_a_tot_spectra_north_sea_with_746_750_background.pdf"),
  device = cairo_pdf,
  width = 8,
  height = 6
)

# Export ------------------------------------------------------------------

absorption %>%
  select(station, wavelength, starts_with("a_")) %>%
  data.table::fwrite(here("data/clean/absorption_background_corrected.csv"))
