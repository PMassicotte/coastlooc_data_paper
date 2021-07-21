# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the relationships for non radiometric data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/zzz.R")

station <- read_csv(here("data","clean","stations.csv"))

pigments <- read_csv(here("data","clean","surface.csv")) %>%
  left_join(station, ., by = "station")

df_viz <- pigments %>%
  select(c(
    station,
    area,
    contains("chl"),
    contains("xanthin"),
    poc_g_m_3,
    spm:poc_g_m_3,
    al:micro,
    doc_um
  ))

df_viz %>%
  select(where(is.numeric)) %>%
  correlate() %>%
  focus(chl_a) %>%
  arrange(desc(chl_a))

df_viz %>%
  select(where(is.numeric)) %>%
  correlate() %>%
  stretch(remove.dups = TRUE, na.rm = TRUE) %>%
  filter(r >= 0.7)

df_viz %>%
  select(where(is.numeric), -doc_um) %>%
  correlate() %>%
  network_plot(min_cor = 0.6, curved = F)

df_viz %>%
  select(contains("chl"), contains("xanthin")) %>%
  correlate() %>%
  rearrange()

df_viz %>%
  drop_na(
    total_chl_a,
    diadinoxanthin,
    diatoxanthin,
    zeaxanthin,
    violaxanthin,
    carotene
  ) %>%
  rowwise() %>%
  mutate(photo_pigment = sum(c_across(
    c(
      total_chl_a,
      diadinoxanthin,
      diatoxanthin,
      zeaxanthin,
      violaxanthin,
      carotene
    )
  ), na.rm = TRUE)) %>%
  # filter(if_all(c(total_chl_a, diadinoxanthin), ~ . > 0)) %>%
  ggplot(aes(x = total_chl_a, y = photo_pigment, color = area)) +
  geom_point() +
  facet_wrap(~area, scales = "free") +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.25) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "Total chla",
    y = "Total photoprotection pigments"
  ) +
  theme(legend.position = "none")


df_viz %>%
  drop_na(nano_chl_a, hexanoyloxyfucoxanthin_19) %>%
  filter(if_all(c(nano_chl_a, hexanoyloxyfucoxanthin_19), ~. > 0)) %>%
  ggplot(aes(x = nano_chl_a, y = hexanoyloxyfucoxanthin_19, color = area)) +
  geom_point(size = 0.5) +
  facet_wrap(~area) +
  geom_smooth(method = "lm", size = 0.25) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(size = 0.25) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = "nano_chl_a",
    y = "hexanoyloxyfucoxanthin_19"
  ) +
  theme(
    legend.position = "none"
  )
