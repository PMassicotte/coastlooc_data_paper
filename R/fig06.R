# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Explore the particle scattering coefficients.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "zzz.R"))

# Particulate scattering --------------------------------------------------

stations <- read_csv(here("data", "clean", "stations.csv"))
ac9 <- read_csv(here("data", "clean", "ac9.csv"))
ac9 <- inner_join(stations, ac9, by = "station")

ac9

# Only 2 observations in the Adriatic Sea?
ac9 |>
  filter(wavelength == 440) |>
  drop_na(bp_m1) |>
  count(area)

p1 <- ac9 |>
  filter(wavelength == 440) |>
  drop_na(bp_m1) |>
  mutate(area = fct_reorder(area, bp_m1)) |>
  ggplot(aes(x = area, y = bp_m1)) +
  geom_boxplot(aes(color = area), size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = TRUE,
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = parse(text = "italic(b)[p](440)~(m^{-1})")
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Downward attenuation coefficient ----------------------------------------

irradiance <- read_csv(here("data", "clean", "irradiance.csv"))
stations <- read_csv(here("data", "clean", "stations.csv"))

irradiance <- irradiance |>
  inner_join(stations, by = "station")

p2 <- irradiance |>
  filter(wavelength == 443) |>
  drop_na(k_ed_m1) |>
  mutate(area = fct_reorder(area, k_ed_m1)) |>
  ggplot(aes(x = area, y = k_ed_m1)) +
  geom_boxplot(aes(color = area), size = 0.25, outlier.size = 0.5) +
  ggbeeswarm::geom_quasirandom(
    groupOnX = TRUE,
    aes(fill = area),
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  scale_y_log10() +
  scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_color_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = parse(text = "italic(K)[Ed](443) ~ (m^{-1})")
  ) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs", "fig06.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)

# Stats for the paper -----------------------------------------------------

ac9 |>
  filter(wavelength == 440) |>
  pull(bp_m1) |>
  range(na.rm = TRUE)

ac9 |>
  filter(wavelength == 440) |>
  group_by(area) |>
  summarise(median_bp_m1 = median(bp_m1, na.rm = TRUE)) |>
  arrange(median_bp_m1)

irradiance |>
  filter(wavelength == 443) |>
  group_by(area) |>
  summarise(median_kd_m1 = median(k_ed_m1, na.rm = TRUE)) |>
  arrange(median_kd_m1)

# %% ---- title

# Relation between a and c from AC9 (at surface). Made to anwswer reviewer's
# comment.

p <- ac9 |>
  drop_na(a_m1, bp_m1) |>
  ggplot(aes(x = a_m1, y = bp_m1)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = parse(text = "a~(m^{-1})"),
    y = parse(text = "b[p]~(m^{-1})")
  ) +
  annotation_logticks(size = 0.2) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 0.05,
    family = "Montserrat",
    size = 3.5
  ) +
  ggpmisc::stat_poly_eq(
    aes(
      label = paste(..rr.label.., after_stat(p.value.label), sep = "*\", \"*")
    ),
    label.x.npc = 0.05,
    label.y.npc = 0.88,
    coef.digits = 4,
    parse = TRUE,
    family = "Montserrat",
    size = 3.5,
    small.p = TRUE
  ) +
  facet_wrap(~ glue("{wavelength} nm"), scales = "free")

ggsave(
  here("graphs", "999_a_vs_bp_ac9.png"),
  device = ragg::agg_png(),
  width = 9,
  height = 9,
  dpi = 300,
  units = "in"
)
# %%
# %% ---- title

ac9

absorption <- read_csv(here("data", "clean", "absorption.csv")) |>
  select(station, wavelength, a_p_m1)

df_viz <- ac9 |>
  inner_join(absorption)

df_viz |>
  ggplot(aes(x = a_p_m1, y = bp_m1)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = parse(text = "a[p]~(m^{-1})"),
    y = parse(text = "b[p]~(m^{-1})")
  ) +
  annotation_logticks(size = 0.2) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    parse = TRUE,
    coef.digits = 4,
    f.digits = 5,
    p.digits = 10,
    label.x.npc = 0.05,
    family = "Montserrat",
    size = 3.5
  ) +
  ggpmisc::stat_poly_eq(
    aes(
      label = paste(..rr.label.., after_stat(p.value.label), sep = "*\", \"*")
    ),
    label.x.npc = 0.05,
    label.y.npc = 0.88,
    coef.digits = 4,
    parse = TRUE,
    family = "Montserrat",
    size = 3.5,
    small.p = TRUE
  ) +
  facet_wrap(~ glue("{wavelength} nm"), scales = "free")

ggsave(
  here("graphs", "999_ap_vs_bp_ac9.png"),
  device = ragg::agg_png(),
  width = 12,
  height = 9,
  dpi = 300,
  units = "in"
)
# %%
