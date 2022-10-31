# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure about extrapolation to the surface.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "appendix01_eu_surface_extrapolation.csv"))

# Something weird with the depth difference of 0.00001 m. Check at 443 nm for
# example. 1.20204 m vs 1.20203 m. After pointing it out to Frank, it seems it
# is ok...
df |>
  distinct(wavelength, depth_m)

# Data for Eu 0-
surface <- df |>
  distinct(across(c(wavelength, starts_with("c")))) |>
  mutate(depth_m = 0) |>
  mutate(predicted_eu = c1 * exp(-c2 * depth_m) + c3 * exp(-c4 * depth_m))

# Extrapolate from the top most measure up to the surface
extrapolated_eu <- df |>
  select(wavelength, depth_m, starts_with("c")) |>
  group_by(wavelength) |>
  filter(depth_m == min(depth_m)) |>
  complete(depth_m = seq(0, max(depth_m), by = 0.1)) |>
  ungroup() |>
  fill(everything(), .direction = "up") |>
  mutate(predicted_eu = c1 * exp(-c2 * depth_m) + c3 * exp(-c4 * depth_m))

# %% ---- Eu and Keu

# The fit function id of the form: y = c1 * EXP (-c2*z) + c3 * EXP (-c4*z)
# EU (0-) = c1 + c3 (with c1 indicating the Raman contribution).

# From my deduction, Keu = C4.
# Confirmed by Frank Fell that C4 should be considered as Keu

note <- df |>
  group_by(wavelength) |>
  filter(depth_m == min(depth_m)) |>
  mutate(eu0 = round(c1 + c3, digits = 2)) |>
  mutate(keu = round(c4, digits = 2)) |>
  mutate(eu0_string = glue("E[u](0^'-')~'='~{eu0}")) |>
  mutate(keu_string = glue("K[Eu]~'='~{keu}"))

curve <- surface |>
  filter(wavelength == 443 & depth_m == 0) |>
  mutate(
    x = 41.5,
    y = 0.8,
    xend = predicted_eu,
    yend = 0.1
  )

# %%

# %% ---- Plot
p <- df |>
  # distinct(wavelength, depth_m, .keep_all = TRUE) |>
  ggplot(aes(x = eu, y = depth_m)) +
  geom_point(
    fill = "black",
    size = 2,
    stroke = 0.3,
    pch = 21,
    alpha = 0.5
  ) +
  geom_point(
    data = surface,
    aes(x = predicted_eu, y = depth_m),
    color = "#FE4A49",
    size = 3,
    pch = 4
  ) +
  geom_line(aes(x = predicted_eu, linetype = "Model"), color = "#FE4A49") +
  geom_line(
    data = extrapolated_eu,
    aes(x = predicted_eu, linetype = "Extrapolated"),
    color = "#FE4A49"
  ) +
  geom_text(
    data = note,
    aes(x = Inf, y = Inf, label = eu0_string),
    parse = TRUE,
    vjust = -2,
    hjust = 1,
    size = 3
  ) +
  geom_text(
    data = note,
    aes(x = Inf, y = Inf, label = keu_string),
    parse = TRUE,
    vjust = -1,
    hjust = 1,
    size = 3
  ) +
  geom_curve(
    data = curve,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0.3,
    arrow = ggplot2::arrow(length = unit(2, "mm")),
    size = 0.2
  ) +
  geom_text(
    data = curve,
    aes(x = x, y = y, label = "Predicted value\nat the surface"),
    vjust = 1.1,
    hjust = 0.5,
    size = 3
  ) +
  scale_y_reverse() +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  scale_linetype_manual(
    values = c("Model" = 1, "Extrapolated" = 3)
  ) +
  labs(
    x = parse(text = "E[u]~(W~{m^-2}~mu*m^{-1})"),
    y = "Depth (m)"
  ) +
  facet_wrap(~ glue("{wavelength} nm"), scale = "free_x") +
  theme(
    legend.justification = c(0, 0),
    legend.position = c(0.05, 0.6),
    legend.title = element_blank()
  )

ggsave(
  here("graphs", "appendix01.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)
# %%
