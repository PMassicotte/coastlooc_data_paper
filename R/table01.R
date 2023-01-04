# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Table 1 listing the number of stations visited during each
# cruise.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

stations <- read_csv(here("data", "clean", "stations.csv"))

df <- stations |>
  mutate(cruise = str_sub(station, 1, 2), .before = station) |>
  group_by(area, cruise) |>
  summarise(n = n(), sampling_period = paste(min(date), max(date)), .groups = "drop") |>
  arrange(cruise) |>
  rename_all(.funs = \(x) str_to_title(x)) |>
  rename_all(.funs = \(x) str_replace(x, "_", " "))

df |>
  kbl(
    caption = "Areas visited during the COASTlOOC campaigns C1 to C6 and the corresponding number of stations visited.",
    booktabs = TRUE,
    longtable = TRUE,
    format = "latex",
    escape = FALSE,
    linesep = "\\addlinespace"
  ) |>
  kable_styling(
    latex_options = c("striped", "repeat_header", "hold_position"),
    font_size = 8
  ) |>
  column_spec(1, width = "10em") |>
  column_spec(2, width = "2em") |>
  column_spec(3, width = "2em") |>
  column_spec(4, width = "10em") |>
  landscape() |>
  save_kable(here("tables", "table01.tex"))
