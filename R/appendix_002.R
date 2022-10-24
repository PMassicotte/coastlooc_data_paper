# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a table with papers that used COASTLOOC data. The list
# was provided by Frank Fell.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

papers <- read_lines(here("data", "raw", "coastlooc_papers.txt"))

papers

papers |>
  as_tibble() |>
  mutate(value = str_replace_all(value, "λ", "\\lambda")) |>
  # mutate(value = linebreak(value, double_escape = TRUE)) |>
  set_names("Publications") |>
  kbl(
    caption = "Related COASTLOOC publication",
    booktabs = TRUE,
    longtable = TRUE,
    format = "latex",
    escape = TRUE,
    linesep = "\\addlinespace"
  ) |>
  kable_styling(
    latex_options = c("striped", "repeat_header", "hold_position"),
    font_size = 8
  ) |>
  column_spec(1, width = "60em") |>
  save_kable(here("tables", "appendix02.tex"))
