# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract variables names from the clean data to prepare Table 1.
# The data was pasted into a Google Sheet so collaborators can work on it.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# List all variables (columns) in the data --------------------------------

files <- fs::dir_ls(here("data", "clean"), glob = "*.csv")

df <- map(files, data.table::fread) |>
  map(.x = _, names) |>
  map(.x = _, enframe, value = "variable", name = NULL) |>
  bind_rows(.id = "source_file") |>
  mutate(source_file = basename(source_file)) |>
  distinct(variable, .keep_all = TRUE)

df

# Create a latex table for the paper --------------------------------------

## ├ Download the data from Google sheet ----

df <- read_sheet("https://docs.google.com/spreadsheets/d/1zxyXOQypL-lr68DxmS8DIBQbzxSmFJKcOtZUq4dFcV0/edit#gid=0") |>
  janitor::clean_names() |>
  janitor::remove_empty(which = c("rows", "cols"))

df

## ├ Create the table ----

# %% ---- Create and save the table

# To show missing values as blank cells instead of 'NA'
options(knitr.kable.NA = "")

df |>
  mutate(source_file = str_replace_all(source_file, "_", "\\\\_")) |>
  mutate(variable = str_replace_all(variable, "_", "\\\\_")) |>
  mutate(variable = str_replace_all(variable, "%", "\\\\%")) |>
  mutate(units = case_when(
    units == "m-1" ~ "m\\textsuperscript{-1}",
    units == "mg m-3" ~ "mg~m\\textsuperscript{-3}",
    units == "m-2 mg chla -1" ~ "m\\textsuperscript{2}~mg~chla~\\textsuperscript{-1}",
    units == "wm-2 µm-1" ~ "w~m\\textsuperscript{-2}~\\textmu m~\\textsuperscript{-1}",
    units == "nm-1" ~ "nm\\textsuperscript{-1}",
    units == "g m-3" ~ "g~m\\textsuperscript{-3}",
    units == "um" ~ "µm",
    units == "ms cm-1" ~ "ms~cm\\textsuperscript{-1}",
    units == "m s-1" ~ "m~s\\textsuperscript{-1}",
    units == "µmol m-2 s-1" ~ "µmol~m\\textsuperscript{-2}~s\\textsuperscript{-1}",
    TRUE ~ units
  )) |>
  kbl(
    caption = "List of measured parameters",
    booktabs = TRUE,
    longtable = TRUE,
    format = "latex",
    escape = FALSE,
    linesep = "\\addlinespace",
    col.names = c("Source file", "Variable", "Units", "PI", "Description")
  ) |>
  kable_styling(
    latex_options = c("striped", "repeat_header", "hold_position"),
    font_size = 8
  ) |>
  column_spec(1, width = "10em") |>
  column_spec(2, width = "15em") |>
  column_spec(3, width = "8em") |>
  column_spec(4, width = "5em") |>
  column_spec(5, width = "25em") |>
  landscape() |>
  save_kable(here("tables", "table01.tex"))
# %%
