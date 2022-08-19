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

df <- read_sheet("https://docs.google.com/spreadsheets/d/1zxyXOQypL-lr68DxmS8DIBQbzxSmFJKcOtZUq4dFcV0/edit#gid=0") |>
  janitor::clean_names() |>
  janitor::remove_empty(which = c("rows", "cols"))

df

df |>
  mutate(source_file = str_remove(source_file, ".csv")) |>
  # Not sure if I should group by category
  group_by(source_file) |>
  gt(rowname_col = "df") |>
  tab_header(md("**List of variables**")) |>
  cols_label(
    variable = "Variable",
    units = "Units",
    pi = "PI",
    description = "Description"
  ) |>
  sub_missing(
    columns = c(pi, description),
    missing_text = "TBD"
  ) |>
  # sub_missing(
  #   columns = units,
  #   missing_text = "Not Applicable"
  # ) |>
  text_transform(
    locations = cells_body(columns = units),
    fn = function(x) {
      # Write latex here for the PDF
      case_when(
        x == "m-1" ~ "m\\textsuperscript{-1}",
        x == "m2 mg-1" ~ "m\\textsuperscript{2}~mg\\textsuperscript{-1}",
        x == "wm-2" ~ "wm\\textsuperscript{-2}",
        x == "nm-1" ~ "nm\\textsuperscript{-1}",
        x == "gm-3" ~ "gm\\textsuperscript{-3}",
        x == "um" ~ "µm",
        TRUE ~ x
      )
    }
  ) |>
  # cols_width(
  #   description ~ px(150)
  # ) |>
  gtsave("~/Desktop/gt.tex")
