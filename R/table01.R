# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract variables names from the clean data to prepare Table 1.
# The data was pasted into a Google Sheet so collaborators can work on it.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

files <- fs::dir_ls(here("data","clean"), glob = "*.csv")

df <- map(files, data.table::fread) %>%
  map(., names) %>%
  map(., enframe, value = "variable", name = NULL) %>%
  bind_rows(.id = "source_file") %>%
  mutate(source_file = basename(source_file)) %>%
  distinct(variable, .keep_all = TRUE)

df

