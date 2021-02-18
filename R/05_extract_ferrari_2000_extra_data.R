# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Extract data from Ferrari 2000.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

file <- here("man/Ferrari_2000.pdf")

df <- tabulizer::extract_tables(
  file,
  pages = 14:18,
  method = "decide"
)

df

# Calculate how many columns each table has.

i <- map_int(df, ncol)

# Tidy every table with 7 columns.

df1 <- df[i == 7] %>%
  do.call(rbind, .) %>%
  as_tibble() %>%
  filter(str_detect(V1, "VH\\d{2}|The\\d{2}")) %>%
  set_names(c("station", "depth_m", "a_cdom_350", "s_cdom", "aqy", "doc_um", "salinity")) %>%
  mutate(across(where(is.character), ~ na_if(., "n.m."))) %>%
  type_convert()

# Tidy every table with 6 columns.

df2 <- df[i == 6] %>%
  do.call(rbind, .) %>%
  as_tibble() %>%
  filter(str_detect(V1, "VH\\d{2}|The\\d{2}")) %>%
  set_names(c("station", "depth_m", "a_cdom_350", "s_cdom", "doc_um", "salinity")) %>%
  mutate(across(where(is.character), ~ na_if(., "n.m."))) %>%
  type_convert()

# Keep only the surface values because we only have such data in the main
# dataset.

massimo <- bind_rows(df1, df2) %>%
  filter(depth_m == 0)

massimo

# Compare scdom between Ferrari and Marcel data ---------------------------

scdom_ferrari <- massimo %>%
  mutate(station = str_replace(station, "VH", "C1")) %>%
  mutate(station = str_replace(station, "The", "C4")) %>%
  extract(station, into = c("cruise", "station"), regex = "(.*)(\\d{2})$")

scdom_ferrari

scdom_babin <- read_csv("data/clean/surface.csv") %>%
  select(station, s_cdom_model) %>%
  filter(str_detect(station, "^C1|^C4")) %>%
  extract(station, into = c("cruise", "station"), regex = "(.{2}).(.{2})")

df_viz <- scdom_ferrari %>%
  left_join(scdom_babin) %>%
  mutate(cruise = case_when(
    cruise == "C1" ~ "C1 (VH)",
    cruise == "C4" ~ "C4 (The)",
  ))


p <- df_viz %>%
  ggplot(aes(x = s_cdom, s_cdom_model, color = cruise)) +
  geom_point() +
  geom_abline(lty = 2, size = 0.25) +
  ggrepel::geom_text_repel(aes(label = station), show.legend = FALSE, size = 2) +
  labs(
    x = quote(S[cdom] ~ (Marcel)),
    y = quote(S[cdom] ~ (Ferrari))
  )

ggsave(
  here("graphs/05_scdom_ferrari_vs_babin.pdf"),
  device = cairo_pdf,
  width = 6,
  height = 4
)

# Export data -------------------------------------------------------------

# TODO
