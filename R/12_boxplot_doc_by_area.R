source("R/zzz.R")

station <- read_csv(here("data/clean/stations.csv"))

doc <- read_csv(here("data/clean/surface.csv")) %>%
  select(station, doc_um, salinity)

df <- station %>%
  left_join(doc, by = "station")

# Plot --------------------------------------------------------------------

p <- df %>%
  drop_na(doc_um) %>%
  mutate(area = fct_reorder(area, doc_um)) %>%
  ggplot(aes(x = area, y = doc_um, fill = area)) +
  geom_boxplot(size = 0.1, outlier.size = 1) +
  scale_fill_manual(
    breaks = area_breaks,
    values = area_colors
  ) +
  labs(
    x = NULL,
    y = quote(DOC~(mu*MC))
  ) +
  theme(
    legend.position = "none"
  )

file <- here("graphs/12_boxplot_doc_by_area.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 6.91,
  height = 5.2
)

pdftools::pdf_convert(
  file,
  format = "png",
  filenames = fs::path_ext_set(file, "png"),
  dpi = 300
)
