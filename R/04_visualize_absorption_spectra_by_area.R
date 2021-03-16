# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Visualize the absorption spectra (a_cdom, a_nap, a_tot, etc.).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

absorption <- vroom::vroom(here("data/clean/absorption.csv"))

stations <- read_csv(here("data/clean/stations.csv"))

absorption <- absorption %>%
  full_join(stations, by = "station") %>%
  add_count(station, area, wavelength) %>%
  assertr::verify(n == 1) %>%
  select(-n)

ggabsorption <- function(absorption, variable, display_name) {

  p <- absorption %>%
    drop_na({{variable}}) %>%
    ggplot(aes(x = wavelength, y = {{variable}}, group = station)) +
    geom_line(size = 0.1, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "blue", lty = 2, size = 0.25) +
    facet_wrap(~ glue("{station} ({area})"), scales = "free_y") +
    labs(
      title = parse(text = glue("bold({display_name}~'spectra for the different systems')")),
      y = parse(text = glue("{display_name}~(m^{-1})")),
      x = "Wavelength (nm)"
    ) +
    facet_wrap(~area, scales = "free_y") +
    theme(
      panel.border = element_blank(),
      axis.ticks = element_blank()
    )


}

p_a_phy <- ggabsorption(absorption, a_phy, "a[phy]")
p_a_nap <- ggabsorption(absorption, a_nap, "a[nap]")
p_a_tot <- ggabsorption(absorption, a_tot, "a[tot]")
p_a_cdom <- ggabsorption(absorption, a_cdom_modeled, "a[cdom]")

save_fun <- function(p) {

  fname <- deparse(substitute(p)) %>%
    str_remove("p_")

  ggsave(
    here(glue("graphs/04_{fname}_spectra_by_area.pdf")),
    plot = p,
    device = cairo_pdf,
    height = 6,
    width = 10
  )
}

save_fun(p_a_cdom)
save_fun(p_a_nap)
save_fun(p_a_phy)
save_fun(p_a_tot)
