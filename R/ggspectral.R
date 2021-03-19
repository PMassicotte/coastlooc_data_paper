
#' Plot spectral profiles by area.
#'
#' @param df Dataframe.
#' @param variable Which variable to plot.
#' @param ylab Name of the y axes.
#'
#' @return
#' @export
#'
#' @examples
ggspectral <- function(df, variable, ylab) {
  p <- df %>%
    ggplot(aes(
      x = wavelength,
      y = {{ variable }},
      color = area,
      group = station
    )) +
    geom_line(size = 0.1) +
    scale_color_manual(
      breaks = area_breaks,
      values = area_colors
    ) +
    facet_wrap(~area, scales = "free_y") +
    labs(
      x = "Wavelength (nm)",
      y = parse(text = ylab)
    ) +
    theme(legend.position = "none")

  invisible(p)
}

save_fun <- function(p, filename) {

  # fname <- deparse(substitute(p)) %>%
  #   str_remove("p_")

  ggsave(
    here(filename),
    plot = p,
    device = cairo_pdf,
    height = 6,
    width = 10
  )
}
