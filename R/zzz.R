
# Color palette -----------------------------------------------------------

area_breaks <-
  c(
    "North Sea",
    "English Channel",
    "Atlantic Ocean",
    "Med. Sea (Case 2)",
    "Adriatic Sea",
    "Med. Sea (Case 1)",
    "Baltic Sea"
  )

area_colors <-
  c(
    "#3366CCFF",
    "#DC3912FF",
    "#FF9900FF",
    "#109618FF",
    "#990099FF",
    "#603808FF",
    "#586f7cFF"
  )

# area_colors <- as.vector(palette.colors(length(area_breaks), "Okabe-Ito"))
# area_colors <- as.vector(paletteer::paletteer_d("nbapalettes::nuggets_city2"))

pdf2png <- function(pdf_file, dpi = 300) {
  png_file <- fs::path_ext_set(fs::path_file(pdf_file), "png")
  png_folder <- "graphs/png/"

  fs::dir_create(png_folder)

  outfile <- fs::path(png_folder, png_file)

  png_file <- pdftools::pdf_convert(
    pdf_file,
    format = "png",
    filenames = outfile,
    dpi = dpi,
    verbose = FALSE
  )


  return(png_file)
}
