
# Color palette -----------------------------------------------------------

# paletteer::paletteer_d("ggthemes::gdoc")
#
# read_csv(here("data/clean/stations.csv")) %>%
#   distinct(area) %>%
#   pull(area)

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
