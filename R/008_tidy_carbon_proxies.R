# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy carbon proxies such as DOC, POC, PON, TCP, etc.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

stations <- read_csv(here("data", "raw", "tidied", "surface_data.csv"))

stations

head(names(stations), 20)

carbon <- stations |>
  select(
    station,
    particulate_organic_carbon_g_m3 = poc_g_m_3,
    particulate_organic_nitrogen_g_m3 = pon_g_m_3,
    suspended_particulate_matter_g_m3 = spm,
    total_particulate_carbon_g_m3 = tpc_g_m_3
  )

carbon

carbon |>
  ggplot(aes(x = particulate_organic_carbon_g_m3)) +
  geom_histogram() +
  scale_x_log10()

# One very low outlier value in POC
carbon <- carbon |>
  mutate(
    particulate_organic_carbon_g_m3 = case_when(
      particulate_organic_carbon_g_m3 < 0.01 ~ NA_real_,
      TRUE ~ particulate_organic_carbon_g_m3
    )
  )

carbon |>
  arrange(particulate_organic_carbon_g_m3)

carbon |>
  ggplot(aes(x = particulate_organic_carbon_g_m3)) +
  geom_histogram() +
  scale_x_log10()

# %% ---- Extract DOC from Farrari 200 paper
file <- here("man", "Ferrari_2000.pdf")

df <- tabulizer::extract_tables(file,
  pages = 14:18,
  method = "decide"
)

df

# Calculate how many columns each table has.
i <- map_int(df, ncol)

# Tidy every table with 7 columns.

df1 <- df[i == 7] |>
  do.call(rbind, args = _) |>
  as_tibble() |>
  filter(str_detect(V1, "VH\\d{2}|The\\d{2}")) |>
  set_names(c(
    "station",
    "depth_m",
    "a_cdom_350",
    "s_cdom",
    "aqy",
    "doc_um_c",
    "salinity"
  )) |>
  mutate(across(where(is.character), ~ na_if(., "n.m."))) |>
  type_convert()

# Tidy every table with 6 columns.

df2 <- df[i == 6] |>
  do.call(rbind, args = _) |>
  as_tibble() |>
  filter(str_detect(V1, "VH\\d{2}|The\\d{2}")) |>
  set_names(c(
    "station",
    "depth_m",
    "a_cdom_350",
    "s_cdom",
    "doc_um_c",
    "salinity"
  )) |>
  mutate(across(where(is.character), ~ na_if(., "n.m."))) |>
  type_convert()

# Keep only the surface values because we only have such data in the main
# dataset.

massimo <- bind_rows(df1, df2) |>
  filter(depth_m == 0) |>
  select(-depth_m, -contains("cdom"), -aqy) |>
  rename(dissolved_organic_carbon_um_c = doc_um_c)

massimo

# %% ---- Convert from mol to mg

# 1 mol C = 12.0107 grams of C Here it is assumed that the data in Massimo is
# presented in uMol C per liter, hence the '* 1000' to convert from L to cubic
# metter.

massimo <- massimo |>
  mutate(
    dissolved_organic_carbon_g_m3 =
      dissolved_organic_carbon_um_c * 0.000001 * 12.0107 * 1000,
    .keep = "unused"
  )

# %%

# %%
# %% ---- Set the correct station names
massimo <- massimo |>
  mutate(station = str_replace(station, "VH", "C10")) |>
  mutate(station = str_replace(station, "The", "C40")) |>
  mutate(station = str_pad(
    station,
    width = 8,
    side = "right",
    pad = "0"
  ))

# The DOC average in g m3 is similar to that presented in Table 11 (page 83 in
# the final report). Looks like the conversion from uMol/L to g/m3 is ok.
massimo |>
  filter(str_starts(station, "C4")) |>
  pull(dissolved_organic_carbon_g_m3) |>
  na.omit() |>
  mean()

# %%

# %% ---- Export data
carbon <- carbon |>
  left_join(massimo, by = "station")

carbon

names(carbon)

write_csv(carbon, here("data", "clean", "nutrients.csv"))
# %%
