read_mrg <- function(file) {
  # file <- here("data", "raw", "spmr_vertical_profiles", "v5", "C6003tot.mrg")

  dat <- read_lines(file)

  headers1 <- dat[[1]] |>
    str_split(" ") |>
    unlist()

  headers1 <- headers1[headers1 != ""]

  headers2 <- dat[[2]] |>
    str_split("\\s{2,}") |>
    unlist()

  headers2 <- headers2[headers2 != ""]

  headers <- paste(headers1, headers2, sep = "_") |>
    janitor::make_clean_names(replace = c("µ" = "u"))

  df <-
    read_table(
      dat,
      skip = 2,
      col_names = headers,
      na = "NaN",
      col_types = cols(.default = col_number())
    )

  df <- df |>
    mutate(
      station = basename(tools::file_path_sans_ext(file)),
      .before = 1
    ) |>
    mutate(station = str_remove(station, "tot")) |>
    mutate(station = str_pad(
      station,
      width = 8,
      pad = 0,
      side = "right"
    )) |>
    rename_with(~ str_remove(., "_1$"), ends_with("_1"))


  return(df)
}


tidy_mrg <- function(df_raw) {
  df_tidy <- df_raw |>
    select(!starts_with("er00")) |>
    pivot_longer(
      matches("\\d{3}"),
      names_to = c("variable", "wavelength", "unit"),
      names_pattern = c("(ed|eu|kd|ku)(\\d{3})_(.*)"),
      values_to = "value",
      names_transform = list(wavelength = parse_number)
    ) |>
    unite(variable, variable, unit) |>
    pivot_wider(names_from = variable, values_from = value) |>
    add_count(station, depth_m, wavelength) |>
    assertr::verify(n == 1) |>
    select(-n) |>
    rename_with(~ str_replace(., "1_m", "m1"), ends_with("1_m"))

  # Wavelengths are not exactly the same, have to find a way to reorganize that.
  # In Marcel's data wavelengths are:
  # 411, 443, 456, 490, 509, 532, 556, 590, 665, 683, 705, 799, 866

  # -   Recode Ed wavelengths as:
  # -   412 -> 411
  # -   510 -> 509
  # -   589 -> 590
  # -   666 -> 665
  # -   780 -> 779

  # df_tidy <- df_tidy |>
  #   mutate(
  #     wavelength = case_when(
  #       wavelength == 412 ~ 411,
  #       wavelength == 510 ~ 509,
  #       wavelength == 560 ~ 559,
  #       wavelength == 589 ~ 590,
  #       wavelength == 666 ~ 665,
  #       wavelength == 780 ~ 779,
  #       TRUE ~ wavelength
  #     )
  #   ) |>
  #   assertr::verify(wavelength %in%
  #                     c(411, 443, 456, 490, 509, 532, 556, 559, 590, 619, 665, 683, 705, 779, 866))

  # Classify up and down cast

  df_tidy <- df_tidy |>
    mutate(
      cast_group = case_when(
        cast < 0 ~ "downcast",
        cast > 0 ~ "upcast",
        TRUE ~ NA_character_
      ),
      .after = cast
    )

  return(df_tidy)
}
