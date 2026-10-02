unificar_filas <- function(data, key_col = 1, sep = "\n") {

  if (is.numeric(key_col)) {
    key_col <- names(data)[key_col]
  }

  data |>
    dplyr::mutate(
      .grupo = cumsum(
        !is.na(.data[[key_col]]) & .data[[key_col]] != ""
      )
    ) |>
    dplyr::filter(.grupo > 0) |>
    dplyr::group_by(.grupo) |>
    dplyr::summarise(
      dplyr::across(
        -.grupo,
        ~ paste(.x[!is.na(.x) & .x != ""], collapse = sep)
      ),
      .groups = "drop"
    ) |>
    dplyr::select(-.grupo)
}
