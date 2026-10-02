#' Unify empty cells in a dataframe
#'
#' @param data dataframe
#' @param key_col column to be unified
#' @param sep separator
#'
#' @returns
#' @export
#'
#' @examples
#'
#' @export

unify_table <- function(data, key_col = 1, sep = "\n") {

  if (is.numeric(key_col)) {
    key_col <- names(data)[key_col]
  }

  data |>
    dplyr::mutate(
      .grupo = cumsum(
        !is.na(.data[[key_col]]) &
          trimws(as.character(.data[[key_col]])) != ""
      )
    ) |>
    dplyr::filter(.grupo > 0) |>
    dplyr::group_by(.grupo) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::everything(),
        \(x) {
          x <- as.character(x)
          paste(
            x[!is.na(x) & trimws(x) != ""],
            collapse = sep
          )
        }
      ),
      .groups = "drop"
    ) |>
    dplyr::select(-.grupo)
}
