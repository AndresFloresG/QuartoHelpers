#' List available Quarto profiles
#'
#' Detects profiles from files named `_quarto-<profile>.yml` or
#' `_quarto-<profile>.yaml` in a Quarto project root.
#'
#' @param root Quarto project root. If `NULL`, detected with [quarto_root()].
#'
#' @return A named character vector. Names are profile names and values are
#'   profile configuration file paths.
#' @export
available_profiles <- function(root = NULL) {
  if (is.null(root)) {
    root <- quarto_root()
  }

  if (!dir.exists(root)) {
    stop("`root` does not point to an existing directory: ", root, call. = FALSE)
  }

  files <- list.files(
    path = root,
    pattern = "^_quarto-.+\\.(ya?ml)$",
    full.names = FALSE,
    ignore.case = TRUE
  )

  if (!length(files)) {
    return(setNames(character(), character()))
  }

  profiles <- sub(
    pattern = "^_quarto-(.+)\\.(ya?ml)$",
    replacement = "\\1",
    x = files,
    ignore.case = TRUE
  )

  setNames(file.path(root, files), profiles)
}


#' Get active Quarto profiles
#'
#' Reads active profiles from the `QUARTO_PROFILE` environment variable.
#' Multiple profiles separated by commas are supported.
#'
#' @param default Value returned when `QUARTO_PROFILE` is not set.
#'
#' @return A character vector of active profiles.
#' @export
active_profiles <- function(default = character()) {
  x <- Sys.getenv("QUARTO_PROFILE", unset = "")

  if (!nzchar(x)) {
    return(default)
  }

  profiles <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  profiles <- profiles[nzchar(profiles)]

  unique(profiles)
}


#' Check whether a Quarto profile is active
#'
#' @param profile Profile name or names to check.
#' @param default Value used when `QUARTO_PROFILE` is not set.
#'
#' @return A logical vector with the same length as `profile`.
#' @export
is_profile_active <- function(profile, default = character()) {
  if (missing(profile) || !length(profile)) {
    stop("`profile` must contain at least one profile name.", call. = FALSE)
  }

  profile %in% active_profiles(default = default)
}
