#' Find the root of a Quarto project
#'
#' Looks for a Quarto project root by searching for `_quarto.yml` or
#' `_quarto.yaml`. When available, `QUARTO_PROJECT_ROOT` is used first.
#'
#' @param path Starting path for the upward search. If `NULL`, the function uses
#'   `QUARTO_DOCUMENT_PATH` when available, otherwise the current working
#'   directory.
#' @param use_quarto_env Logical. If `TRUE`, use `QUARTO_PROJECT_ROOT` when set.
#' @param required Logical. If `TRUE`, error when no Quarto project root is found.
#'
#' @return A path to the Quarto project root, or `NULL` if not found and
#'   `required = FALSE`.
#' @export
quarto_root <- function(path = NULL,
                        use_quarto_env = TRUE,
                        required = TRUE) {
  if (isTRUE(use_quarto_env)) {
    env_root <- Sys.getenv("QUARTO_PROJECT_ROOT", unset = "")

    if (nzchar(env_root)) {
      if (!dir.exists(env_root)) {
        stop(
          "`QUARTO_PROJECT_ROOT` is set but does not point to an existing directory: ",
          env_root,
          call. = FALSE
        )
      }

      return(env_root)
    }
  }

  if (is.null(path)) {
    doc_path <- Sys.getenv("QUARTO_DOCUMENT_PATH", unset = "")

    if (nzchar(doc_path)) {
      path <- dirname(doc_path)
    } else {
      path <- "."
    }
  }

  if (!length(path) == 1 || is.na(path) || !nzchar(path)) {
    stop("`path` must be a non-empty character string.", call. = FALSE)
  }

  if (file.exists(path) && !dir.exists(path)) {
    path <- dirname(path)
  }

  current <- path

  repeat {
    has_quarto_yml <- file.exists(file.path(current, "_quarto.yml"))
    has_quarto_yaml <- file.exists(file.path(current, "_quarto.yaml"))

    if (has_quarto_yml || has_quarto_yaml) {
      return(current)
    }

    parent <- dirname(current)

    if (identical(normalizePath(parent, mustWork = FALSE),
                  normalizePath(current, mustWork = FALSE))) {
      break
    }

    current <- parent
  }

  if (isTRUE(required)) {
    stop(
      "Could not find a Quarto project root. ",
      "Expected `_quarto.yml` or `_quarto.yaml` in `path` or one of its parents.",
      call. = FALSE
    )
  }

  NULL
}


#' Get the current Quarto document directory
#'
#' Uses `QUARTO_DOCUMENT_PATH` when available.
#'
#' @param default Value returned when `QUARTO_DOCUMENT_PATH` is not set.
#'
#' @return A directory path.
#' @export
quarto_document_dir <- function(default = ".") {
  doc_path <- Sys.getenv("QUARTO_DOCUMENT_PATH", unset = "")

  if (!nzchar(doc_path)) {
    return(default)
  }

  dirname(doc_path)
}
