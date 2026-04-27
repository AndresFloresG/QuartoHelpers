#' Include a QMD child document conditionally by active Quarto profile
#'
#' Selects one or more `.qmd` files according to active Quarto profiles and
#' includes them with [include_qmd()].
#'
#' @param files Named character vector or list. Names must be profile names and
#'   values must be `.qmd` paths.
#' @param root One of `"project"`, `"document"` or `"none"`.
#' @param default Default profiles used when `QUARTO_PROFILE` is not set.
#' @param project_root Quarto project root. Used when `root = "project"`.
#' @param document_dir Quarto document directory. Used when `root = "document"`.
#' @param require_file Logical. If `TRUE`, error when a selected file does not
#'   exist. If `FALSE`, missing selected files are skipped.
#' @param require_match Logical. If `TRUE`, error when no active profile has an
#'   assigned file.
#' @param quiet Passed to [knitr::knit_child()].
#' @param envir Environment used by [knitr::knit_child()].
#' @param options Optional list of knitr chunk options passed to
#'   [knitr::knit_child()].
#'
#' @return A named character vector of included file paths, invisibly.
#' @export
include_profile_qmd <- function(files,
                                root = c("project", "document", "none"),
                                default = character(),
                                project_root = NULL,
                                document_dir = NULL,
                                require_file = TRUE,
                                require_match = TRUE,
                                quiet = TRUE,
                                envir = parent.frame(),
                                options = NULL) {
  root <- match.arg(root)

  if (missing(files) || !length(files)) {
    stop("`files` must be a non-empty named character vector or list.", call. = FALSE)
  }

  if (is.list(files)) {
    files <- unlist(files, recursive = FALSE, use.names = TRUE)
  }

  if (!is.character(files)) {
    stop("`files` must be a named character vector or list.", call. = FALSE)
  }

  file_names <- names(files)

  if (is.null(file_names) || any(!nzchar(file_names))) {
    stop(
      "`files` must be named. Use profile names as names, for example: ",
      'c(v1 = "section-v1.qmd", v2 = "section-v2.qmd").',
      call. = FALSE
    )
  }

  active <- active_profiles(default = default)

  if (!length(active)) {
    stop(
      "No active Quarto profile was found. ",
      "Set `QUARTO_PROFILE` or provide `default`.",
      call. = FALSE
    )
  }

  matched_profiles <- intersect(active, file_names)

  if (!length(matched_profiles)) {
    if (isTRUE(require_match)) {
      stop(
        "No file was assigned for the active profile(s): ",
        paste(active, collapse = ", "),
        call. = FALSE
      )
    }

    return(invisible(setNames(character(), character())))
  }

  selected_files <- files[matched_profiles]

  resolved_files <- resolve_quarto_path(
    path = selected_files,
    root = root,
    project_root = project_root,
    document_dir = document_dir,
    must_exist = FALSE
  )

  missing_files <- !file.exists(resolved_files)

  if (any(missing_files)) {
    missing_msg <- paste(
      paste0(names(resolved_files)[missing_files], ": ", resolved_files[missing_files]),
      collapse = ", "
    )

    if (isTRUE(require_file)) {
      stop(
        "The following selected profile file(s) do not exist: ",
        missing_msg,
        call. = FALSE
      )
    }

    resolved_files <- resolved_files[!missing_files]
  }

  for (file in resolved_files) {
    include_qmd(
      path = file,
      root = "none",
      quiet = quiet,
      envir = envir,
      options = options
    )
  }

  invisible(resolved_files)
}
