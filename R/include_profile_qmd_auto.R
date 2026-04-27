#' Include a QMD file selected by profile-specific filename convention
#'
#' Builds candidate file names from active profiles and includes matching files.
#' This is optional and convention-based; use [include_profile_qmd()] when you
#' want explicit file mapping.
#'
#' @param pattern Filename pattern. Must contain `\{profile\}`.
#' @param profiles Profiles to use. Defaults to [active_profiles()].
#' @param root One of `"project"`, `"document"` or `"none"`.
#' @param default Default profiles used when `QUARTO_PROFILE` is not set.
#' @param project_root Quarto project root. Used when `root = "project"`.
#' @param document_dir Quarto document directory. Used when `root = "document"`.
#' @param require_file Logical. If `TRUE`, error when a selected file does not
#'   exist.
#' @param require_match Logical. If `TRUE`, error when no profile-specific file
#'   can be included.
#' @param quiet Passed to [knitr::knit_child()].
#' @param envir Environment used by [knitr::knit_child()].
#' @param options Optional list of knitr chunk options passed to
#'   [knitr::knit_child()].
#'
#' @return A named character vector of included file paths, invisibly.
#' @importFrom stats setNames
#' @export
include_profile_qmd_auto <- function(pattern,
                                     profiles = active_profiles(default = default),
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

  if (missing(pattern) || !length(pattern) || is.na(pattern) || !nzchar(pattern)) {
    stop("`pattern` must be a non-empty character string.", call. = FALSE)
  }

  if (!grepl("{profile}", pattern, fixed = TRUE)) {
    stop("`pattern` must contain `{profile}`.", call. = FALSE)
  }

  if (!length(profiles)) {
    stop(
      "No active Quarto profile was found. ",
      "Set `QUARTO_PROFILE`, provide `profiles`, or provide `default`.",
      call. = FALSE
    )
  }

  files <- stats::setNames(
    gsub("{profile}", profiles, pattern, fixed = TRUE),
    profiles
  )

  include_profile_qmd(
    files = files,
    root = root,
    default = default,
    project_root = project_root,
    document_dir = document_dir,
    require_file = require_file,
    require_match = require_match,
    quiet = quiet,
    envir = envir,
    options = options
  )
}

#' @noRd
#' @export
.insert_qmd <- function(path = file.path(),
                       root = c("project", "document", "none"),
                       project_root = NULL,
                       document_dir = NULL,
                       quiet = TRUE,
                       envir = parent.frame(),
                       options = NULL) {
  include_qmd(
    path = path,
    root = root,
    project_root = project_root,
    document_dir = document_dir,
    quiet = quiet,
    envir = envir,
    options = options
  )
}

#' @noRd
#' @export
.quartocond_cont <- function(archivos,
                            root = c("project", "document", "none"),
                            default = character(),
                            exigir_archivo = TRUE,
                            project_root = NULL,
                            document_dir = NULL,
                            quiet = TRUE,
                            envir = parent.frame(),
                            options = NULL) {
  include_profile_qmd(
    files = archivos,
    root = root,
    default = default,
    project_root = project_root,
    document_dir = document_dir,
    require_file = exigir_archivo,
    require_match = TRUE,
    quiet = quiet,
    envir = envir,
    options = options
  )
}
