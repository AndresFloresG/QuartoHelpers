#' Include a QMD child document
#'
#' Resolves, knits and prints a `.qmd` child document.
#'
#' @param path Path to a `.qmd` file.
#' @param root One of `"project"`, `"document"` or `"none"`.
#' @param project_root Quarto project root. Used when `root = "project"`.
#' @param document_dir Quarto document directory. Used when `root = "document"`.
#' @param quiet Passed to [knitr::knit_child()].
#' @param envir Environment used by [knitr::knit_child()].
#' @param options Optional list of knitr chunk options passed to
#'   [knitr::knit_child()].
#'
#' @return The resolved file path, invisibly.
#' @export
include_qmd <- function(path,
                        root = c("project", "document", "none"),
                        project_root = NULL,
                        document_dir = NULL,
                        quiet = TRUE,
                        envir = parent.frame(),
                        options = NULL) {
  root <- match.arg(root)

  file <- resolve_quarto_path(
    path = path,
    root = root,
    project_root = project_root,
    document_dir = document_dir,
    must_exist = TRUE
  )

  output <- knit_qmd_child(
    path = file,
    root = "none",
    quiet = quiet,
    envir = envir,
    options = options
  )

  cat(output, sep = "\n")

  invisible(file)
}
