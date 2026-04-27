#' Knit a child QMD document
#'
#' Resolves a `.qmd` path and returns the knitted child output as character
#' without printing it.
#'
#' @param path Path to a `.qmd` file.
#' @param root One of `"project"`, `"document"` or `"none"`.
#' @param project_root Quarto project root. Used when `root = "project"`.
#' @param document_dir Quarto document directory. Used when `root = "document"`.
#' @param quiet Passed to [knitr::knit_child()].
#' @param envir Environment used by [knitr::knit_child()].
#' @param options Optional list of knitr chunk options passed to
#'   [knitr::knit_child()].
#' @importFrom knitr knit_child
#' @return A character vector with knitted output.
#' @export
knit_qmd_child <- function(path,
                           root = c("project", "document", "none"),
                           project_root = NULL,
                           document_dir = NULL,
                           quiet = TRUE,
                           envir = parent.frame(),
                           options = NULL) {
  root <- match.arg(root)

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package `knitr` is required to knit child documents.", call. = FALSE)
  }

  file <- resolve_quarto_path(
    path = path,
    root = root,
    project_root = project_root,
    document_dir = document_dir,
    must_exist = TRUE
  )

  knitr::knit_child(
    input = file,
    quiet = quiet,
    envir = envir,
    options = options
  )
}
