#' Resolve a path relative to a Quarto context
#'
#' @param path Relative or absolute path.
#' @param root One of `"project"`, `"document"` or `"none"`.
#' @param project_root Quarto project root. Used when `root = "project"`.
#' @param document_dir Document directory. Used when `root = "document"`.
#' @param must_exist Logical. If `TRUE`, error when the resolved path does not
#'   exist.
#'
#' @return A resolved path.
#' @export
resolve_quarto_path <- function(path,
                                root = c("project", "document", "none"),
                                project_root = NULL,
                                document_dir = NULL,
                                must_exist = FALSE) {
  root <- match.arg(root)

  if (missing(path) || !length(path) || anyNA(path) || any(!nzchar(path))) {
    stop("`path` must be a non-empty character vector.", call. = FALSE)
  }

  is_absolute <- grepl("^([A-Za-z]:)?[/\\\\]", path)

  resolved <- path

  if (root == "project") {
    if (is.null(project_root)) {
      project_root <- quarto_root()
    }

    resolved <- ifelse(is_absolute, path, file.path(project_root, path))
  }

  if (root == "document") {
    if (is.null(document_dir)) {
      document_dir <- quarto_document_dir()
    }

    resolved <- ifelse(is_absolute, path, file.path(document_dir, path))
  }

  if (isTRUE(must_exist)) {
    missing_files <- resolved[!file.exists(resolved)]

    if (length(missing_files)) {
      stop(
        "The following path does not exist: ",
        paste(missing_files, collapse = ", "),
        call. = FALSE
      )
    }
  }

  resolved
}
