#' Scan an object and return a datascan report
#'
#' `scan()` dispatches on the class of `x` (S3) and returns a unified
#' report containing:
#' - `details`: descriptive summaries of the object
#' - `issues`: structured issues (info/warn/error) with suggestions
#' - `meta`: optional diagnostic info
#'
#' Supported types include data.frame/tibble, matrix, list, SummarizedExperiment, Seurat,
#' plus a default fallback for other objects.
#'
#' @param x Any R object.
#' @param ... Additional options forwarded to specific scanners.
#'
#' @return An object of class `datascan_report`.
#' @export
scan <- function(x, ...) {
  UseMethod("scan")
}

# ---- internal constructor -------------------------------------------------

#' @noRd
new_datascan_report <- function(type, class, details = list(), issues = NULL, meta = list()) {
  # issues: allow NULL or data.frame; we'll standardize later in issues.R too
  if (is.null(issues)) {
    issues <- data.frame(
      id = character(),
      severity = character(),
      message = character(),
      where = character(),
      suggestion = character(),
      stringsAsFactors = FALSE
    )
  }

  res <- list(
    type = type,
    class = class,
    details = details,
    issues = issues,
    meta = meta
  )

  structure(res, class = c("datascan_report", paste0("datascan_", type)))
}

# ---- default method -------------------------------------------------------

#' @export
scan.default <- function(x, ...) {
  details <- list(
    typeof = typeof(x),
    length = tryCatch(length(x), error = function(e) NA_integer_),
    dim = tryCatch(dim(x), error = function(e) NULL),
    names = tryCatch(names(x), error = function(e) NULL)
  )

  # 注意：这里不做“价值判断”，issues 留空
  meta <- list(
    notes = "No specific scanner for this class; returning basic structure.",
    type_guess = detect_type(x)
  )


  new_datascan_report(
    type = "default",
    class = class(x),
    details = details,
    issues = NULL,
    meta = meta
  )
}
