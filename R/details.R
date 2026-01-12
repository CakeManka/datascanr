#' Extract details from a datascan report
#'
#' @param report A `datascan_report`.
#' @return The `details` list stored in the report.
#' @export
report_details <- function(report) {
  stopifnot(inherits(report, "datascan_report"))
  report$details
}

#' Extract issues from a datascan report
#'
#' @param report A `datascan_report`.
#' @return A data.frame of issues.
#' @export
report_issues <- function(report) {
  stopifnot(inherits(report, "datascan_report"))
  report$issues
}

#' Extract meta from a datascan report
#'
#' @param report A `datascan_report`.
#' @return The `meta` list stored in the report.
#' @export
report_meta <- function(report) {
  stopifnot(inherits(report, "datascan_report"))
  report$meta
}

# ---- internal helpers used by scan.* methods ------------------------------

#' @noRd
details_dim <- function(x) {
  d <- tryCatch(dim(x), error = function(e) NULL)
  if (is.null(d)) return(list(dim = NULL))
  out <- list(dim = d)
  if (length(d) >= 2) {
    out$nrow <- d[[1]]
    out$ncol <- d[[2]]
  }
  out
}

#' @noRd
details_names <- function(x) {
  nm <- tryCatch(names(x), error = function(e) NULL)
  list(
    has_names = !is.null(nm),
    n_names = if (is.null(nm)) 0L else length(nm),
    any_empty_names = if (is.null(nm)) FALSE else any(!nzchar(nm)),
    any_duplicated_names = if (is.null(nm)) FALSE else any(duplicated(nm))
  )
}

#' @noRd
details_colnames_rownames <- function(x) {
  rn <- tryCatch(rownames(x), error = function(e) NULL)
  cn <- tryCatch(colnames(x), error = function(e) NULL)
  list(
    has_rownames = !is.null(rn),
    has_colnames = !is.null(cn),
    duplicated_rownames = !is.null(rn) && any(duplicated(rn)),
    duplicated_colnames = !is.null(cn) && any(duplicated(cn))
  )
}
