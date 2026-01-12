#' Guess a coarse type tag for an object (helper)
#'
#' This is a lightweight helper used for diagnostics and fallback cases.
#' It does NOT control S3 dispatch (scan.* methods do that).
#'
#' @param x Any R object.
#' @return A list with `type_tag`, `class`, `dim`, and optional size fields.
#' @noRd
detect_type <- function(x) {
  cls <- class(x)
  d <- tryCatch(dim(x), error = function(e) NULL)

  type_tag <- "unknown"

  # --- common tabular types ---
  if (inherits(x, "tbl_df")) {
    type_tag <- "tibble"
  } else if (is.data.frame(x)) {
    type_tag <- "data.frame"
  } else if (is.matrix(x)) {
    type_tag <- "matrix"
  } else if (is.array(x) && !is.null(d) && length(d) > 2) {
    type_tag <- "array"
  } else if (inherits(x, "SummarizedExperiment")) {
    type_tag <- "SummarizedExperiment"
  } else if (inherits(x, "Seurat")) {
    type_tag <- "Seurat"

    # --- vectors / lists ---
  } else if (is.atomic(x) && is.null(d)) {
    type_tag <- "atomic_vector"
  } else if (is.list(x)) {
    type_tag <- "list"
  } else if (is.environment(x)) {
    type_tag <- "environment"
  } else if (is.function(x)) {
    type_tag <- "function"
  }

  out <- list(
    type_tag = type_tag,
    class = cls,
    dim = d
  )

  if (!is.null(d) && length(d) >= 2) {
    out$nrow <- d[[1]]
    out$ncol <- d[[2]]
  } else if (is.atomic(x) && is.null(d)) {
    out$length <- tryCatch(length(x), error = function(e) NA_integer_)
  } else if (is.list(x)) {
    out$length <- length(x)
    out$named <- !is.null(names(x)) && all(nzchar(names(x)))
  }

  out
}
