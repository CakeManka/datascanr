detect_type <- function(x) {
  cls <- class(x)
  d <- dim(x)

  type_tag <- "unknown"
  if (is.data.frame(x)) {
    type_tag <- "data_frame"
  } else if (is.matrix(x)) {
    type_tag <- "matrix"
  } else if (is.array(x) && !is.null(d) && length(d) > 2) {
    type_tag <- "array"
  } else if (is.atomic(x) && is.null(d)) {
    type_tag <- "vector"
  } else if (is.list(x)) {
    type_tag <- "list"
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
    out$length <- length(x)
  }

  out
}
