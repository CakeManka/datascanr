#' Create an empty issues table
#'
#' @return A data.frame with standardized issue columns.
#' @noRd
new_issues <- function() {
  data.frame(
    id = character(),
    severity = character(),     # "info" | "warn" | "error"
    message = character(),
    where = character(),        # column/slot/assay/element location
    suggestion = character(),
    stringsAsFactors = FALSE
  )
}

#' Add one issue row into an issues table
#'
#' @param issues A data.frame returned by `new_issues()`.
#' @param id A short machine-readable identifier (e.g., "missing_values").
#' @param severity One of "info", "warn", "error".
#' @param message Human-readable message.
#' @param where Where the issue occurs (e.g., column name); can be "".
#' @param suggestion Suggested fix; can be "".
#' @return Updated issues data.frame.
#' @noRd
add_issue <- function(issues,
                      id,
                      severity = c("warn", "info", "error"),
                      message,
                      where = "",
                      suggestion = "") {
  severity <- match.arg(severity)
  if (is.null(issues)) issues <- new_issues()

  issues[nrow(issues) + 1, ] <- list(
    as.character(id),
    as.character(severity),
    as.character(message),
    as.character(where %||% ""),
    as.character(suggestion %||% "")
  )
  issues
}

#' Combine multiple issues tables
#'
#' @param ... issues data.frames (or NULL)
#' @return A single issues data.frame.
#' @noRd
bind_issues <- function(...) {
  xs <- list(...)
  xs <- xs[!vapply(xs, is.null, logical(1))]
  if (length(xs) == 0) return(new_issues())
  # ensure same columns
  for (i in seq_along(xs)) {
    miss <- setdiff(names(new_issues()), names(xs[[i]]))
    if (length(miss) > 0) xs[[i]][miss] <- ""
    xs[[i]] <- xs[[i]][names(new_issues())]
  }
  do.call(rbind, xs)
}

#' Null coalescing operator
#'
#' @param x,y objects
#' @return y if x is NULL else x
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
