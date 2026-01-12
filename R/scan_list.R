#' @export
scan.list <- function(x, ...) {
  issues <- new_issues()
  
  n <- length(x)
  nm <- names(x)
  
  lens <- vapply(x, length, integer(1))
  cls  <- vapply(x, function(.x) class(.x)[1], character(1))
  
  # ---- details ----
  has_names <- !is.null(nm)
  empty_names_n <- if (!has_names) 0L else sum(!nzchar(nm))
  dup_names_n <- if (!has_names) 0L else sum(duplicated(nm[nzchar(nm)]))
  
  details <- list(
    length = n,
    has_names = has_names,
    empty_names = empty_names_n,
    duplicated_names = dup_names_n,
    element_classes = sort(table(cls), decreasing = TRUE),
    element_lengths_summary = summary(lens),
    n_null = sum(vapply(x, is.null, logical(1)))
  )
  
  # ---- issues ----
  if (n == 0) {
    issues <- add_issue(
      issues,
      id = "list_empty",
      severity = "warn",
      message = "List is empty (length = 0).",
      where = "list",
      suggestion = "Verify upstream steps; did you filter everything out or read an empty object?"
    )
  }
  
  if (!has_names) {
    issues <- add_issue(
      issues,
      id = "list_no_names",
      severity = "info",
      message = "List has no names; accessing elements by $ may be inconvenient.",
      where = "names(x)",
      suggestion = "Consider setting names(x) for clarity (e.g., names(x) <- ...)."
    )
  } else {
    if (empty_names_n > 0) {
      issues <- add_issue(
        issues,
        id = "list_empty_names",
        severity = "warn",
        message = "Empty element names detected (may hinder downstream access).",
        where = paste0("empty_names=", empty_names_n),
        suggestion = "Provide non-empty names for all elements."
      )
    }
    if (dup_names_n > 0) {
      issues <- add_issue(
        issues,
        id = "list_dup_names",
        severity = "warn",
        message = "Duplicate element names detected (may cause ambiguous access).",
        where = "names(x)",
        suggestion = "Make names unique (e.g., make.unique(names(x)))."
      )
    }
  }
  
  if (details$n_null > 0) {
    issues <- add_issue(
      issues,
      id = "list_has_nulls",
      severity = "info",
      message = "NULL elements detected in list.",
      where = paste0("n_null=", details$n_null),
      suggestion = "Consider removing NULLs (Filter(Negate(is.null), x)) if unintended."
    )
  }
  
  # performance heads-up (optional)
  if (n >= 100000) {
    issues <- add_issue(
      issues,
      id = "list_very_large",
      severity = "info",
      message = "Very large list detected; scanning/printing may be slow.",
      where = paste0("length=", n),
      suggestion = "Consider scanning a subset or summarizing upstream."
    )
  }
  
  new_datascan_report(
    type = "list",
    class = class(x),
    details = details,
    issues = issues,
    meta = list()
  )
}
