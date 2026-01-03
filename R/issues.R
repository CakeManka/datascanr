new_issues <- function() {
  data.frame(
    level = character(),
    code = character(),
    message = character(),
    evidence = character(),
    suggestion = character(),
    stringsAsFactors = FALSE
  )
}

add_issue <- function(issues, level, code, message,
                      evidence = "", suggestion = "") {
  issues[nrow(issues) + 1, ] <- list(level, code, message, evidence, suggestion)
  issues
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
