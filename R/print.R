ansi_ok <- function() {
  # RStudio/终端一般支持；不支持就自动退回
  isTRUE(getOption("cli.ansi", TRUE)) && isTRUE(interactive())
}

col_wrap <- function(txt, col) {
  if (!ansi_ok()) return(txt)
  paste0("\033[", col, "m", txt, "\033[0m")
}

badge <- function(severity) {
  sym <- switch(
    severity,
    error = intToUtf8(0x1F7E5),  # 🟥
    warn  = intToUtf8(0x1F7E8),  # 🟨
    info  = intToUtf8(0x2139),   # ℹ
    severity
  )

  if (ansi_ok()) {
    col <- switch(severity, error = "31", warn = "33", info = "34", "0")
    return(paste0("\033[", col, "m", sym, "\033[0m"))
  }
  sym
}

#' Print a datascan report
#'
#' @param x A datascan_report
#' @param ... Unused
#' @param top_n Number of issues to show.
#' @export
print.datascan_report <- function(x, ..., top_n = 5) {
  cat(col_wrap("datascanr", "1"), "\n")  # 1=粗体

  # ---- header ----
  type <- x$type %||% "unknown"
  cls  <- x$class %||% class(x)

  cat("- type :", type, "\n")
  if (length(cls) > 0) cat("- class:", paste(cls, collapse = ", "), "\n")

  # dims (if available)
  n <- x$details$nrow %||% (x$details$dim %||% c(NA, NA))[1]
  p <- x$details$ncol %||% (x$details$dim %||% c(NA, NA))[2]
  if (!is.na(n) && !is.na(p)) cat("- dim  :", n, "x", p, "\n")

  # optional: show a couple of common details if present
  if (!is.null(x$details$missing_overall)) {
    cat("- missing(overall):", sprintf("%.2f%%", 100 * x$details$missing_overall), "\n")
  }

  # ---- issues ----
  issues <- x$issues
  if (!is.data.frame(issues) || nrow(issues) == 0) {
    cat("- issues: 0\n")
    cat("\nTip: use `report_details(x)` to inspect details; `report_issues(x)` for issues table.\n")
    return(invisible(x))
  }

  # count by severity
  tab <- table(issues$severity)
  n_error <- if ("error" %in% names(tab)) unname(tab[["error"]]) else 0
  n_warn  <- if ("warn"  %in% names(tab)) unname(tab[["warn"]])  else 0
  n_info  <- if ("info"  %in% names(tab)) unname(tab[["info"]])  else 0

  cat("- issues:",
      badge("error"), n_error, " ",
      badge("warn"),  n_warn,  " ",
      badge("info"),  n_info, "\n", sep = "")

  # sort issues: error > warn > info
  sev_order <- c("error", "warn", "info")
  issues$severity <- factor(issues$severity, levels = sev_order, ordered = TRUE)
  issues <- issues[order(issues$severity), , drop = FALSE]

  show_n <- min(top_n, nrow(issues))
  top <- issues[seq_len(show_n), c("severity", "message", "where"), drop = FALSE]
  rownames(top) <- NULL

  cat("\nTop issues:\n")
  for (i in seq_len(nrow(top))) {
    cat(" ", badge(as.character(top$severity[i])), " ",
        top$message[i],
        if (nzchar(top$where[i])) paste0("  (", top$where[i], ")") else "",
        "\n", sep = "")
  }

  cat("\nTip: `report_details(x)` for details; `report_issues(x)` for all issues.\n")
  cat("Next: fix the fields shown in `(where)`; then rerun `scan()`.\n")
  invisible(x)
}
