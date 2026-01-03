ansi_ok <- function() {
  # RStudio 终端一般支持；不支持就自动退回
  isTRUE(getOption("cli.ansi", TRUE)) && isTRUE(interactive())
}

col_wrap <- function(txt, col) {
  if (!ansi_ok()) return(txt)
  paste0("\033[", col, "m", txt, "\033[0m")
}

badge <- function(level) {
  sym <- switch(
    level,
    red    = intToUtf8(0x1F7E5),  # 🟥
    yellow = intToUtf8(0x1F7E8),  # 🟨
    info   = intToUtf8(0x2139),   # ℹ
    level
  )

  if (ansi_ok()) {
    col <- switch(level, red = "31", yellow = "33", info = "34", "0")
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
  ...
}

print.datascan_report <- function(x, ..., top_n = 5) {
  cat(col_wrap("datascanr", "1"), "\n")  # 1=粗体

  type <- x$input$type_tag %||% "unknown"
  n <- x$summary$nrow %||% x$input$nrow
  p <- x$summary$ncol %||% x$input$ncol

  cat("- type:", type, "\n")
  if (!is.null(n) && !is.null(p)) cat("- dim :", n, "x", p, "\n")
  if (!is.null(x$summary$missing_overall)) {
    cat("- missing(overall):", sprintf("%.2f%%", 100 * x$summary$missing_overall), "\n")
  }

  issues <- x$issues
  if (!is.data.frame(issues) || nrow(issues) == 0) {
    cat("- issues: 0\n")
    cat("\nTip: use `details(x)` to see tables (missing_by_col/type_profile)\n")
    return(invisible(x))
  }

  # 计数 + 彩色徽章
  tab <- table(issues$level)
  red <- if ("red" %in% names(tab)) unname(tab[["red"]]) else 0
  yellow <- if ("yellow" %in% names(tab)) unname(tab[["yellow"]]) else 0
  info <- if ("info" %in% names(tab)) unname(tab[["info"]]) else 0

  cat("- issues:",
      badge("red"), red, " ",
      badge("yellow"), yellow, " ",
      badge("info"), info, "\n", sep = "")

  # Top issues：不再打印 level 字段，用徽章代替
  show_n <- min(top_n, nrow(issues))
  top <- issues[seq_len(show_n), c("level", "message", "evidence"), drop = FALSE]
  rownames(top) <- NULL

  cat("\nTop issues:\n")
  for (i in seq_len(nrow(top))) {
    cat(" ", badge(top$level[i]), " ",
        top$message[i],
        if (nzchar(top$evidence[i])) paste0("  (", top$evidence[i], ")") else "",
        "\n", sep = "")
  }

  cat("\nTip: `details(x)` for tables; `x$issues` for all issues.\n")
  cat("\nNext: fix columns shown in evidence; then rerun scan().\n")
  invisible(x)
}
