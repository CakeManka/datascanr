#' Scan a tibble (tbl_df)
#'
#' Tibble inherits from data.frame, but we keep a dedicated method so that
#' tibble-specific quirks can be handled and debugged independently.
#'
#' @param x A tibble object.
#' @param ... Passed to underlying scanner.
#'
#' @return A `datascan_report`.
#' @keywords internal
#' @export
scan.tbl_df <- function(x, ...) {
  # 先复用 data.frame 的核心检查（S3 推荐用 NextMethod）
  res <- NextMethod()
  
  # 强制标记类型
  res$type <- "tibble"
  
  # tibble-specific details
  is_list_col <- vapply(x, is.list, logical(1))
  list_cols <- names(x)[is_list_col]
  
  res$details$tibble_class <- class(x)
  res$details$has_list_col <- any(is_list_col)
  res$details$list_cols <- list_cols
  
  # 如果 list-column 特别多，升一级提示（可选规则）
  if (length(list_cols) >= 3) {
    res$issues <- add_issue(
      res$issues,
      id = "tibble_many_list_columns",
      severity = "warn",
      message = "Many list-columns detected in tibble; downstream modeling/export may fail.",
      where = paste(list_cols, collapse = ", "),
      suggestion = "Consider unnesting or extracting scalar values before exporting/modeling."
    )
  }
  
  # 给一个更具体的子类（可选，但很好用）
  class(res) <- unique(c("datascan_tibble", class(res)))
  res
}
