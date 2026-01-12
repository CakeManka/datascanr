#' @export
scan.matrix <- function(x, ...) {
  issues <- new_issues()
  
  n <- nrow(x)
  p <- ncol(x)
  
  # ---- details ----
  storage <- typeof(x)
  miss_n <- sum(is.na(x))
  miss_rate <- if (n * p > 0) miss_n / (n * p) else NA_real_
  
  rn <- rownames(x)
  cn <- colnames(x)
  
  details <- list(
    nrow = n,
    ncol = p,
    typeof = storage,
    is_numeric = is.numeric(x),
    missing = miss_n,
    missing_rate = miss_rate,
    has_rownames = !is.null(rn),
    has_colnames = !is.null(cn),
    duplicated_rownames = !is.null(rn) && any(duplicated(rn)),
    duplicated_colnames = !is.null(cn) && any(duplicated(cn)),
    min = suppressWarnings(if (is.numeric(x)) min(x, na.rm = TRUE) else NA_real_),
    max = suppressWarnings(if (is.numeric(x)) max(x, na.rm = TRUE) else NA_real_)
  )
  
  # 可选：0 的比例（对表达矩阵很常用）
  if (is.numeric(x) && n * p > 0) {
    details$zero_rate <- mean(x == 0, na.rm = TRUE)
  }
  
  # ---- issues ----
  if (n == 0 || p == 0) {
    issues <- add_issue(
      issues,
      id = "matrix_empty",
      severity = "error",
      message = "Matrix is empty (0 rows or 0 columns).",
      where = paste0("nrow=", n, ", ncol=", p),
      suggestion = "Check data import/creation; verify dimensions before analysis."
    )
  }
  
  if (!is.na(miss_rate)) {
    if (miss_rate >= 0.8) {
      issues <- add_issue(
        issues,
        id = "matrix_missing_very_high",
        severity = "error",
        message = "Matrix missing rate >= 80% detected.",
        where = sprintf("missing_rate=%.2f%%", 100 * miss_rate),
        suggestion = "Verify import/encoding or consider filtering/imputing."
      )
    } else if (miss_rate >= 0.1) {
      issues <- add_issue(
        issues,
        id = "matrix_missing_high",
        severity = "warn",
        message = "Matrix missing rate >= 10% detected.",
        where = sprintf("missing_rate=%.2f%%", 100 * miss_rate),
        suggestion = "Consider filtering/imputing missing values or documenting in Methods."
      )
    }
  }
  
  if (details$duplicated_rownames) {
    issues <- add_issue(
      issues,
      id = "matrix_dup_rownames",
      severity = "warn",
      message = "Duplicate rownames detected (may break joins/subsetting).",
      where = "rownames",
      suggestion = "Use make.unique(rownames(x)) or fix upstream identifiers."
    )
  }
  
  if (details$duplicated_colnames) {
    issues <- add_issue(
      issues,
      id = "matrix_dup_colnames",
      severity = "warn",
      message = "Duplicate colnames detected (may break joins/subsetting).",
      where = "colnames",
      suggestion = "Use make.unique(colnames(x)) or fix upstream identifiers."
    )
  }
  
  # Inf/NaN (only meaningful for numeric)
  if (is.numeric(x) && n * p > 0) {
    if (any(is.infinite(x), na.rm = TRUE) || any(is.nan(x), na.rm = TRUE)) {
      issues <- add_issue(
        issues,
        id = "matrix_nonfinite",
        severity = "warn",
        message = "Inf or NaN detected in numeric matrix (may break statistics).",
        where = "matrix",
        suggestion = "Trace the source (divide-by-zero/overflow) and fix before analysis."
      )
    }
  }
  
  new_datascan_report(
    type = "matrix",
    class = class(x),
    details = details,
    issues = issues,
    meta = list()
  )
}
