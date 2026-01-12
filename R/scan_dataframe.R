bucket_type <- function(v) {
  if (inherits(v, c("POSIXct", "POSIXt"))) return("datetime")
  if (inherits(v, "Date")) return("date")
  if (is.factor(v)) return("factor")
  if (is.logical(v)) return("logical")
  if (is.integer(v)) return("integer")
  if (is.numeric(v)) return("numeric")
  if (is.character(v)) return("character")
  if (is.list(v)) return("list") # tibble/list-column 常见
  "other"
}

suspect_numeric_text <- function(v, threshold = 0.9) {
  if (!(is.character(v) || is.factor(v))) return(FALSE)
  vv <- as.character(v)
  vv <- vv[!is.na(vv)]
  if (length(vv) == 0) return(FALSE)

  cleaned <- gsub(",", "", trimws(vv))
  suppressWarnings(num <- as.numeric(cleaned))
  mean(!is.na(num)) >= threshold
}

#' @export
scan.data.frame <- function(x, ..., suspect_threshold = 0.9) {
  df <- x
  issues <- new_issues()

  n <- nrow(df)
  p <- ncol(df)
  cn <- colnames(df) %||% rep("", p)

  # ---- details ----
  details <- c(
    list(
      nrow = n,
      ncol = p,
      missing_overall = if (n * p > 0) sum(is.na(df)) / (n * p) else NA_real_
    ),
    details_names(as.list(setNames(vector("list", p), cn))) # 仅借用 names helper
  )

  # ---- Empty data frame ----
  if (n == 0 || p == 0) {
    issues <- add_issue(
      issues,
      id = "df_empty",
      severity = "error",
      message = "Data frame is empty (0 rows or 0 columns).",
      where = paste0("nrow=", n, ", ncol=", p),
      suggestion = "Check data import steps (delimiter, header, file path)."
    )
  }

  # ---- Column names ----
  if (any(cn == "")) {
    issues <- add_issue(
      issues,
      id = "df_colname_missing",
      severity = "warn",
      message = "Empty column names detected (may break selection or joins).",
      where = paste0("empty_names=", sum(cn == "")),
      suggestion = "Provide unique column names (e.g., make.names or manual renaming)."
    )
  }

  dup_cn <- cn[duplicated(cn) & cn != ""]
  if (length(dup_cn) > 0) {
    issues <- add_issue(
      issues,
      id = "df_colname_dup",
      severity = "warn",
      message = "Duplicate column names detected (common source of errors).",
      where = paste(unique(dup_cn), collapse = ", "),
      suggestion = "Make column names unique (e.g., make.unique(colnames(df)))."
    )
  }

  # ---- Missing values by column ----
  na_mat <- is.na(df)
  miss_by_col <- data.frame(
    colname = cn,
    na_count = colSums(na_mat),
    na_rate  = if (n > 0) colMeans(na_mat) else rep(NA_real_, p),
    stringsAsFactors = FALSE
  )
  miss_by_col <- miss_by_col[order(-miss_by_col$na_rate), , drop = FALSE]

  if (any(miss_by_col$na_rate >= 0.8, na.rm = TRUE)) {
    bad <- miss_by_col$colname[miss_by_col$na_rate >= 0.8]
    issues <- add_issue(
      issues,
      id = "df_missing_very_high",
      severity = "error",
      message = "Columns with missing rate >= 80% detected.",
      where = paste(bad, collapse = ", "),
      suggestion = "Consider dropping these columns or verify import/encoding."
    )
  } else if (any(miss_by_col$na_rate >= 0.1, na.rm = TRUE)) {
    bad <- miss_by_col$colname[miss_by_col$na_rate >= 0.1]
    issues <- add_issue(
      issues,
      id = "df_missing_high",
      severity = "warn",
      message = "Columns with missing rate >= 10% detected.",
      where = paste(bad, collapse = ", "),
      suggestion = "Handle missing values (drop, impute, or report in Methods)."
    )
  }

  # ---- Type profile ----
  type_profile <- data.frame(
    colname = cn,
    class = vapply(df, function(v) paste(class(v), collapse = "/"), character(1)),
    bucket = vapply(df, bucket_type, character(1)),
    n_unique = vapply(df, function(v) length(unique(v[!is.na(v)])), integer(1)),
    stringsAsFactors = FALSE
  )
  type_profile$unique_ratio <- if (n > 0) type_profile$n_unique / n else NA_real_
  type_profile$suspect_numeric_text <- vapply(
    df, suspect_numeric_text, logical(1), threshold = suspect_threshold
  )

  # list-columns (tibble常见)
  list_cols <- type_profile$colname[type_profile$bucket == "list"]
  if (length(list_cols) > 0) {
    issues <- add_issue(
      issues,
      id = "df_list_columns",
      severity = "info",
      message = "List-columns detected (common in tibbles; may break some exporters/models).",
      where = paste(list_cols, collapse = ", "),
      suggestion = "Consider unnesting, extracting scalar fields, or converting to character."
    )
  }

  suspect_cols <- type_profile$colname[type_profile$suspect_numeric_text]
  if (length(suspect_cols) > 0) {
    issues <- add_issue(
      issues,
      id = "df_type_suspect_numeric_text",
      severity = "warn",
      message = "Columns look numeric but are stored as text or factor.",
      where = paste(suspect_cols, collapse = ", "),
      suggestion = "Clean text (trim spaces/commas) and convert to numeric."
    )
  }

  # Constant columns
  const_cols <- type_profile$colname[type_profile$n_unique <= 1]
  if (length(const_cols) > 0) {
    issues <- add_issue(
      issues,
      id = "df_constant_col",
      severity = "warn",
      message = "Constant columns detected (no information content).",
      where = paste(const_cols, collapse = ", "),
      suggestion = "Consider dropping constant columns or verify import."
    )
  }

  # Inf / NaN in numeric columns
  num_cols <- which(type_profile$bucket %in% c("numeric", "integer"))
  if (length(num_cols) > 0) {
    inf_cols <- character()
    for (j in num_cols) {
      v <- df[[j]]
      if (any(is.infinite(v), na.rm = TRUE) || any(is.nan(v), na.rm = TRUE)) {
        inf_cols <- c(inf_cols, cn[[j]])
      }
    }
    if (length(inf_cols) > 0) {
      issues <- add_issue(
        issues,
        id = "df_num_inf_nan",
        severity = "warn",
        message = "Inf or NaN detected in numeric columns (may break statistics).",
        where = paste(unique(inf_cols), collapse = ", "),
        suggestion = "Trace the source (divide-by-zero or overflow) and fix before analysis."
      )
    }
  }

  # ---- attach tables into details (so users can inspect) ----
  details$tables <- list(
    missing_by_col = miss_by_col,
    type_profile = type_profile
  )
  details$n_suspect_numeric_text <- sum(type_profile$suspect_numeric_text, na.rm = TRUE)

  # ---- build report ----
  type <- if (inherits(df, "tbl_df")) "tibble" else "data.frame"
  new_datascan_report(
    type = type,
    class = class(df),
    details = details,
    issues = issues,
    meta = list()
  )
}
