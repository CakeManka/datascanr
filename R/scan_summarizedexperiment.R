#' @export
scan.SummarizedExperiment <- function(x, ...) {
  # 允许对象存在但包没装的情况：尽量给出友好错误
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Package 'SummarizedExperiment' is required to scan this object.")
  }
  
  issues <- new_issues()
  
  row_n <- tryCatch(nrow(x), error = function(e) NA_integer_)
  col_n <- tryCatch(ncol(x), error = function(e) NA_integer_)
  
  assays_obj <- tryCatch(SummarizedExperiment::assays(x), error = function(e) NULL)
  assays_n <- if (is.null(assays_obj)) NA_integer_ else length(assays_obj)
  assay_names <- if (is.null(assays_obj)) character(0) else names(assays_obj)
  
  rowdata_p <- tryCatch(ncol(SummarizedExperiment::rowData(x)), error = function(e) NA_integer_)
  coldata_p <- tryCatch(ncol(SummarizedExperiment::colData(x)), error = function(e) NA_integer_)
  
  # pick a primary assay for quick diagnostics (first one)
  primary_assay <- NULL
  if (!is.null(assays_obj) && length(assays_obj) >= 1) {
    primary_assay <- assays_obj[[1]]
  }
  primary_dim <- tryCatch(dim(primary_assay), error = function(e) NULL)
  
  # ---- details ----
  details <- list(
    nrow = row_n,
    ncol = col_n,
    n_assays = assays_n,
    assays = assay_names,
    rowData_ncol = rowdata_p,
    colData_ncol = coldata_p,
    primary_assay_class = if (is.null(primary_assay)) NA_character_ else class(primary_assay)[1],
    primary_assay_dim = primary_dim
  )
  
  # ---- issues ----
  if (!is.na(row_n) && !is.na(col_n) && (row_n == 0 || col_n == 0)) {
    issues <- add_issue(
      issues,
      id = "se_empty_dims",
      severity = "error",
      message = "SummarizedExperiment has empty dimensions (0 rows or 0 cols).",
      where = paste0("nrow=", row_n, ", ncol=", col_n),
      suggestion = "Verify filtering/subsetting steps; check object construction."
    )
  }
  
  if (!is.na(assays_n) && assays_n == 0) {
    issues <- add_issue(
      issues,
      id = "se_no_assays",
      severity = "error",
      message = "No assays found in SummarizedExperiment.",
      where = "assays(x)",
      suggestion = "Ensure assay data was attached (e.g., assays= list(counts=...))."
    )
  }
  
  if (!is.na(rowdata_p) && rowdata_p == 0) {
    issues <- add_issue(
      issues,
      id = "se_rowdata_empty",
      severity = "info",
      message = "rowData is empty (0 columns).",
      where = "rowData(x)",
      suggestion = "Optional: add feature annotations (gene_id, gene_name, etc.)."
    )
  }
  
  if (!is.na(coldata_p) && coldata_p == 0) {
    issues <- add_issue(
      issues,
      id = "se_coldata_empty",
      severity = "info",
      message = "colData is empty (0 columns).",
      where = "colData(x)",
      suggestion = "Optional: add sample metadata (group, batch, patient_id, etc.)."
    )
  }
  
  # assay dimension mismatch check (important)
  if (!is.null(primary_dim) && length(primary_dim) >= 2 &&
      !is.na(row_n) && !is.na(col_n)) {
    if (primary_dim[1] != row_n || primary_dim[2] != col_n) {
      issues <- add_issue(
        issues,
        id = "se_assay_dim_mismatch",
        severity = "error",
        message = "Primary assay dimensions do not match SummarizedExperiment dimensions.",
        where = paste0("assay_dim=", primary_dim[1], "x", primary_dim[2],
                       " vs se_dim=", row_n, "x", col_n),
        suggestion = "Check how the assay was attached; ensure rows/features and cols/samples align."
      )
    }
  }
  
  # optional: non-finite values in numeric assay
  if (!is.null(primary_assay) && is.numeric(primary_assay)) {
    if (any(is.infinite(primary_assay), na.rm = TRUE) || any(is.nan(primary_assay), na.rm = TRUE)) {
      issues <- add_issue(
        issues,
        id = "se_assay_nonfinite",
        severity = "warn",
        message = "Inf or NaN detected in primary assay (may break downstream analysis).",
        where = "primary_assay",
        suggestion = "Trace the source and replace/remove non-finite values."
      )
    }
  }
  
  new_datascan_report(
    type = "SummarizedExperiment",
    class = class(x),
    details = details,
    issues = issues,
    meta = list()
  )
}
