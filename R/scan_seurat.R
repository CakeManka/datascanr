#' @export
scan.Seurat <- function(x, ...) {
  if (!requireNamespace("SeuratObject", quietly = TRUE) &&
      !requireNamespace("Seurat", quietly = TRUE)) {
    stop("Package 'SeuratObject' or 'Seurat' is required to scan a Seurat object.")
  }
  
  issues <- new_issues()
  
  # 尽量只用 slot/通用函数，避免版本差异炸
  assays <- tryCatch(names(x@assays), error = function(e) character(0))
  meta_ncol <- tryCatch(ncol(x@meta.data), error = function(e) NA_integer_)
  default_assay <- tryCatch(SeuratObject::DefaultAssay(x), error = function(e) NA_character_)
  
  n_cells <- tryCatch(ncol(x), error = function(e) NA_integer_)
  n_features <- tryCatch(nrow(x), error = function(e) NA_integer_)
  
  reductions <- tryCatch(names(x@reductions), error = function(e) character(0))
  has_active_ident <- tryCatch(!is.null(x@active.ident), error = function(e) NA)
  
  # ---- details ----
  details <- list(
    assays = assays,
    n_assays = length(assays),
    default_assay = default_assay,
    n_cells = n_cells,
    n_features = n_features,
    meta_columns = meta_ncol,
    reductions = reductions,
    n_reductions = length(reductions),
    has_active_ident = has_active_ident
  )
  
  # ---- issues ----
  if (length(assays) == 0) {
    issues <- add_issue(
      issues,
      id = "seurat_no_assays",
      severity = "error",
      message = "No assays found in Seurat object.",
      where = "x@assays",
      suggestion = "Ensure you created the object properly (CreateSeuratObject) and did not drop assays."
    )
  }
  
  if (!is.na(n_cells) && !is.na(n_features) && (n_cells == 0 || n_features == 0)) {
    issues <- add_issue(
      issues,
      id = "seurat_empty",
      severity = "error",
      message = "Seurat object has 0 cells or 0 features.",
      where = paste0("n_features=", n_features, ", n_cells=", n_cells),
      suggestion = "Check filtering/subsetting; confirm raw counts matrix is non-empty."
    )
  }
  
  if (!is.na(default_assay) && nzchar(default_assay) &&
      length(assays) > 0 && !(default_assay %in% assays)) {
    issues <- add_issue(
      issues,
      id = "seurat_default_assay_missing",
      severity = "warn",
      message = "DefaultAssay is not present in assays list.",
      where = paste0("DefaultAssay=", default_assay),
      suggestion = "Set a valid DefaultAssay(x) or restore the missing assay."
    )
  }
  
  if (!is.na(meta_ncol) && meta_ncol == 0) {
    issues <- add_issue(
      issues,
      id = "seurat_metadata_empty",
      severity = "info",
      message = "meta.data has 0 columns (no sample/cell metadata).",
      where = "x@meta.data",
      suggestion = "Optional: add metadata columns (sample, batch, group, QC metrics) for downstream analysis."
    )
  }
  
  new_datascan_report(
    type = "Seurat",
    class = class(x),
    details = details,
    issues = issues,
    meta = list()
  )
}
