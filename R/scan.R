#' Scan an R object and report potential data issues
#'
#' @param x An R object (e.g. data.frame, vector)
#' @param ... Reserved for future use
#'
#' @return A datascan_report object
#' @export
scan <- function(x, ...) {
  info <- detect_type(x)

  report <- switch(
    info$type_tag,
    data_frame = scan_dataframe(x, ...),
    list(input = info, summary = list(), tables = list(), issues = new_issues())
  )

  report$input <- utils::modifyList(info, report$input)
  class(report) <- "datascan_report"
  report
}
