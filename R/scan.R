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
