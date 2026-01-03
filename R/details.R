details <- function(report) {
  stopifnot(inherits(report, "datascan_report"))
  list(
    issues = report$issues,
    tables = report$tables,
    summary = report$summary
  )
}
