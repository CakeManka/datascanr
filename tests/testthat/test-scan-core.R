test_that("scan returns unified report structure", {
  r <- scan(data.frame(a = 1:3))
  expect_s3_class(r, "datascan_report")
  expect_true(all(c("type", "class", "details", "issues", "meta") %in% names(r)))
  expect_true(is.list(r$details))
  expect_true(is.data.frame(r$issues))
})

test_that("data.frame issues are detected", {
  df <- data.frame(a = c(1, NA, 3), b = c("1", "2", "3"), stringsAsFactors = FALSE)
  r <- scan(df)
  expect_true(any(r$issues$id %in% c("df_missing_high", "df_missing_very_high")))
})

test_that("matrix issues are detected", {
  m <- matrix(c(1, NA, 3, 4), nrow = 2)
  rownames(m) <- c("g1", "g1")
  r <- scan(m)
  expect_true(any(r$issues$id == "matrix_dup_rownames"))
})

test_that("list issues are detected", {
  r <- scan(list(a = 1:3, b = NULL))
  expect_true(any(r$issues$id == "list_has_nulls"))
})
