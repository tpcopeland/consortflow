test_that("cf_init creates valid consortflow object", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All patients"))

  expect_s3_class(obj, "consortflow")
  expect_equal(obj$initial_n, 1000L)
  expect_equal(obj$initial_label, "All patients")
  expect_equal(nrow(obj$data), 1000L)
  expect_length(obj$steps, 0L)
})

test_that("cf_init uses default label", {
  d <- make_test_data(n = 50)
  obj <- suppressMessages(cf_init(d))
  expect_equal(obj$initial_label, "Initial population")
})

test_that("cf_init rejects non-data-frame", {
  expect_error(cf_init(list(a = 1)), "must be a data frame")
  expect_error(cf_init("not a df"), "must be a data frame")
})

test_that("cf_init rejects empty data frame", {
  d <- data.frame(x = integer(0))
  expect_error(cf_init(d), "0 rows")
})

test_that("cf_init rejects non-string label", {
  d <- make_test_data(n = 10)
  expect_error(cf_init(d, label = 123), "single character string")
  expect_error(cf_init(d, label = c("a", "b")), "single character string")
})

test_that("cf_init prints message with count", {
  d <- make_test_data(n = 100)
  expect_message(cf_init(d, label = "Test"), "100 observations")
})

test_that("print.consortflow displays correctly", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "Source"))
  out <- capture.output(print(obj))
  expect_true(any(grepl("1,000", out)))
  expect_true(any(grepl("Source", out)))
  expect_true(any(grepl("Steps:.*0", out)))
})

test_that("cf_init coerces non-plain data.frames", {
  d <- make_test_data(n = 50)
  # simulate a tibble-like class
  class(d) <- c("tbl_df", "tbl", "data.frame")
  obj <- suppressMessages(cf_init(d, label = "Test"))
  expect_identical(class(obj$data), "data.frame")
})

test_that("summary.consortflow returns empty data.frame with no steps", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  s <- summary(obj)
  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 0L)
  expect_true("label" %in% names(s))
})

test_that("summary.consortflow returns correct step table", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18",
                                     remaining = "Adults"))
  obj <- suppressMessages(cf_exclude(obj, !has_labs, label = "No labs"))

  s <- summary(obj)
  expect_equal(nrow(s), 2L)
  expect_equal(s$step, 1:2)
  expect_equal(s$label, c("Under 18", "No labs"))
  expect_equal(s$remaining_label[1], "Adults")
  expect_true(is.na(s$remaining_label[2]))
  expect_equal(s$n_remaining[2], nrow(obj$data))
})

test_that("plot.consortflow works", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 8, height = 6, units = "in", res = 72)
  expect_no_error(plot(obj))
  grDevices::dev.off()
})
