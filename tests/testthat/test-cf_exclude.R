test_that("cf_exclude drops correct rows", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  n_young <- sum(d$age < 18)

  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))

  expect_length(obj$steps, 1L)
  expect_equal(obj$steps[[1]]$n_excluded, n_young)
  expect_equal(obj$steps[[1]]$n_remaining, 1000L - n_young)
  expect_equal(nrow(obj$data), 1000L - n_young)
  expect_true(all(obj$data$age >= 18))
})

test_that("cf_exclude handles multiple sequential steps", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))

  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  obj <- suppressMessages(cf_exclude(obj, followup < 30, label = "Short FU"))
  obj <- suppressMessages(cf_exclude(obj, !has_labs, label = "Missing labs"))

  expect_length(obj$steps, 3L)
  # counts should be monotonically decreasing
  remaining <- vapply(obj$steps, function(s) s$n_remaining, integer(1))
  expect_true(all(diff(remaining) <= 0))
  expect_equal(nrow(obj$data), remaining[3])
})

test_that("cf_exclude records remaining_label", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(
    cf_exclude(obj, age < 18, label = "Under 18", remaining = "Adults")
  )
  expect_equal(obj$steps[[1]]$remaining_label, "Adults")
})

test_that("cf_exclude with no matches excludes zero", {
  d <- data.frame(id = 1:10, age = rep(25L, 10))
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))

  expect_equal(obj$steps[[1]]$n_excluded, 0L)
  expect_equal(obj$steps[[1]]$n_remaining, 10L)
  expect_equal(nrow(obj$data), 10L)
})

test_that("cf_exclude treats NA condition as keep", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  n_missing_edu <- sum(is.na(d$education))

  # education == "primary" is TRUE for primary, FALSE for others, NA for missing
  # rows with NA education should NOT be excluded
  obj <- suppressMessages(
    cf_exclude(obj, education == "primary", label = "Primary education")
  )

  n_primary <- sum(d$education == "primary", na.rm = TRUE)
  expect_equal(obj$steps[[1]]$n_excluded, n_primary)
})

test_that("cf_exclude rejects non-consortflow object", {
  expect_error(cf_exclude(list(), age < 18, label = "x"), "consortflow object")
})

test_that("cf_exclude rejects bad label", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  expect_error(
    suppressMessages(cf_exclude(obj, age < 18, label = 99)),
    "single character string"
  )
})

test_that("cf_exclude prints message with counts", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  expect_message(
    cf_exclude(obj, age < 18, label = "Under 18"),
    "excluded"
  )
})

test_that("cf_exclude preserves other columns", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  expect_equal(names(obj$data), names(d))
})

test_that("cf_exclude gives contextual error for bad column", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  expect_error(
    cf_exclude(obj, nonexistent_col < 5, label = "Bad col"),
    "Bad col"
  )
})

test_that("cf_exclude rejects recycled mask", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  expect_error(
    cf_exclude(obj, c(TRUE, FALSE), label = "Recycled"),
    "does not match"
  )
})
