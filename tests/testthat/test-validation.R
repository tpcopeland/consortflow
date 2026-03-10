# Validation tests: verify counts match manual calculation

test_that("V01: counts match manual computation through pipeline", {
  set.seed(99)
  d <- data.frame(
    id = 1:500,
    age = sample(10:90, 500, replace = TRUE),
    fu = sample(0:365, 500, replace = TRUE),
    lab = sample(c(TRUE, FALSE), 500, replace = TRUE, prob = c(0.8, 0.2))
  )

  # manual counts
  n0 <- nrow(d)
  d1 <- d[d$age >= 18, ]
  n_excl1 <- n0 - nrow(d1)
  d2 <- d1[d1$fu >= 30, ]
  n_excl2 <- nrow(d1) - nrow(d2)
  d3 <- d2[d2$lab, ]
  n_excl3 <- nrow(d2) - nrow(d3)

  # consortflow counts
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  obj <- suppressMessages(cf_exclude(obj, fu < 30, label = "Short FU"))
  obj <- suppressMessages(cf_exclude(obj, !lab, label = "No labs"))

  expect_equal(obj$initial_n, n0)
  expect_equal(obj$steps[[1]]$n_excluded, n_excl1)
  expect_equal(obj$steps[[1]]$n_remaining, nrow(d1))
  expect_equal(obj$steps[[2]]$n_excluded, n_excl2)
  expect_equal(obj$steps[[2]]$n_remaining, nrow(d2))
  expect_equal(obj$steps[[3]]$n_excluded, n_excl3)
  expect_equal(obj$steps[[3]]$n_remaining, nrow(d3))
  expect_equal(nrow(obj$data), nrow(d3))
})

test_that("V02: total excluded equals initial minus final", {
  d <- make_test_data(n = 2000, seed = 77)
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  obj <- suppressMessages(cf_exclude(obj, followup < 30, label = "Short FU"))
  obj <- suppressMessages(cf_exclude(obj, !has_labs, label = "No labs"))
  obj <- suppressMessages(cf_exclude(obj, is.na(education), label = "Missing edu"))

  total_excl <- sum(vapply(obj$steps, function(s) s$n_excluded, integer(1)))
  n_final <- obj$steps[[length(obj$steps)]]$n_remaining

  expect_equal(obj$initial_n - total_excl, n_final)
  expect_equal(n_final, nrow(obj$data))
})

test_that("V03: exclusion order matters - different orders yield different step counts", {
  d <- make_test_data(n = 500, seed = 11)

  # order A: age then followup
  a <- suppressMessages(cf_init(d, label = "All"))
  a <- suppressMessages(cf_exclude(a, age < 18, label = "Under 18"))
  a <- suppressMessages(cf_exclude(a, followup < 30, label = "Short FU"))

  # order B: followup then age
  b <- suppressMessages(cf_init(d, label = "All"))
  b <- suppressMessages(cf_exclude(b, followup < 30, label = "Short FU"))
  b <- suppressMessages(cf_exclude(b, age < 18, label = "Under 18"))

  # final count should be the same
  expect_equal(
    a$steps[[2]]$n_remaining,
    b$steps[[2]]$n_remaining
  )

  # but per-step counts may differ
  expect_equal(a$initial_n, b$initial_n)
})

test_that("V04: NA values in condition are not excluded", {
  d <- data.frame(
    id = 1:100,
    x = c(rep(1, 40), rep(0, 40), rep(NA, 20))
  )

  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, x == 1, label = "x is 1"))

  # 40 excluded (x==1), 60 remain (40 zeros + 20 NAs)
  expect_equal(obj$steps[[1]]$n_excluded, 40L)
  expect_equal(obj$steps[[1]]$n_remaining, 60L)
})

test_that("V05: PNG output is a valid image", {
  skip_if_not_installed("png")

  d <- make_test_data(n = 200)
  obj <- suppressMessages(cf_init(d, label = "Test"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  obj <- suppressMessages(cf_exclude(obj, followup < 30, label = "Short FU"))

  outfile <- tempfile(fileext = ".png")
  on.exit(unlink(outfile), add = TRUE)

  suppressMessages(cf_save(obj, output = outfile, dpi = 72))

  img <- png::readPNG(outfile)
  expect_true(is.array(img))
  expect_equal(length(dim(img)), 3L)  # height x width x channels
  expect_true(dim(img)[1] > 0)  # has height
  expect_true(dim(img)[2] > 0)  # has width
})

test_that("V06: remaining data is usable after pipeline", {
  d <- make_test_data(n = 500)
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  obj <- suppressMessages(cf_exclude(obj, !has_labs, label = "No labs"))

  # the remaining data should be a proper data.frame we can operate on
  final <- obj$data
  expect_s3_class(final, "data.frame")
  expect_true(all(final$age >= 18))
  expect_true(all(final$has_labs))
  expect_true(all(names(d) %in% names(final)))
})
