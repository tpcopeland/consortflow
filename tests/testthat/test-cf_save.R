test_that("cf_save creates a PNG file", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All patients"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  obj <- suppressMessages(cf_exclude(obj, followup < 30, label = "Short FU"))

  outfile <- tempfile(fileext = ".png")
  on.exit(unlink(outfile), add = TRUE)

  result <- suppressMessages(
    cf_save(obj, output = outfile, final = "Study cohort", dpi = 72)
  )

  expect_true(file.exists(outfile))
  expect_true(file.size(outfile) > 0)
  expect_equal(result$output, outfile)
})

test_that("cf_save creates a PDF file", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))

  outfile <- tempfile(fileext = ".pdf")
  on.exit(unlink(outfile), add = TRUE)

  result <- suppressMessages(
    cf_save(obj, output = outfile, final = "Final", dpi = 72)
  )

  expect_true(file.exists(outfile))
  expect_true(file.size(outfile) > 0)
})

test_that("cf_save with shading produces output", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))

  outfile <- tempfile(fileext = ".png")
  on.exit(unlink(outfile), add = TRUE)

  result <- suppressMessages(
    cf_save(obj, output = outfile, shading = TRUE, dpi = 72)
  )

  expect_true(file.exists(outfile))
  expect_true(file.size(outfile) > 0)
})

test_that("cf_save rejects unsupported format", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "x"))

  expect_error(
    cf_save(obj, output = "out.bmp"),
    "Unsupported format"
  )
})

test_that("cf_save rejects object with no steps", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))

  expect_error(cf_save(obj, output = "out.png"), "No exclusion steps")
})

test_that("cf_save rejects non-consortflow object", {
  expect_error(cf_save(list(), output = "x.png"), "consortflow object")
})

test_that("cf_save prints summary message", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))

  outfile <- tempfile(fileext = ".png")
  on.exit(unlink(outfile), add = TRUE)

  expect_message(
    cf_save(obj, output = outfile, dpi = 72),
    "cf_save"
  )
})

test_that("cf_save handles many exclusion steps", {
  d <- make_test_data(n = 5000)
  obj <- suppressMessages(cf_init(d, label = "Large cohort"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))
  obj <- suppressMessages(cf_exclude(obj, age > 80, label = "Over 80"))
  obj <- suppressMessages(cf_exclude(obj, followup < 30, label = "FU < 30d"))
  obj <- suppressMessages(cf_exclude(obj, !has_labs, label = "No labs"))
  obj <- suppressMessages(cf_exclude(obj, is.na(education), label = "Missing education"))

  outfile <- tempfile(fileext = ".png")
  on.exit(unlink(outfile), add = TRUE)

  result <- suppressMessages(
    cf_save(obj, output = outfile, final = "Analytic cohort",
            shading = TRUE, dpi = 72)
  )

  expect_true(file.exists(outfile))
  expect_length(obj$steps, 5L)
})

test_that("cf_draw works without error", {
  d <- make_test_data()
  obj <- suppressMessages(cf_init(d, label = "All"))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "Under 18"))

  # draw to a null PNG device
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 8, height = 6, units = "in", res = 72)
  expect_no_error(cf_draw(obj))
  grDevices::dev.off()
})

test_that("cf_save validates final parameter", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "x"))

  expect_error(
    cf_save(obj, output = tempfile(fileext = ".png"), final = c("a", "b")),
    "single character string"
  )
})

test_that("cf_save validates shading parameter", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "x"))

  expect_error(
    cf_save(obj, output = tempfile(fileext = ".png"), shading = "yes"),
    "TRUE or FALSE"
  )
})

test_that("cf_save validates dpi parameter", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "x"))

  expect_error(
    cf_save(obj, output = tempfile(fileext = ".png"), dpi = -1),
    "positive number"
  )
})

test_that("cf_save rejects output with no extension", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "x"))

  expect_error(cf_save(obj, output = "noext"), "file extension")
})

test_that("cf_save rejects nonexistent output directory", {
  d <- make_test_data(n = 10)
  obj <- suppressMessages(cf_init(d))
  obj <- suppressMessages(cf_exclude(obj, age < 18, label = "x"))

  expect_error(
    cf_save(obj, output = "/nonexistent/path/out.png"),
    "does not exist"
  )
})
