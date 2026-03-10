#' Create a test dataset for consortflow tests
#'
#' @param n Number of rows (default 1000).
#' @param seed Random seed (default 42).
#' @return A data.frame with columns: id, age, sex, followup, has_labs,
#'   education.
make_test_data <- function(n = 1000L, seed = 42L) {
  set.seed(seed)
  data.frame(
    id        = seq_len(n),
    age       = sample(12:90, n, replace = TRUE),
    sex       = sample(c("M", "F"), n, replace = TRUE),
    followup  = sample(0:730, n, replace = TRUE),
    has_labs  = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.85, 0.15)),
    education = sample(c("primary", "secondary", "tertiary", NA),
                       n, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1))
  )
}
