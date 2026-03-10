#' Create a new consortflow object
#'
#' Internal constructor for the `consortflow` S3 class.
#'
#' @param data A data frame (the working copy, rows get dropped).
#' @param initial_label Character label for the starting population.
#' @param initial_n Integer count of starting rows.
#' @param steps A list of exclusion step records.
#'
#' @return A `consortflow` object.
#' @keywords internal
new_consortflow <- function(data, initial_label, initial_n, steps = list()) {
  obj <- list(
    data = data,
    initial_label = initial_label,
    initial_n = initial_n,
    steps = steps
  )
  class(obj) <- "consortflow"
  obj
}


#' Print a consortflow object
#'
#' @param x A `consortflow` object.
#' @param ... Ignored.
#'
#' @export
print.consortflow <- function(x, ...) {
  n_steps <- length(x$steps)
  n_remaining <- if (n_steps > 0) {
    x$steps[[n_steps]]$n_remaining
  } else {
    x$initial_n
  }
  cat("consortflow object\n")
  cat("  Initial:   ", format(x$initial_n, big.mark = ","), " (", x$initial_label, ")\n", sep = "")
  cat("  Steps:     ", n_steps, "\n", sep = "")
  cat("  Remaining: ", format(n_remaining, big.mark = ","), "\n", sep = "")
  invisible(x)
}


#' Summarize a consortflow object
#'
#' Returns a data frame with one row per exclusion step showing the label,
#' number excluded, and number remaining.
#'
#' @param object A `consortflow` object.
#' @param ... Ignored.
#'
#' @return A `data.frame` with columns `step`, `label`, `n_excluded`,
#'   `n_remaining`, and `remaining_label`.
#'
#' @examples
#' d <- data.frame(id = 1:100, age = sample(15:80, 100, replace = TRUE))
#' flow <- cf_init(d, label = "Source population")
#' flow <- cf_exclude(flow, age < 18, label = "Age < 18 years")
#' summary(flow)
#'
#' @export
summary.consortflow <- function(object, ...) {
  steps <- object$steps
  n <- length(steps)
  if (n == 0L) {
    return(data.frame(
      step = integer(0), label = character(0),
      n_excluded = integer(0), n_remaining = integer(0),
      remaining_label = character(0), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    step = seq_len(n),
    label = vapply(steps, `[[`, character(1), "label"),
    n_excluded = vapply(steps, `[[`, integer(1), "n_excluded"),
    n_remaining = vapply(steps, `[[`, integer(1), "n_remaining"),
    remaining_label = vapply(steps, function(s) {
      if (is.null(s$remaining_label)) NA_character_ else s$remaining_label
    }, character(1)),
    stringsAsFactors = FALSE
  )
}


#' Plot a consortflow object
#'
#' Draws the exclusion flowchart to the current graphics device.
#' Wrapper around [cf_draw()].
#'
#' @param x A `consortflow` object.
#' @param ... Additional arguments passed to [cf_draw()].
#'
#' @export
plot.consortflow <- function(x, ...) {
  cf_draw(x, ...)
}
