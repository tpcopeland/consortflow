#' Exclude observations from the flowchart
#'
#' Apply an exclusion criterion. Rows matching the condition are dropped
#' from the working data and recorded as an exclusion step in the diagram.
#'
#' @param obj A `consortflow` object.
#' @param condition An unquoted expression evaluated in the context of the
#'   current data (e.g., `age < 18`). Rows where this is `TRUE` are excluded.
#'   Rows where the result is `NA` are kept (not excluded).
#' @param label Character string describing this exclusion
#'   (e.g., `"Age < 18 years"`).
#' @param remaining Optional character label for the remaining-population box
#'   after this step (e.g., `"Adult cohort"`). If `NULL`, no label is shown
#'   on the remaining box.
#'
#' @return The updated `consortflow` object (returned invisibly).
#'
#' @examples
#' d <- data.frame(id = 1:100, age = sample(15:80, 100, replace = TRUE))
#' flow <- cf_init(d, label = "Source population")
#' flow <- cf_exclude(flow, age < 18, label = "Age < 18 years")
#' print(flow)
#'
#' @export
cf_exclude <- function(obj, condition, label, remaining = NULL) {
  if (!inherits(obj, "consortflow")) {
    stop("`obj` must be a consortflow object. Run cf_init() first.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1L) {
    stop("`label` must be a single character string.", call. = FALSE)
  }
  if (!is.null(remaining) && (!is.character(remaining) || length(remaining) != 1L)) {
    stop("`remaining` must be NULL or a single character string.", call. = FALSE)
  }

  cond_expr <- substitute(condition)
  mask <- tryCatch(
    eval(cond_expr, envir = obj$data, enclos = parent.frame()),
    error = function(e) {
      stop(
        "Failed to evaluate exclusion condition for '", label, "' (",
        deparse(cond_expr, width.cutoff = 60L)[1], "): ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (!is.logical(mask)) {
    stop("Exclusion condition must evaluate to a logical vector.", call. = FALSE)
  }
  if (length(mask) != nrow(obj$data)) {
    stop(
      "Exclusion condition length (", length(mask),
      ") does not match number of rows (", nrow(obj$data), ").",
      call. = FALSE
    )
  }

  # NA in condition means "keep" (not excluded)
  mask[is.na(mask)] <- FALSE

  n_excluded <- sum(mask)
  obj$data <- obj$data[!mask, , drop = FALSE]
  n_remaining <- nrow(obj$data)

  step <- list(
    label = label,
    n_excluded = n_excluded,
    n_remaining = n_remaining,
    remaining_label = remaining
  )
  obj$steps <- c(obj$steps, list(step))

  message(
    "cf_exclude: ", label, " (",
    format(n_excluded, big.mark = ","), " excluded, ",
    format(n_remaining, big.mark = ","), " remaining)"
  )
  invisible(obj)
}
