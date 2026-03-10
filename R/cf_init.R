#' Initialize an exclusion flowchart
#'
#' Start a new exclusion flowchart from a data frame. This captures the
#' initial population count and label, and stores a working copy of the data
#' for subsequent [cf_exclude()] calls.
#'
#' @param data A data frame.
#' @param label Character string describing the initial population
#'   (e.g., `"All antidepressant dispensings"`).
#'
#' @return A `consortflow` object (returned invisibly).
#'
#' @examples
#' d <- data.frame(id = 1:100, age = sample(15:80, 100, replace = TRUE))
#' flow <- cf_init(d, label = "Source population")
#' print(flow)
#'
#' @export
cf_init <- function(data, label = "Initial population") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.character(label) || length(label) != 1L) {
    stop("`label` must be a single character string.", call. = FALSE)
  }
  # coerce to plain data.frame (data.table's [ doesn't support drop=FALSE)
  if (!identical(class(data), "data.frame")) {
    data <- as.data.frame(data)
  }
  n <- nrow(data)
  if (n == 0L) {
    stop("`data` has 0 rows.", call. = FALSE)
  }
  message("cf_init: ", format(n, big.mark = ","), " observations (", label, ")")
  invisible(new_consortflow(data, initial_label = label, initial_n = n))
}
