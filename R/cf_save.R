#' Save the exclusion flowchart to a file
#'
#' Render the exclusion flowchart and save it as PNG, PDF, or SVG.
#'
#' @param obj A `consortflow` object with at least one exclusion step.
#' @param output Character path for the output file. The file extension
#'   determines the format: `.png`, `.pdf`, or `.svg`.
#' @param final Character label for the final cohort box
#'   (default: `"Final Cohort"`).
#' @param shading Logical; if `TRUE`, main-flow boxes are light blue and
#'   exclusion boxes are light red. Default is `FALSE` (white boxes).
#' @param dpi Integer resolution for PNG output (default: 150; use 300 for
#'   publication quality). Ignored for PDF/SVG.
#' @param font_family Character font family (default: `"sans"`).
#'
#' @return The `consortflow` object (returned invisibly), with `$output`
#'   set to the file path written.
#'
#' @examples
#' d <- data.frame(
#'   id = 1:1000,
#'   age = sample(15:80, 1000, replace = TRUE),
#'   fu = sample(1:365, 1000, replace = TRUE)
#' )
#' flow <- cf_init(d, label = "Source population")
#' flow <- cf_exclude(flow, age < 18, label = "Age < 18 years")
#' flow <- cf_exclude(flow, fu < 30, label = "Follow-up < 30 days")
#' \donttest{
#' cf_save(flow, output = tempfile(fileext = ".png"),
#'         final = "Study population", dpi = 150)
#' }
#'
#' @export
cf_save <- function(obj, output, final = "Final Cohort", shading = FALSE,
                    dpi = 150L, font_family = "sans") {

  if (!inherits(obj, "consortflow")) {
    stop("`obj` must be a consortflow object.", call. = FALSE)
  }
  if (length(obj$steps) == 0L) {
    stop("No exclusion steps recorded. Run cf_exclude() first.", call. = FALSE)
  }
  if (!is.character(output) || length(output) != 1L) {
    stop("`output` must be a single file path.", call. = FALSE)
  }
  if (!is.character(final) || length(final) != 1L) {
    stop("`final` must be a single character string.", call. = FALSE)
  }
  if (!is.logical(shading) || length(shading) != 1L) {
    stop("`shading` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(dpi) || length(dpi) != 1L || dpi < 1) {
    stop("`dpi` must be a positive number.", call. = FALSE)
  }
  if (!is.character(font_family) || length(font_family) != 1L) {
    stop("`font_family` must be a single character string.", call. = FALSE)
  }

  if (!grepl("\\.", basename(output))) {
    stop("`output` must have a file extension (.png, .pdf, or .svg).",
         call. = FALSE)
  }
  ext <- tolower(sub(".*\\.", "", basename(output)))

  outdir <- dirname(output)
  if (!dir.exists(outdir)) {
    stop("Output directory does not exist: ", outdir, call. = FALSE)
  }

  # pre-render to compute figure dimensions
  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)
  dims <- .safe_render(
    function() grDevices::png(tmp_png, width = 10, height = 10,
                              units = "in", res = 72),
    obj, final = final, shading = shading, font_family = font_family
  )

  fig_w <- dims$width
  fig_h <- dims$height

  # render to actual output
  if (ext == "png") {
    open_fn <- function() grDevices::png(output, width = fig_w,
                                         height = fig_h, units = "in",
                                         res = dpi)
  } else if (ext == "pdf") {
    open_fn <- function() grDevices::pdf(output, width = fig_w,
                                         height = fig_h)
  } else if (ext == "svg") {
    open_fn <- function() grDevices::svg(output, width = fig_w,
                                         height = fig_h)
  } else {
    stop("Unsupported format '", ext, "'. Use .png, .pdf, or .svg.",
         call. = FALSE)
  }

  .safe_render(open_fn, obj, final = final, shading = shading,
               font_family = font_family)

  n_final <- obj$steps[[length(obj$steps)]]$n_remaining
  message(
    "cf_save: ", output, " (",
    format(obj$initial_n, big.mark = ","), " -> ",
    format(n_final, big.mark = ","), ", ",
    length(obj$steps), " steps)"
  )

  obj$output <- output
  invisible(obj)
}


#' Draw the exclusion flowchart to the current device
#'
#' Render the exclusion flowchart directly to the active graphics device
#' (e.g., an RStudio plot pane or an open file device).
#'
#' @param obj A `consortflow` object with at least one exclusion step.
#' @param final Character label for the final cohort box.
#' @param shading Logical; use colored boxes?
#' @param font_family Character font family.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
cf_draw <- function(obj, final = "Final Cohort", shading = FALSE,
                    font_family = "sans") {
  if (!inherits(obj, "consortflow")) {
    stop("`obj` must be a consortflow object.", call. = FALSE)
  }
  if (length(obj$steps) == 0L) {
    stop("No exclusion steps recorded. Run cf_exclude() first.", call. = FALSE)
  }
  if (!is.character(final) || length(final) != 1L) {
    stop("`final` must be a single character string.", call. = FALSE)
  }
  if (!is.logical(shading) || length(shading) != 1L) {
    stop("`shading` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(font_family) || length(font_family) != 1L) {
    stop("`font_family` must be a single character string.", call. = FALSE)
  }
  .render_diagram(obj, final = final, shading = shading,
                  font_family = font_family)
  invisible(NULL)
}
