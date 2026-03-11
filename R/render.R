#' Open a device, render, and close safely
#'
#' Opens a graphics device, renders the diagram, and ensures the device
#' is always closed — even if rendering fails.
#'
#' @param open_fn A function that opens the graphics device.
#' @param obj A `consortflow` object.
#' @param ... Arguments passed to [.render_diagram()].
#'
#' @return The return value of [.render_diagram()].
#' @keywords internal
.safe_render <- function(open_fn, obj, ...) {
  open_fn()
  tryCatch(
    {
      result <- .render_diagram(obj, ...)
      grDevices::dev.off()
      result
    },
    error = function(e) {
      try(grDevices::dev.off(), silent = TRUE)
      stop("Rendering failed: ", conditionMessage(e), call. = FALSE)
    }
  )
}


#' Render a consortflow diagram using grid graphics
#'
#' Internal function that draws the exclusion flowchart onto the current
#' graphics device using the grid package.
#'
#' @param obj A `consortflow` object with at least one exclusion step.
#' @param final Character label for the final cohort box.
#' @param shading Logical; use colored boxes?
#' @param font_family Character font family name.
#'
#' @return Invisibly returns a list with `width` and `height` in inches.
#' @keywords internal
.render_diagram <- function(obj, final = "Final Cohort", shading = FALSE,
                            font_family = "sans") {

  steps <- obj$steps
  n_steps <- length(steps)

  # --- colour palette ---
  edge_col    <- "#2c3e50"
  main_fill   <- if (shading) "#d4e6f1" else "white"
  excl_fill   <- if (shading) "#fadbd8" else "white"
  arrow_col   <- edge_col

  # --- layout constants (inches) ---
  main_fs    <- 11    # font size for main/initial/final boxes
  excl_fs    <- 10    # font size for exclusion boxes
  count_fs   <- 10    # font size for count-only remaining boxes
  pad_x      <- 0.12  # horizontal text padding

  pad_y      <- 0.08  # vertical text padding
  v_gap      <- 0.12  # vertical gap between elements
  h_gap      <- 0.25  # horizontal gap main column -> exclusion column
  rounding   <- unit(0.06, "inches")

  # --- helper: measure text size in inches ---
  measure_text <- function(txt, fontsize) {
    lines <- strsplit(txt, "\n")[[1]]
    n_lines <- length(lines)
    lh <- fontsize / 72
    h <- lh * n_lines * 1.35
    max_chars <- max(nchar(lines))
    w <- max_chars * fontsize / 72 * 0.52
    list(w = w, h = h)
  }

  # --- helper: wrap text ---
  wrap_text <- function(txt, max_chars) {
    raw_lines <- strsplit(txt, "\n")[[1]]
    out <- character(0)
    for (line in raw_lines) {
      if (nchar(line) <= max_chars) {
        out <- c(out, line)
      } else {
        words <- strsplit(line, " ")[[1]]
        current <- words[1]
        # force-break single words longer than max_chars
        if (nchar(current) > max_chars) {
          while (nchar(current) > max_chars) {
            out <- c(out, substr(current, 1, max_chars))
            current <- substr(current, max_chars + 1L, nchar(current))
          }
        }
        for (w in words[-1]) {
          if (nchar(paste(current, w)) <= max_chars) {
            current <- paste(current, w)
          } else {
            out <- c(out, current)
            current <- w
            # force-break long words
            if (nchar(current) > max_chars) {
              while (nchar(current) > max_chars) {
                out <- c(out, substr(current, 1, max_chars))
                current <- substr(current, max_chars + 1L, nchar(current))
              }
            }
          }
        }
        out <- c(out, current)
      }
    }
    paste(out, collapse = "\n")
  }

  fmt_n <- function(n) format(n, big.mark = ",")

  # --- precompute all box text and sizes ---
  init_text <- wrap_text(
    paste0(obj$initial_label, "\n(n=", fmt_n(obj$initial_n), ")"),
    max_chars = 45
  )
  init_sz <- measure_text(init_text, main_fs)
  init_w <- init_sz$w + pad_x * 2
  init_h <- init_sz$h + pad_y * 2

  excl_texts <- character(n_steps)
  excl_ws    <- numeric(n_steps)
  excl_hs    <- numeric(n_steps)

  main_texts <- character(n_steps)
  main_ws    <- numeric(n_steps)
  main_hs    <- numeric(n_steps)
  main_fontsizes <- numeric(n_steps)

  for (i in seq_len(n_steps)) {
    s <- steps[[i]]

    # exclusion box
    etxt <- wrap_text(
      paste0(s$label, "\n(n=", fmt_n(s$n_excluded), ")"),
      max_chars = 35
    )
    excl_texts[i] <- etxt
    esz <- measure_text(etxt, excl_fs)
    excl_ws[i] <- esz$w + pad_x * 2
    excl_hs[i] <- esz$h + pad_y * 2

    # remaining / final box
    if (i == n_steps) {
      rlbl <- paste0(final, "\n(n=", fmt_n(s$n_remaining), ")")
      fs_i <- main_fs
    } else if (!is.null(s$remaining_label)) {
      rlbl <- paste0(s$remaining_label, "\n(n=", fmt_n(s$n_remaining), ")")
      fs_i <- main_fs
    } else {
      rlbl <- paste0("(n=", fmt_n(s$n_remaining), ")")
      fs_i <- count_fs
    }
    mtxt <- wrap_text(rlbl, max_chars = 45)
    main_texts[i] <- mtxt
    main_fontsizes[i] <- fs_i
    msz <- measure_text(mtxt, fs_i)
    main_ws[i] <- msz$w + pad_x * 2
    main_hs[i] <- msz$h + pad_y * 2
  }

  # --- column widths ---
  main_col_w <- max(init_w, main_ws)
  init_w <- main_col_w  # stretch init to match
  main_ws[] <- pmax(main_ws, main_col_w * 0.5)  # min 50% of column

  excl_col_w <- max(excl_ws)
  total_w <- main_col_w + h_gap + excl_col_w

  main_cx <- main_col_w / 2
  excl_cx <- main_col_w + h_gap + excl_col_w / 2

  # --- vertical layout ---
  # Pattern per step (top to bottom):
  #   prev_box_bottom -> v_gap -> excl_box (horizontal branch) -> v_gap -> remaining_box
  # The vertical line runs through main_cx. At excl_cy, a horizontal arrow
  # branches right to the exclusion box. The remaining box sits below.

  y_pos <- 0
  init_cy <- y_pos + init_h / 2
  y_pos <- y_pos + init_h

  # store positions for each step
  step_excl_cy <- numeric(n_steps)
  step_main_cy <- numeric(n_steps)

  for (i in seq_len(n_steps)) {
    eh <- excl_hs[i]
    mh <- main_hs[i]

    # gap after previous box
    y_pos <- y_pos + v_gap

    # exclusion box center
    step_excl_cy[i] <- y_pos + eh / 2
    y_pos <- y_pos + eh

    # gap between exclusion and remaining
    y_pos <- y_pos + v_gap

    # remaining box center
    step_main_cy[i] <- y_pos + mh / 2
    y_pos <- y_pos + mh
  }

  total_h <- y_pos
  margin <- 0.3
  fig_w <- total_w + margin * 2
  fig_h <- total_h + margin * 2

  # --- draw ---
  grid.newpage()
  pushViewport(viewport(
    width = unit(fig_w, "inches"),
    height = unit(fig_h, "inches"),
    xscale = c(-margin, total_w + margin),
    yscale = c(total_h + margin, -margin)  # y increases downward
  ))

  draw_box <- function(cx, cy, w, h, txt, fill, fontsize) {
    grid.roundrect(
      x = unit(cx, "native"), y = unit(cy, "native"),
      width = unit(w, "native"), height = unit(h, "native"),
      r = rounding,
      gp = gpar(fill = fill, col = edge_col, lwd = 1.5)
    )
    grid.text(
      txt,
      x = unit(cx, "native"), y = unit(cy, "native"),
      gp = gpar(fontsize = fontsize, col = edge_col, fontfamily = font_family)
    )
  }

  draw_arrow <- function(x0, y0, x1, y1) {
    grid.lines(
      x = unit(c(x0, x1), "native"), y = unit(c(y0, y1), "native"),
      arrow = arrow(type = "closed", length = unit(0.08, "inches")),
      gp = gpar(col = arrow_col, lwd = 1.5, fill = arrow_col)
    )
  }

  draw_line <- function(x0, y0, x1, y1) {
    grid.lines(
      x = unit(c(x0, x1), "native"), y = unit(c(y0, y1), "native"),
      gp = gpar(col = arrow_col, lwd = 1.5)
    )
  }

  # --- initial box ---
  draw_box(main_cx, init_cy, init_w, init_h, init_text, main_fill, main_fs)
  prev_bottom <- init_cy + init_h / 2

  # --- each step ---
  for (i in seq_len(n_steps)) {
    ecy <- step_excl_cy[i]
    mcy <- step_main_cy[i]
    eh  <- excl_hs[i]
    mh  <- main_hs[i]

    # vertical line from previous box bottom down to exclusion box center y
    draw_line(main_cx, prev_bottom, main_cx, ecy)

    # horizontal arrow from main column to exclusion box left edge
    excl_left <- excl_cx - excl_ws[i] / 2
    draw_arrow(main_cx, ecy, excl_left, ecy)

    # draw exclusion box
    draw_box(excl_cx, ecy, excl_ws[i], eh, excl_texts[i], excl_fill, excl_fs)

    # vertical arrow from exclusion-y down to remaining box top
    main_top <- mcy - mh / 2
    draw_arrow(main_cx, ecy, main_cx, main_top)

    # draw remaining / final box
    draw_box(main_cx, mcy, main_ws[i], mh, main_texts[i], main_fill,
             main_fontsizes[i])

    prev_bottom <- mcy + mh / 2
  }

  popViewport()
  invisible(list(width = fig_w, height = fig_h))
}
