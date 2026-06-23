# CONSORT plotting helpers for CohortPipeline.
#
# Renders cohort flows as CONSORT diagrams using grid graphics directly.
# The public entry point is the class method
# CohortPipeline$draw_consort_panels(); these functions implement it.
#
# Layout works in millimetres throughout: positions and box sizes are
# computed once in mm, then drawn into a viewport whose native scale is
# also mm. The panel grob therefore has an intrinsic size that the
# enclosing layout can stretch or letterbox.

#' @keywords internal
.draw_consort_panels_impl <- function(panels, nodes, file = NULL,
                                      ncol = NULL, width = NULL, height = NULL,
                                      text_width = 40, title_fontsize = 14) {
  if (!is.list(panels) || length(panels) == 0L) {
    stop("draw_consort_panels: 'panels' must be a non-empty list.",
      call. = FALSE)
  }

  layouts <- lapply(seq_along(panels), function(i) {
    spec  <- panels[[i]]
    title <- names(panels)[i] %||% sprintf("Panel %d", i)
    if (is.list(spec) && !is.null(spec$flow)) {
      flow <- spec$flow
      sb   <- spec$side_branches %||% list()
    } else {
      flow <- spec
      sb   <- list()
    }
    rows <- .panel_rows(flow, sb, nodes, text_width = text_width)
    list(title = title, layout = .layout_panel(rows))
  })

  ncol_use <- ncol %||% length(panels)
  nrow_use <- ceiling(length(panels) / ncol_use)

  # Device size: fit the largest panel, then arrange in a grid.
  max_w_mm <- max(vapply(layouts, function(p) p$layout$total_w, 0))
  max_h_mm <- max(vapply(layouts, function(p) p$layout$total_h, 0))
  width_use  <- width  %||% max(6, ncol_use * (max_w_mm / 25.4) + 0.5)
  height_use <- height %||% max(6, nrow_use * (max_h_mm / 25.4) + 0.7)

  # Use the tallest panel's height as the common y-scale so each
  # panel's content lands at the same y when the grobs are placed in
  # equally sized cells of grid.arrange.
  panel_grobs <- lapply(layouts, function(p) {
    .panel_grob(p$layout, p$title,
      title_fontsize = title_fontsize, common_h = max_h_mm)
  })

  if (!is.null(file)) {
    ext <- tools::file_ext(file)
    if (identical(ext, "pdf")) {
      grDevices::pdf(file, width = width_use, height = height_use)
    } else if (identical(ext, "png")) {
      grDevices::png(file, width = width_use, height = height_use,
        units = "in", res = 150)
    } else {
      stop("draw_consort_panels: unsupported file extension: .", ext,
        call. = FALSE)
    }
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  gridExtra::grid.arrange(grobs = panel_grobs, ncol = ncol_use)
  invisible(panel_grobs)
}

# Number of own (non-inherited) log entries on a node.
#' @keywords internal
.own_log_n <- function(node) {
  length(node$log_entries) - (node$branched_at_log_len %||% 0L)
}

# Return the node's own (non-inherited) log entries as a data.table.
#' @keywords internal
.own_log <- function(node) {
  start <- (node$branched_at_log_len %||% 0L) + 1L
  end   <- length(node$log_entries)
  if (end < start) {
    return(data.table::data.table(
      step        = integer(),
      reason      = character(),
      n_excluded  = integer(),
      n_remaining = integer()
    ))
  }
  data.table::rbindlist(lapply(node$log_entries[start:end], function(e) {
    data.table::data.table(
      step        = e$step,
      reason      = e$reason,
      n_excluded  = e$n_excluded,
      n_remaining = e$n_remaining
    )
  }))
}

# Walk a flow specification and emit an ordered list of plain box
# records ready for layout. Each row is list(type, text); spine rows
# are the vertical column, side rows are the exclusion bullet boxes
# placed to the right of the gap above the next spine box.
#' @keywords internal
.panel_rows <- function(flow, side_branches, nodes, text_width = 40) {
  fmt <- function(n) format(n, big.mark = ",")
  split_label <- function(label, n_val) {
    n_line <- sprintf("(n = %s)", fmt(n_val))
    idx <- regexpr(" \\(", label)
    if (idx[[1L]] > 0L) {
      name <- substr(label, 1L, idx[[1L]] - 1L)
      desc <- substr(label, idx[[1L]] + 1L, nchar(label))
      paste(c(name, desc, n_line), collapse = "\n")
    } else {
      paste(c(label, n_line), collapse = "\n")
    }
  }

  identity_merges <- list()
  for (i in seq_along(side_branches)) {
    sb_label <- side_branches[[i]]
    sb <- nodes[[sb_label]]
    if (is.null(sb)) next
    if (.own_log_n(sb) > 0L) {
      stop("draw_consort_panels supports identity side branches only; '",
        sb_label, "' has its own exclusions.", call. = FALSE)
    }
    attach_n <- as.character(sb$branched_at_n %||% length(sb$status))
    identity_merges[[attach_n]] <- list(
      name = names(side_branches)[i],
      n    = sum(sb$status == 0L)
    )
  }
  attach_keys <- names(identity_merges)

  rows <- list()
  add_spine <- function(text) {
    rows[[length(rows) + 1L]] <<- list(type = "spine", text = text)
  }
  add_side <- function(text) {
    rows[[length(rows) + 1L]] <<- list(type = "side",  text = text)
  }

  for (fi in seq_along(flow)) {
    br_name    <- flow[fi]
    br_label   <- names(flow)[fi]
    node       <- nodes[[br_name]]
    if (is.null(node)) {
      stop("draw_consort_panels: unknown cohort '", br_name,
        "' in flow.", call. = FALSE)
    }
    log_       <- .own_log(node)
    br_n_final <- sum(node$status == 0L)

    if (fi == 1L) {
      n_total <- if (is.na(node$parent)) length(node$status) else node$branched_at_n
      tot_merge <- identity_merges[[as.character(n_total)]]
      lbl <- split_label("Cohort participants", n_total)
      if (!is.null(tot_merge)) lbl <- paste0(lbl, "\n", tot_merge$name)
      add_spine(lbl)
    }

    if (nrow(log_) > 0L) {
      chunk_boundaries <- c(
        which(as.character(log_$n_remaining) %in% attach_keys),
        nrow(log_)
      )
      chunk_boundaries <- sort(unique(chunk_boundaries))
      chunk_start <- 1L
      for (ck in seq_along(chunk_boundaries)) {
        chunk_end   <- chunk_boundaries[ck]
        chunk       <- log_[chunk_start:chunk_end]
        chunk_start <- chunk_end + 1L
        is_last     <- ck == length(chunk_boundaries)
        final_n     <- chunk$n_remaining[nrow(chunk)]
        merge_      <- identity_merges[[as.character(final_n)]]
        reasons <- vapply(seq_len(nrow(chunk)), function(j) {
          sprintf("- %s (n = %s)", chunk$reason[j], fmt(chunk$n_excluded[j]))
        }, character(1L))
        excl_lbl <- sprintf("Excluded (n = %s):\n%s",
          fmt(sum(chunk$n_excluded)),
          paste(reasons, collapse = "\n"))
        add_side(excl_lbl)
        if (is_last) {
          main_lbl <- split_label(br_label, br_n_final)
        } else if (!is.null(merge_)) {
          main_lbl <- split_label(merge_$name, final_n)
        } else {
          main_lbl <- sprintf("n = %s", fmt(final_n))
        }
        add_spine(main_lbl)
      }
    } else if (fi > 1L) {
      add_spine(split_label(br_label, br_n_final))
    }
  }

  rows
}

# Wrap text at the given character width, preserving \n-separated
# paragraphs. Returns a single string with line breaks.
#' @keywords internal
.wrap_text <- function(text, width) {
  if (length(text) == 0L || !nzchar(text)) return("")
  paragraphs <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  wrapped <- unlist(lapply(paragraphs, function(p) {
    if (!nzchar(p)) return("")
    strwrap(p, width = width)
  }))
  paste(wrapped, collapse = "\n")
}

# Lay out a list of row records into mm coordinates. Returns a list
# with `rows` (each augmented with cx, cy, w, h, lines), `arrows`
# (list of segments), `total_w`, `total_h`, `spine_cx`, `side_cx`.
# Coordinates use a top-down y axis (y = 0 at the top).
#' @keywords internal
.layout_panel <- function(rows,
                          pad_h = 3.0, pad_v = 2.0,
                          line_h = 4.5, char_w = 2.4,
                          min_v_gap = 8.0, h_gap = 8.0,
                          v_pad_in_gap = 2.0,
                          title_pad = 8.0, margin = 4.0,
                          text_width = 40) {
  rows <- lapply(rows, function(r) {
    wrapped <- .wrap_text(r$text, text_width)
    lines   <- strsplit(wrapped, "\n", fixed = TRUE)[[1L]]
    if (length(lines) == 0L) lines <- ""
    r$lines   <- lines
    r$n_lines <- length(lines)
    r$max_chars <- max(nchar(lines))
    r$h <- r$n_lines * line_h + 2 * pad_v
    r$w <- r$max_chars * char_w + 2 * pad_h
    r
  })

  events <- list()
  i <- 1L
  while (i <= length(rows)) {
    if (rows[[i]]$type == "spine") {
      ev <- list(spine = rows[[i]])
      if (i + 1L <= length(rows) && rows[[i + 1L]]$type == "side") {
        ev$side <- rows[[i + 1L]]
        i <- i + 2L
      } else {
        i <- i + 1L
      }
      events[[length(events) + 1L]] <- ev
    } else {
      i <- i + 1L
    }
  }

  spine_w <- max(vapply(events, function(e) e$spine$w, 0))
  side_ws <- vapply(events, function(e) {
    if (is.null(e$side)) 0 else e$side$w
  }, 0)
  side_w  <- max(side_ws, 0)
  has_side <- side_w > 0

  spine_cx <- margin + spine_w / 2
  side_cx  <- if (has_side) margin + spine_w + h_gap + side_w / 2 else NA_real_
  panel_w  <- spine_w + (if (has_side) h_gap + side_w else 0) + 2 * margin

  y <- title_pad + margin
  laid_rows <- list()
  arrows    <- list()
  prev_spine <- NULL

  for (k in seq_along(events)) {
    sp <- events[[k]]$spine
    sp$cx <- spine_cx
    sp$cy <- y + sp$h / 2
    sp$y_top <- y
    sp$y_bot <- y + sp$h
    laid_rows[[length(laid_rows) + 1L]] <- sp

    if (!is.null(prev_spine)) {
      arrows[[length(arrows) + 1L]] <- list(
        x0 = spine_cx, y0 = prev_spine$y_bot,
        x1 = spine_cx, y1 = sp$y_top,
        kind = "spine"
      )
    }

    if (!is.null(events[[k]]$side)) {
      sd <- events[[k]]$side
      gap <- max(min_v_gap, sd$h + 2 * v_pad_in_gap)
      mid_y <- sp$y_bot + gap / 2
      sd$cx <- side_cx
      sd$cy <- mid_y
      sd$y_top <- mid_y - sd$h / 2
      sd$y_bot <- mid_y + sd$h / 2
      laid_rows[[length(laid_rows) + 1L]] <- sd

      arrows[[length(arrows) + 1L]] <- list(
        x0 = spine_cx, y0 = mid_y,
        x1 = side_cx - sd$w / 2, y1 = mid_y,
        kind = "side"
      )

      y <- sp$y_bot + gap
    } else if (k < length(events)) {
      y <- sp$y_bot + min_v_gap
    } else {
      y <- sp$y_bot
    }
    prev_spine <- sp
  }

  total_h <- y + margin
  list(
    rows     = laid_rows,
    arrows   = arrows,
    total_w  = panel_w,
    total_h  = total_h,
    spine_cx = spine_cx,
    side_cx  = side_cx,
    title_y  = margin + title_pad / 2
  )
}

# Build a gTree for one panel from its layout. Coordinates are in mm
# via a viewport with an explicit native scale. `common_h` (the max
# total_h across all panels) keeps the y-axis consistent so panels
# top-align in a multi-panel layout.
#' @keywords internal
.panel_grob <- function(layout, title,
                        title_fontsize = 14, box_fontsize = 10,
                        common_h = NULL) {
  pad_h <- 3.0  # must match .layout_panel's horizontal box padding
  total_h <- common_h %||% layout$total_h

  vp <- grid::viewport(
    xscale = c(0, layout$total_w),
    yscale = c(total_h, 0)
  )

  children <- list()
  add <- function(g) {
    children[[length(children) + 1L]] <<- g
  }

  add(grid::textGrob(
    title,
    x = grid::unit(layout$total_w / 2, "native"),
    y = grid::unit(layout$title_y, "native"),
    just = c("center", "center"),
    gp = grid::gpar(fontsize = title_fontsize, fontface = "bold")
  ))

  for (r in layout$rows) {
    add(grid::rectGrob(
      x = grid::unit(r$cx, "native"),
      y = grid::unit(r$cy, "native"),
      width  = grid::unit(r$w, "native"),
      height = grid::unit(r$h, "native"),
      just = "center",
      gp = grid::gpar(fill = "white", col = "black", lwd = 1)
    ))
    if (identical(r$type, "side")) {
      # Side boxes carry bullet lists. Render flush-left so each
      # bullet's "-" lines up.
      add(grid::textGrob(
        paste(r$lines, collapse = "\n"),
        x = grid::unit(r$cx - r$w / 2 + pad_h, "native"),
        y = grid::unit(r$cy, "native"),
        just = c("left", "center"),
        gp = grid::gpar(fontsize = box_fontsize, lineheight = 0.95)
      ))
    } else {
      add(grid::textGrob(
        paste(r$lines, collapse = "\n"),
        x = grid::unit(r$cx, "native"),
        y = grid::unit(r$cy, "native"),
        just = c("center", "center"),
        gp = grid::gpar(fontsize = box_fontsize, lineheight = 0.95)
      ))
    }
  }

  for (a in layout$arrows) {
    add(grid::linesGrob(
      x = grid::unit(c(a$x0, a$x1), "native"),
      y = grid::unit(c(a$y0, a$y1), "native"),
      arrow = grid::arrow(angle = 30,
        length = grid::unit(2.2, "mm"), type = "closed"),
      gp = grid::gpar(col = "black", fill = "black", lwd = 1)
    ))
  }

  grid::gTree(
    children = do.call(grid::gList, children),
    vp = vp
  )
}
