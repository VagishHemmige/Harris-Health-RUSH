# Purpose: build the site's teaching figures as SVG files.
# Run from the project root: Rscript quarto/teaching_figures/build_teaching_figures.R
# Reads only quarto/published/tables_2_to_4.csv (values typed in from the
# article) and invented example numbers written below. It reads no study data
# and fits nothing. The SVG files it writes are committed; the site shows
# them and never runs this script.

out_dir <- "quarto/teaching_figures"

# ---- Shared drawing helpers -------------------------------------------------

palette <- list(
  ink = "#1f1f1e",
  ink_2 = "#52514e",
  ink_3 = "#7a7974",
  rule = "#d9d8d3",
  panel = "#f4f3ef",
  before = "#9fbfe8",
  after = "#2a78d6",
  better = "#2a78d6",
  worse = "#eb6834"
)

font <- "system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif"

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

svg_open <- function(width, height, id, title, desc) {
  sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d" ',
      'role="img" aria-labelledby="%s-title %s-desc" font-family="%s">\n',
      '<title id="%s-title">%s</title>\n<desc id="%s-desc">%s</desc>\n',
      '<rect width="100%%" height="100%%" fill="#ffffff"/>\n'
    ),
    width, height, width, height, id, id, font, id, esc(title), id, esc(desc)
  )
}

txt <- function(x, y, label, size = 13, fill = palette$ink, anchor = "start",
                weight = "normal", extra = "") {
  sprintf(
    '<text x="%.1f" y="%.1f" font-size="%g" fill="%s" text-anchor="%s" font-weight="%s"%s>%s</text>\n',
    x, y, size, fill, anchor, weight, extra, esc(label)
  )
}

rect <- function(x, y, w, h, fill, stroke = "none", rx = 4, extra = "") {
  sprintf(
    '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%g" fill="%s" stroke="%s"%s/>\n',
    x, y, w, h, rx, fill, stroke, extra
  )
}

line <- function(x1, y1, x2, y2, stroke = palette$rule, width = 1, extra = "") {
  sprintf(
    '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="%g"%s/>\n',
    x1, y1, x2, y2, stroke, width, extra
  )
}

circle <- function(cx, cy, r, fill, stroke = "#ffffff", width = 2, tip = NULL) {
  tip_el <- if (is.null(tip)) "" else sprintf("<title>%s</title>", esc(tip))
  sprintf(
    '<circle cx="%.1f" cy="%.1f" r="%g" fill="%s" stroke="%s" stroke-width="%g">%s</circle>\n',
    cx, cy, r, fill, stroke, width, tip_el
  )
}

write_svg <- function(parts, file) {
  writeLines(c(parts, "</svg>"), file.path(out_dir, file), sep = "", useBytes = TRUE)
  message("wrote ", file.path(out_dir, file))
}

# ---- F1. Discordant pairs (invented example) --------------------------------
# The example on the Methodological background page: of 100 people, 30 had
# the outcome both times (a), 5 lost it (b), 20 gained it (c), 45 never (d).

build_discordant_pairs <- function() {
  a <- 30; b <- 5; c <- 20; d <- 45
  w <- 680; h <- 360
  s <- svg_open(
    w, h, "f1",
    "Paired outcomes: only the people whose outcome changed carry information",
    paste(
      "Invented example of 100 people measured before and after.",
      "A two-by-two table: 30 had the outcome both times, 45 never had it,",
      "5 lost it and 20 gained it. The 25 people whose outcome changed are highlighted.",
      "The odds ratio is 20 divided by 5, 4.0. Of the 65 without the outcome before,",
      "20 gained it (31%); of the 35 with it, 5 lost it (14%)."
    )
  )
  x0 <- 104; y0 <- 70; cw <- 128; ch <- 80
  s <- c(s,
    txt(16, 26, "Invented example: 100 people, before and after", 15, weight = "600"),
    txt(x0 + cw / 2, y0 - 14, "After: yes", 13, palette$ink_2, "middle"),
    txt(x0 + cw * 1.5, y0 - 14, "After: no", 13, palette$ink_2, "middle"),
    txt(x0 - 12, y0 + ch / 2 + 5, "Before: yes", 13, palette$ink_2, "end"),
    txt(x0 - 12, y0 + ch * 1.5 + 5, "Before: no", 13, palette$ink_2, "end")
  )
  cells <- list(
    list(col = 0, row = 0, n = a, key = "a", what = "had it both times", fill = palette$panel, ink = palette$ink_2),
    list(col = 1, row = 0, n = b, key = "b", what = "lost it (worse)", fill = palette$worse, ink = "#ffffff"),
    list(col = 0, row = 1, n = c, key = "c", what = "gained it (better)", fill = palette$better, ink = "#ffffff"),
    list(col = 1, row = 1, n = d, key = "d", what = "never had it", fill = palette$panel, ink = palette$ink_2)
  )
  for (cell in cells) {
    cx <- x0 + cell$col * cw
    cy <- y0 + cell$row * ch
    s <- c(s,
      rect(cx + 2, cy + 2, cw - 4, ch - 4, cell$fill, rx = 6),
      txt(cx + cw / 2, cy + 36, sprintf("%s = %d", cell$key, cell$n), 20, cell$ink, "middle", "600"),
      txt(cx + cw / 2, cy + 58, cell$what, 12, cell$ink, "middle")
    )
  }
  # Right-hand side: what the discordant pairs give.
  rx <- 384
  s <- c(s,
    txt(rx, y0 + 4, "Only b and c carry the change", 13, palette$ink, weight = "600"),
    rect(rx, y0 + 18, 7 * c, 18, palette$better, rx = 4),
    txt(rx + 7 * c + 6, y0 + 32, sprintf("c = %d better", c), 12, palette$ink_2),
    rect(rx, y0 + 44, 7 * b, 18, palette$worse, rx = 4),
    txt(rx + 7 * b + 6, y0 + 58, sprintf("b = %d worse", b), 12, palette$ink_2),
    txt(rx, y0 + 96, sprintf("Odds ratio = c / b = %d / %d = %.1f", c, b, c / b), 13, palette$ink, weight = "600"),
    txt(rx, y0 + 118, "Conditional logistic regression gives", 12, palette$ink_2),
    txt(rx, y0 + 134, "the same odds ratio.", 12, palette$ink_2),
    line(16, 258, w - 16, 258),
    txt(16, 284, "What a clinician asks, from the same table:", 13, palette$ink, weight = "600"),
    txt(16, 308, sprintf(
      "Of the %d without the outcome before, %d gained it: %.0f%%  (c / (c + d))",
      c + d, c, 100 * c / (c + d)
    ), 13, palette$ink_2),
    txt(16, 330, sprintf(
      "Of the %d with the outcome before, %d lost it: %.0f%%  (b / (a + b))",
      a + b, b, 100 * b / (a + b)
    ), 13, palette$ink_2)
  )
  write_svg(s, "discordant_pairs.svg")
}

# ---- F2. Windows around the visit ---------------------------------------------
# Definitions only: the article's windows, as listed on Study design and methods.

build_outcome_windows <- function() {
  w <- 680; h <- 300
  s <- svg_open(
    w, h, "f2",
    "The before and after windows for each outcome",
    paste(
      "A timeline from 12 months before to 12 months after the emergency department visit.",
      "Engagement: a visit in the 6 months before, compared with the 6 months after.",
      "Retention: two visits at least 3 months apart in the 12 months before, compared with the 12 months after.",
      "Viral suppression: a viral load below 200 copies per mL in 12 months before and 12 months after,",
      "with the dividing line moved 10 days after the visit, because a viral load drawn in those days reflects",
      "treatment before the visit."
    )
  )
  left <- 150; right <- w - 24
  scale_x <- function(month) left + (month + 12) / 24 * (right - left)
  axis_y <- 252
  s <- c(s, txt(16, 26, "Each outcome compares a window before the visit with the same window after", 15, weight = "600"))
  rows <- list(
    list(label = "Engagement", detail = "any HIV care visit", start = -6, split = 0, end = 6),
    list(label = "Retention", detail = "2 visits ≥ 3 months apart", start = -12, split = 0, end = 12),
    list(label = "Viral suppression", detail = "a viral load < 200 copies/mL", start = -12 + 1 / 3, split = 1 / 3, end = 12 + 1 / 3)
  )
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    y <- 50 + (i - 1) * 62
    s <- c(s,
      txt(16, y + 16, r$label, 13, palette$ink, weight = "600"),
      txt(16, y + 33, r$detail, 11, palette$ink_2),
      rect(scale_x(r$start), y + 4, scale_x(r$split) - scale_x(r$start) - 1, 22, palette$before, rx = 4),
      rect(scale_x(r$split) + 1, y + 4, scale_x(r$end) - scale_x(r$split) - 1, 22, palette$after, rx = 4),
      txt((scale_x(r$start) + scale_x(r$split)) / 2, y + 19, "before", 11, palette$ink, "middle"),
      txt((scale_x(r$split) + scale_x(r$end)) / 2, y + 19, "after", 11, "#ffffff", "middle")
    )
  }
  s <- c(s,
    txt(scale_x(1 / 3) + 6, 50 + 2 * 62 + 44, "after-window starts on day 11", 11, palette$ink_2),
    line(scale_x(0), 40, scale_x(0), axis_y, palette$ink, 1.5, ' stroke-dasharray="4 3"'),
    txt(scale_x(0), axis_y + 34, "Emergency department visit", 12, palette$ink, "middle", "600"),
    line(left, axis_y, right, axis_y, palette$ink_3, 1)
  )
  for (m in seq(-12, 12, by = 3)) {
    s <- c(s,
      line(scale_x(m), axis_y, scale_x(m), axis_y + 5, palette$ink_3, 1),
      txt(scale_x(m), axis_y + 18, if (m == 0) "0" else sprintf("%+d", m), 11, palette$ink_2, "middle")
    )
  }
  s <- c(s, txt(right, axis_y - 6, "months", 11, palette$ink_3, "end"))
  write_svg(s, "outcome_windows.svg")
}

# ---- F4. Before and after, by outcome (the article's printed values) -------------
# The abstract's percentages; the odds ratios are the Total rows of Tables 2 to 4.

build_change_by_outcome <- function(published) {
  totals <- published[published$row_label == "Total", ]
  outcomes <- data.frame(
    label = c("Engaged in care", "Retained in care", "Virally suppressed"),
    table = c("Table 2", "Table 3", "Table 4"),
    before = c(41.3, 32.6, 22.8),
    after = c(58.8, 47.1, 34.0)
  )
  outcomes$or <- totals$odds_ratio[match(outcomes$table, totals$table)]
  w <- 680; h <- 270
  s <- svg_open(
    w, h, "f4",
    "All three outcomes were more common after the visit than before",
    paste(
      "Percentage of the 2,068 patients with each outcome before and after the RUSH-era visit, from the article.",
      "Engaged in care: 41.3% before, 58.8% after, up 17.5 percentage points.",
      "Retained in care: 32.6% to 47.1%, up 14.5 points.",
      "Virally suppressed, with missing viral loads counted as not suppressed: 22.8% to 34.0%, up 11.2 points."
    )
  )
  left <- 190; right <- w - 70
  scale_x <- function(p) left + p / 70 * (right - left)
  s <- c(s,
    txt(16, 26, "Patients with each outcome, before and after the visit (% of 2,068)", 15, weight = "600"),
    circle(20, 48, 6, palette$before), txt(32, 52, "before", 12, palette$ink_2),
    circle(96, 48, 6, palette$after), txt(108, 52, "after", 12, palette$ink_2)
  )
  for (p in seq(0, 70, by = 10)) {
    s <- c(s,
      line(scale_x(p), 70, scale_x(p), 222, palette$rule, 1),
      txt(scale_x(p), 240, sprintf("%d%%", p), 11, palette$ink_2, "middle")
    )
  }
  for (i in seq_len(nrow(outcomes))) {
    o <- outcomes[i, ]
    y <- 96 + (i - 1) * 50
    s <- c(s,
      txt(16, y + 4, o$label, 13, palette$ink, weight = "600"),
      line(scale_x(o$before), y, scale_x(o$after), y, palette$ink_3, 2),
      circle(scale_x(o$before), y, 7, palette$before, tip = sprintf("%s before: %.1f%%", o$label, o$before)),
      circle(scale_x(o$after), y, 7, palette$after, tip = sprintf("%s after: %.1f%%", o$label, o$after)),
      txt(scale_x(o$before) - 12, y + 4, sprintf("%.1f%%", o$before), 11, palette$ink_2, "end"),
      txt(scale_x(o$after) + 12, y + 4, sprintf("%.1f%%  +%.1f pts", o$after, o$after - o$before), 11, palette$ink, "start")
    )
  }
  s <- c(s, txt(16, 262, "Viral suppression counts a missing viral load as not suppressed.", 11, palette$ink_3))
  write_svg(s, "change_by_outcome.svg")
}

# ---- F6. Odds ratios within each category (the article's printed values) --------

parse_or <- function(x) {
  m <- regmatches(x, regexec("^([0-9.]+) \\(([0-9.]+)[^0-9.]+([0-9.]+)\\)$", x, useBytes = TRUE))[[1]]
  if (length(m) == 4) as.numeric(m[2:4]) else c(NA, NA, NA)
}

build_forest <- function(published, table_name, outcome_label, file) {
  rows <- published[published$table == table_name, ]
  groups <- unique(rows$group)
  n_lines <- nrow(rows) + length(groups) - 1
  row_h <- 22
  top <- 58
  w <- 680
  h <- top + n_lines * row_h + 64
  lo <- 0.1; hi <- 50
  left <- 250; right <- w - 24
  scale_x <- function(v) left + (log10(v) - log10(lo)) / (log10(hi) - log10(lo)) * (right - left)
  desc_rows <- vapply(seq_len(nrow(rows)), function(i) {
    sprintf("%s %s: %s", rows$group[i], rows$row_label[i], rows$odds_ratio[i])
  }, character(1))
  s <- svg_open(
    w, h, sub("\\.svg$", "", file),
    sprintf("%s: odds ratio for the visit within each category (%s)", outcome_label, table_name),
    paste(
      sprintf("Forest plot of the article's %s on a log scale from 0.1 to 50; 1 means no change.", table_name),
      "Each row is a category's odds ratio with its 95% confidence interval.",
      paste(desc_rows, collapse = "; ")
    )
  )
  s <- c(s, txt(16, 26, sprintf("%s: odds ratio after vs before, within each category", outcome_label), 15, weight = "600"),
    txt(16, 44, "Log scale. Right of 1: more patients improved than got worse. P at each heading: does the effect differ between categories?", 11, palette$ink_2))
  y <- top
  axis_bottom <- top + n_lines * row_h
  for (v in c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50)) {
    s <- c(s,
      line(scale_x(v), top - 4, scale_x(v), axis_bottom, if (v == 1) palette$ink_3 else palette$rule, if (v == 1) 1.5 else 1),
      txt(scale_x(v), axis_bottom + 16, format(v), 11, palette$ink_2, "middle")
    )
  }
  for (g in groups) {
    g_rows <- rows[rows$group == g, ]
    if (g != "Total") {
      chi <- g_rows$chi_p[1]
      heading <- if (nzchar(chi)) sprintf("%s  (across groups P = %s)", g, chi) else g
      s <- c(s, txt(16, y + 14, heading, 12, palette$ink, weight = "600"))
      y <- y + row_h
    }
    for (i in seq_len(nrow(g_rows))) {
      r <- g_rows[i, ]
      est <- parse_or(r$odds_ratio)
      label <- if (g == "Total") "Total" else r$row_label
      s <- c(s, txt(if (g == "Total") 16 else 28, y + 14, label, 12, palette$ink_2,
                    weight = if (g == "Total") "600" else "normal"),
             txt(left - 10, y + 14, sprintf("n = %s", r$n), 11, palette$ink_3, "end"))
      if (is.na(est[1])) {
        s <- c(s, txt(scale_x(1) + 8, y + 14, "∞: no patient in this group got worse", 11, palette$ink_2))
      } else {
        x_lo <- scale_x(max(est[2], lo)); x_hi <- scale_x(min(est[3], hi))
        s <- c(s, line(x_lo, y + 10, x_hi, y + 10, palette$after, 2))
        if (est[3] > hi) {
          s <- c(s, sprintf('<path d="M%.1f %.1f l-7 -4 v8 z" fill="%s"/>\n', x_hi + 1, y + 10, palette$after))
        }
        s <- c(s,
          circle(scale_x(est[1]), y + 10, if (g == "Total") 6 else 5, palette$after,
                 tip = sprintf("%s: %s", label, r$odds_ratio))
        )
      }
      y <- y + row_h
    }
  }
  s <- c(s, txt(16, h - 12, sprintf("Values as printed in the article's %s. Wald intervals from conditional logistic regression.", table_name), 11, palette$ink_3))
  write_svg(s, file)
}

# ---- F9. Data shapes (invented patient) -------------------------------------------

build_data_shapes <- function() {
  w <- 680; h <- 266
  s <- svg_open(
    w, h, "f9",
    "From one row per patient to one row per patient and period",
    paste(
      "Three small tables for one invented patient, P1.",
      "Wide: one row with engagement before (0) and after (1).",
      "Long: two rows, one per period, with period 0 before and 1 after.",
      "Stacked eras: four rows when the patient also had a pre-RUSH visit, with era 0 for pre-RUSH and 1 for RUSH.",
      "The patient is the stratum in every model."
    )
  )
  draw_table <- function(x, y, title, header, rows, col_w) {
    out <- txt(x, y, title, 13, palette$ink, weight = "600")
    yy <- y + 14
    out <- c(out, rect(x, yy, sum(col_w), 22, palette$panel, rx = 3))
    cx <- x
    for (j in seq_along(header)) {
      out <- c(out, txt(cx + 6, yy + 15, header[j], 11, palette$ink_2, weight = "600"))
      cx <- cx + col_w[j]
    }
    for (i in seq_along(rows)) {
      yy <- yy + 22
      cx <- x
      for (j in seq_along(rows[[i]])) {
        out <- c(out, txt(cx + 6, yy + 15, rows[[i]][j], 11, palette$ink))
        cx <- cx + col_w[j]
      }
      out <- c(out, line(x, yy + 22, x + sum(col_w), yy + 22, palette$rule, 1))
    }
    out
  }
  s <- c(s,
    txt(16, 24, "Invented patient P1: how the data change shape", 15, weight = "600"),
    draw_table(16, 52, "Wide (one row)", c("id", "before", "after"), list(c("P1", "0", "1")), c(36, 58, 50)),
    txt(185, 92, "→", 22, palette$ink_3),
    draw_table(214, 52, "Long (two rows)", c("id", "period", "engaged"), list(c("P1", "0", "0"), c("P1", "1", "1")), c(36, 56, 66)),
    txt(400, 92, "→", 22, palette$ink_3),
    draw_table(430, 52, "Both eras (four rows)", c("id", "era", "period", "engaged"), list(
      c("P1", "0", "0", "1"), c("P1", "0", "1", "1"), c("P1", "1", "0", "0"), c("P1", "1", "1", "1")
    ), c(32, 40, 52, 64)),
    txt(16, 206, "The patient (id) is the stratum: each model compares P1 after with P1 before.", 12, palette$ink_2),
    txt(16, 226, "A patient whose outcome is the same in both periods adds nothing to the estimate.", 12, palette$ink_2),
    txt(16, 252, "Invented values, for illustration only.", 11, palette$ink_3)
  )
  write_svg(s, "data_shapes.svg")
}

# ---- Build all --------------------------------------------------------------------

published <- read.csv(
  "quarto/published/tables_2_to_4.csv",
  colClasses = "character", na.strings = character(), encoding = "UTF-8"
)
build_discordant_pairs()
build_outcome_windows()
build_change_by_outcome(published)
build_forest(published, "Table 2", "Engagement in care", "forest_table_2.svg")
build_forest(published, "Table 3", "Retention in care", "forest_table_3.svg")
build_forest(published, "Table 4", "Viral suppression", "forest_table_4.svg")
build_data_shapes()
