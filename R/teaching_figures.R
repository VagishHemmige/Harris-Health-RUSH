# Purpose: draw the site's teaching figures with ggplot2 and save them as
# SVG files in quarto/teaching_figures/, which are committed for the site.
# This is not part of the analysis: main.R does not source it, and it reads
# no study data. It uses only the values the article prints (typed into
# quarto/published/tables_2_to_4.csv, and the abstract's percentages in
# R/setup.R) and the invented example of the Methodological background page.
# Run from the project root: Rscript R/teaching_figures.R
# Needs: R/setup.R (packages and figure constants) and R/functions.R
# (parse_odds_ratio_text()), which it sources first, as main.R does.
# Produces: change_by_outcome.svg, discordant_pairs.svg,
# outcome_windows.svg, and forest_table_2.svg to forest_table_4.svg.

source("R/setup.R")
source("R/functions.R")

## ---- figure-theme
# One plain theme for every figure: no grey panel, light vertical grid
# lines only, and text in dark grey rather than in the data colours.
theme_teaching <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(colour = figure_colours[["ink"]]),
      plot.title = element_text(face = "bold", size = 13),
      plot.title.position = "plot",
      plot.subtitle = element_text(colour = figure_colours[["muted"]]),
      plot.caption = element_text(colour = figure_colours[["muted"]], hjust = 0),
      plot.caption.position = "plot",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = figure_colours[["ink"]]),
      legend.position = "top",
      legend.justification = "left"
    )
}

save_teaching_figure <- function(plot, file, height_in) {
  ggsave(
    file.path(teaching_figures_dir, file),
    plot,
    device = svglite::svglite,
    width = figure_width_in,
    height = height_in
  )
}

## ---- figure-change-by-outcome
# Before and after the visit, for each outcome: one row per outcome, a dot
# for each period joined by a line, and the change in percentage points.
change_data <- published_outcome_percentages |>
  mutate(
    outcome = fct_rev(fct_inorder(outcome)),
    change = after - before,
    before_text = format(before, nsmall = 1),
    after_text = format(after, nsmall = 1),
    change_text = format(change, nsmall = 1)
  )
change_long <- change_data |>
  pivot_longer(c(before, after), names_to = "period", values_to = "percent") |>
  mutate(period = factor(period, levels = c("before", "after")))

change_plot <- ggplot(change_data, aes(y = outcome)) +
  geom_segment(
    aes(x = before, xend = after, yend = outcome),
    colour = figure_colours[["muted"]],
    linewidth = 0.8
  ) +
  geom_point(
    data = change_long,
    aes(x = percent, colour = period),
    size = 4
  ) +
  geom_text(
    aes(x = before, label = str_glue("{before_text}%")),
    hjust = 1.4,
    size = 3.5
  ) +
  geom_text(
    aes(x = after, label = str_glue("{after_text}%  +{change_text} points")),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_colour_manual(
    values = figure_colours[c("before", "after")],
    labels = c(before = "Before the visit", after = "After the visit"),
    name = NULL
  ) +
  scale_x_continuous(
    limits = c(0, 80),
    breaks = seq(0, 70, by = 10),
    labels = \(x) str_glue("{x}%")
  ) +
  labs(
    title = "Patients with each outcome, before and after the visit",
    subtitle = "Percentage of the 2,068 patients, from the article's abstract",
    caption = "Viral suppression counts a missing viral load as not suppressed.",
    x = NULL,
    y = NULL
  ) +
  theme_teaching()

save_teaching_figure(change_plot, "change_by_outcome.svg", height_in = 3.4)

## ---- figure-forest-plots
# Tables 2 to 4 as forest plots: each category's odds ratio and 95% interval
# on a log scale, one panel per characteristic, with the across-groups P
# value in the panel's label. A category in which no patient got worse has
# an infinite odds ratio; it gets a note instead of a dot.
published_rows <- read_csv(
  published_outcome_tables_file,
  col_types = cols(.default = col_character()),
  na = character()
)

forest_data <- published_rows |>
  rename(odds_ratio_text = odds_ratio) |>
  bind_cols(parse_odds_ratio_text(published_rows$odds_ratio)) |>
  group_by(table, group) |>
  mutate(
    across_p = first(chi_p),
    panel = case_when(
      group == "Total" ~ " ",
      nzchar(across_p) ~ str_glue("{group}\nP across groups = {across_p}"),
      .default = group
    ),
    row_label = fct_rev(fct_inorder(row_label))
  ) |>
  ungroup()

draw_forest_plot <- function(rows, title) {
  rows <- rows |>
    mutate(panel = fct_inorder(panel))
  infinite_rows <- rows |>
    filter(is.na(odds_ratio), str_detect(odds_ratio_text, "∞"))
  ggplot(rows, aes(y = row_label)) +
    geom_vline(xintercept = 1, colour = figure_colours[["muted"]]) +
    geom_errorbarh(
      aes(xmin = conf_low, xmax = pmin(conf_high, 50)),
      height = 0,
      colour = figure_colours[["after"]],
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    geom_point(
      aes(x = odds_ratio),
      colour = figure_colours[["after"]],
      size = 2.5,
      na.rm = TRUE
    ) +
    geom_text(
      data = infinite_rows,
      aes(x = 1.15, label = "∞: no patient in this group got worse"),
      hjust = 0,
      size = 3.2,
      colour = figure_colours[["muted"]]
    ) +
    facet_grid(panel ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_x_log10(
      limits = c(0.1, 50),
      breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50),
      labels = c("0.1", "0.2", "0.5", "1", "2", "5", "10", "20", "50"),
      oob = scales::squish
    ) +
    labs(
      title = title,
      subtitle = "Odds ratio after vs before the visit, with 95% interval (log scale; 1 = no change)",
      caption = "Values as printed in the article. An interval running past 50 is cut at the edge.",
      x = NULL,
      y = NULL
    ) +
    theme_teaching() +
    theme(
      strip.placement = "outside",
      strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
      panel.spacing.y = unit(0.6, "lines"),
      panel.grid.major.x = element_line(colour = "#ecebe6")
    )
}

forest_titles <- c(
  `Table 2` = "Engagement in care, by group (Table 2)",
  `Table 3` = "Retention in care, by group (Table 3)",
  `Table 4` = "Viral suppression, by group (Table 4)"
)
for (table_name in names(forest_titles)) {
  plot <- forest_data |>
    filter(table == table_name) |>
    draw_forest_plot(forest_titles[[table_name]])
  file <- str_glue("forest_{str_replace(str_to_lower(table_name), ' ', '_')}.svg")
  save_teaching_figure(plot, file, height_in = 7.2)
}

## ---- figure-discordant-pairs
# The invented example as a 2 x 2 table of pairs. The two cells where the
# outcome changed (b and c) are coloured; they carry all the information
# about change. The caption gives the odds ratio and the two transitions.
pairs_cells <- tibble(
  cell = c("a", "b", "c", "d"),
  before = c("Before: yes", "Before: yes", "Before: no", "Before: no"),
  after = c("After: yes", "After: no", "After: yes", "After: no"),
  meaning = c("had it both times", "lost it (worse)", "gained it (better)", "never had it"),
  kind = c("neither", "worse", "better", "neither"),
  n = example_pairs[c("a", "b", "c", "d")]
) |>
  mutate(
    before = factor(before, levels = c("Before: no", "Before: yes")),
    after = factor(after, levels = c("After: yes", "After: no")),
    label = str_glue("{cell} = {n}\n{meaning}"),
    text_colour = if_else(kind == "neither", "ink", "white")
  )

pairs_plot <- ggplot(pairs_cells, aes(x = after, y = before)) +
  geom_tile(aes(fill = kind), colour = "white", linewidth = 3) +
  geom_text(aes(label = label, colour = text_colour), size = 4.2, lineheight = 1.1) +
  scale_fill_manual(values = figure_colours[c("neither", "worse", "better")], guide = "none") +
  scale_colour_manual(values = c(ink = figure_colours[["ink"]], white = "white"), guide = "none") +
  scale_x_discrete(position = "top") +
  coord_fixed(ratio = 0.55) +
  labs(
    title = "Invented example: 100 people, before and after",
    subtitle = str_glue(
      "Only b and c carry the change. Odds ratio = c / b = ",
      "{example_pairs[['c']]} / {example_pairs[['b']]} = ",
      "{format(example_pairs[['c']] / example_pairs[['b']], nsmall = 1)}"
    ),
    caption = str_glue(
      "Of the {example_pairs[['c']] + example_pairs[['d']]} without the outcome before, ",
      "{example_pairs[['c']]} gained it: ",
      "{round(100 * example_pairs[['c']] / (example_pairs[['c']] + example_pairs[['d']]))}%.\n",
      "Of the {example_pairs[['a']] + example_pairs[['b']]} with the outcome before, ",
      "{example_pairs[['b']]} lost it: ",
      "{round(100 * example_pairs[['b']] / (example_pairs[['a']] + example_pairs[['b']]))}%."
    ),
    x = NULL,
    y = NULL
  ) +
  theme_teaching() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12)
  )

save_teaching_figure(pairs_plot, "discordant_pairs.svg", height_in = 4.2)

## ---- figure-outcome-windows
# The windows before and after the visit, in months: one bar per outcome and
# period. Viral suppression's dividing line is 10 days after the visit.
window_data <- outcome_windows |>
  mutate(
    outcome = fct_rev(fct_inorder(str_glue("{outcome}\n{definition}"))),
    row = as.integer(outcome)
  ) |>
  pivot_longer(c(start, end), names_to = "edge", values_to = "month") |>
  mutate(
    period = if_else(edge == "start", "before", "after"),
    xmin = if_else(period == "before", month, split),
    xmax = if_else(period == "before", split, month)
  )

windows_plot <- ggplot(window_data) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = row - 0.3, ymax = row + 0.3, fill = period),
    colour = "white",
    linewidth = 1
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = figure_colours[["ink"]]) +
  annotate(
    "text", x = 0, y = 3.65, label = "Emergency department visit",
    size = 3.6, fontface = "bold", colour = figure_colours[["ink"]]
  ) +
  scale_fill_manual(
    values = figure_colours[c("before", "after")],
    breaks = c("before", "after"),
    labels = c(before = "Before window", after = "After window"),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(-12, 12, by = 3),
    labels = \(x) if_else(x > 0, str_glue("+{x}"), as.character(x))
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = levels(window_data$outcome),
    expand = expansion(add = c(0.5, 0.6))
  ) +
  labs(
    title = "Each outcome compares a window before the visit with the same window after",
    caption = "Viral suppression's after window starts 10 days after the visit, drawn to scale.",
    x = "Months from the visit",
    y = NULL
  ) +
  theme_teaching()

save_teaching_figure(windows_plot, "outcome_windows.svg", height_in = 3.6)
