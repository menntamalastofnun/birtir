# Inline plot helpers ------------------------------------------------------

format_number <- function(x, digits = 2) {
  fmt_num(x, digits = digits, style = "plain")
}

#' Inline histogram
#'
#' @param x Numeric vector.
#' @param n_bins Number of bins in the histogram.
#'
#' @returns A compact character string with one bar per bin.
#' @export
#'
#' @examples
#' inline_hist(c(1, 2, 3, 4, 5))
#' x <- c(41, 53, 49, 52, NA, 47)
#' inline_hist(x)
inline_hist <- function(x, n_bins = 5) {
  validate_numeric_vector(x, "x")
  validate_positive_integerish_scalar(n_bins, "n_bins", minimum = 1)

  bars <- c("\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2588")
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return("")
  }

  cuts <- cut(x, breaks = as.integer(n_bins))
  counts <- table(cuts)
  idx <- ceiling(counts / max(counts) * length(bars))
  idx <- pmax(idx, 1L)
  paste(bars[idx], collapse = "")
}

#' Inline boxplot
#'
#' @param x Numeric vector.
#' @param width Width of the inline plot in characters.
#'
#' @returns A `birtir_inline_boxplot` object.
#' @export
#'
#' @examples
#' inline_boxplot(c(1, 2, 3, 4, 5))
#' x <- c(41, 53, 49, 52, NA, 47)
#' inline_boxplot(x)
inline_boxplot <- function(x, width = 30) {
  validate_numeric_vector(x, "x")
  validate_positive_integerish_scalar(width, "width", minimum = 5)

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(new_birtir_inline_boxplot("[no observed values]", ""))
  }

  width <- as.integer(width)
  box_stats <- grDevices::boxplot.stats(x)
  whisker_low <- box_stats$stats[[1]]
  q1 <- box_stats$stats[[2]]
  med <- box_stats$stats[[3]]
  q3 <- box_stats$stats[[4]]
  whisker_high <- box_stats$stats[[5]]
  outliers <- box_stats$out
  plot_min <- min(c(whisker_low, outliers))
  plot_max <- max(c(whisker_high, outliers))

  if (identical(plot_min, plot_max)) {
    line1 <- paste0(rep("\u2500", width), collapse = "")
    line1 <- paste0(substr(line1, 1, floor(width / 2) - 1), "\u2502", substr(line1, floor(width / 2) + 1, width))
    line2 <- center_label(as.character(med), width)
    return(new_birtir_inline_boxplot(line1, line2))
  }

  scale_to <- function(v) {
    round((v - plot_min) / (plot_max - plot_min) * (width - 1)) + 1
  }

  p_whisker_low <- scale_to(whisker_low)
  p_q1 <- scale_to(q1)
  p_med <- scale_to(med)
  p_q3 <- scale_to(q3)
  p_whisker_high <- scale_to(whisker_high)

  chars <- rep(" ", width)
  chars[p_whisker_low:p_whisker_high] <- "\u2500"
  chars[p_whisker_low] <- "\u251c"
  chars[p_whisker_high] <- "\u2524"
  chars[p_q1:p_q3] <- "\u2591"
  chars[p_q1] <- "|"
  chars[p_q3] <- "|"
  chars[p_med] <- "\u2502"

  if (length(outliers) > 0) {
    chars[scale_to(outliers)] <- "*"
  }

  line1 <- paste(chars, collapse = "")
  line2 <- inline_label_line(
    positions = c(p_whisker_low, p_q1, p_med, p_q3, p_whisker_high),
    labels = round(c(whisker_low, q1, med, q3, whisker_high)),
    width = width
  )

  new_birtir_inline_boxplot(line1, line2)
}

inline_label_line <- function(positions, labels, width, min_gap = 1L) {
  labels <- as.character(labels)
  order <- order(positions)
  positions <- positions[order]
  labels <- labels[order]
  label_widths <- nchar(labels)

  starts <- pmax(positions - floor(label_widths / 2), 1L)
  starts <- pmin(starts, width - label_widths + 1L)

  for (i in seq_along(starts)) {
    if (i > 1) {
      starts[[i]] <- max(starts[[i]], starts[[i - 1]] + label_widths[[i - 1]] + min_gap)
    }
  }

  for (i in rev(seq_along(starts))) {
    starts[[i]] <- min(starts[[i]], width - label_widths[[i]] + 1L)

    if (i < length(starts)) {
      starts[[i]] <- min(starts[[i]], starts[[i + 1]] - label_widths[[i]] - min_gap)
    }
  }

  starts <- pmax(starts, 1L)
  line <- rep(" ", width)

  for (i in seq_along(labels)) {
    end <- min(starts[[i]] + label_widths[[i]] - 1L, width)
    label_chars <- strsplit(labels[[i]], "")[[1]]
    line[starts[[i]]:end] <- label_chars[seq_len(end - starts[[i]] + 1L)]
  }

  paste(line, collapse = "")
}

new_birtir_inline_boxplot <- function(line1, line2) {
  validate_character_scalar(line1, "line1", allow_empty = TRUE)
  validate_character_scalar(line2, "line2", allow_empty = TRUE)

  structure(
    list(
      line1 = line1,
      line2 = line2
    ),
    class = "birtir_inline_boxplot"
  )
}

#' @export
print.birtir_inline_boxplot <- function(x, ...) {
  cat(x$line1, "\n", x$line2, "\n", sep = "")
  invisible(x)
}

center_label <- function(label, width) {
  chars <- rep(" ", width)
  label_chars <- strsplit(label, "")[[1]]
  start <- max(floor((width - length(label_chars)) / 2) + 1, 1L)
  end <- min(start + length(label_chars) - 1L, width)
  chars[start:end] <- label_chars[seq_len(end - start + 1)]
  paste(chars, collapse = "")
}

#' Inline bar chart
#'
#' @param values Named numeric vector of values, such as group means.
#' @param width Width of the inline plot in characters.
#' @param digits Number of decimal places for printed values.
#'
#' @returns A `birtir_inline_bar` object.
#' @export
#'
#' @examples
#' inline_bar(c(g0 = 49.1, g1 = 51.4))
#' x <- c(41, 53, 49, 52, NA, 47)
#' group <- factor(c("a", "a", "b", "b", "a", "b"))
#' inline_bar(tapply(x, group, mean, na.rm = TRUE))
inline_bar <- function(values, width = 20, digits = 1) {
  validate_numeric_vector(values, "values")
  validate_positive_integerish_scalar(width, "width", minimum = 3)
  validate_non_negative_integerish_scalar(digits, "digits")

  if (is.null(names(values))) {
    cli::cli_abort("`values` must be a named numeric vector.")
  }

  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(new_birtir_inline_bar(character(), character()))
  }

  width <- as.integer(width)
  digits <- as.integer(digits)
  magnitudes <- abs(values)
  max_v <- max(magnitudes)

  if (identical(max_v, 0)) {
    max_v <- 1
  }

  label_width <- max(nchar(names(values)))

  rows <- mapply(
    FUN = format_inline_bar_row,
    lab = names(values),
    val = values,
    magnitude = magnitudes,
    MoreArgs = list(max_v = max_v, width = width, label_width = label_width, digits = digits),
    USE.NAMES = FALSE
  )

  new_birtir_inline_bar(rows, names(values))
}

format_inline_bar_row <- function(lab, val, magnitude, max_v, width, label_width, digits) {
  n_fill <- max(round(magnitude / max_v * width), 1L)
  n_fill <- min(n_fill, width)
  bar <- paste0(
    paste(rep("\u2588", n_fill - 1L), collapse = ""),
    "\u2593"
  )
  bar <- formatC(bar, width = width, flag = "-")
  paste0(formatC(lab, width = label_width), " \u2502", bar, "  ", format_number(val, digits))
}

new_birtir_inline_bar <- function(rows, labels) {
  validate_character_vector(rows, "rows")
  validate_character_vector(labels, "labels")

  structure(
    list(
      rows = rows,
      labels = labels
    ),
    class = "birtir_inline_bar"
  )
}

#' @export
print.birtir_inline_bar <- function(x, ...) {
  cat(paste(x$rows, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' Inline forest plot
#'
#' @param estimate Numeric vector of estimates, or a data frame containing
#'   estimate and confidence interval columns.
#' @param ci_low Lower confidence interval bound. Ignored when `estimate` is a
#'   data frame with a recognized lower-bound column.
#' @param ci_high Upper confidence interval bound. Ignored when `estimate` is a
#'   data frame with a recognized upper-bound column.
#' @param labels Optional term labels. Defaults to names of `estimate` or a
#'   `term`, `variable`, `label`, or `name` column when `estimate` is a data
#'   frame.
#' @param n Optional row counts. If supplied, a count column is printed before
#'   the estimate.
#' @param p Optional p-values. If supplied, stars mark `p < .05`; otherwise
#'   stars mark confidence intervals that do not include `zero`.
#' @param width Width of each inline confidence interval.
#' @param digits Number of decimal places for estimates and intervals.
#' @param title Printed title.
#' @param zero Reference value for significance and zero-line display.
#' @param show_stars Logical; if `TRUE`, mark rows with stars using p-values
#'   when supplied or confidence intervals that do not include `zero`.
#'
#' @returns A `birtir_inline_forest` object.
#' @export
#'
#' @examples
#' inline_forest(
#'   data.frame(
#'     term = c("x1", "x2", "g"),
#'     estimate = c(0.31, 0.18, 0.94),
#'     ci_low = c(0.12, 0.05, 0.41),
#'     ci_high = c(0.50, 0.31, 1.47)
#'   )
#' )
inline_forest <- function(estimate,
                          ci_low = NULL,
                          ci_high = NULL,
                          labels = NULL,
                          n = NULL,
                          p = NULL,
                          width = 12,
                          digits = 2,
                          title = "Coefficients",
                          zero = 0,
                          show_stars = TRUE) {
  validate_positive_integerish_scalar(width, "width", minimum = 5)
  validate_non_negative_integerish_scalar(digits, "digits")
  validate_numeric_scalar(zero, "zero")
  validate_character_scalar(title, "title")
  validate_logical_scalar(show_stars, "show_stars")

  data <- parse_inline_forest_input(
    estimate = estimate,
    ci_low = ci_low,
    ci_high = ci_high,
    labels = labels,
    n = n,
    p = p
  )

  width <- as.integer(width)
  digits <- as.integer(digits)
  keep <- !is.na(data$estimate) & !is.na(data$ci_low) & !is.na(data$ci_high) & !is.na(data$label)
  data <- data[keep, , drop = FALSE]

  if (nrow(data) == 0) {
    lines <- c(print_rule(title), "No coefficients to display.")
    return(new_birtir_inline_forest(lines, character(), data, title))
  }

  if (any(data$ci_low > data$ci_high)) {
    cli::cli_abort("`ci_low` must be less than or equal to `ci_high`.")
  }

  x_min <- min(data$ci_low)
  x_max <- max(data$ci_high)

  if (identical(x_min, x_max)) {
    x_min <- x_min - 1
    x_max <- x_max + 1
  }

  label_width <- max(nchar(data$label))
  show_n <- any(!is.na(data$n))
  n_width <- if (isTRUE(show_n)) {
    max(nchar(c("n", format(data$n[!is.na(data$n)], trim = TRUE, scientific = FALSE))))
  } else {
    0L
  }
  stars <- if (isTRUE(show_stars)) forest_stars(data, zero = zero) else rep(FALSE, nrow(data))
  rows <- mapply(
    FUN = format_inline_forest_row,
    label = data$label,
    estimate = data$estimate,
    ci_low = data$ci_low,
    ci_high = data$ci_high,
    n = data$n,
    star = stars,
    MoreArgs = list(
      width = width,
      digits = digits,
      label_width = label_width,
      n_width = n_width,
      show_n = show_n,
      x_min = x_min,
      x_max = x_max,
      zero = zero
    ),
    USE.NAMES = FALSE
  )

  heading <- paste0(
    strrep(" ", label_width + 2 + width + 2),
    if (isTRUE(show_n)) paste0(formatC("n", width = n_width), "  ") else "",
    "estimate    [95% CI]"
  )
  lines <- c(print_rule(title), heading, rows)

  new_birtir_inline_forest(lines, rows, data, title)
}

parse_inline_forest_input <- function(estimate, ci_low, ci_high, labels, n, p) {
  if (is.data.frame(estimate)) {
    data <- estimate
    estimate <- forest_data_column(data, c("estimate", "est", "coef", "coefficient", "beta"), "estimate")
    ci_low <- if (is.null(ci_low)) forest_data_column(data, c("ci_low", "conf.low", "lower", "lwr"), "ci_low") else ci_low
    ci_high <- if (is.null(ci_high)) forest_data_column(data, c("ci_high", "conf.high", "upper", "upr"), "ci_high") else ci_high
    labels <- if (is.null(labels)) forest_data_column(data, c("term", "variable", "label", "name"), "labels") else labels
    n <- if (is.null(n)) forest_optional_data_column(data, c("n", "count")) else n
    p <- if (is.null(p)) forest_optional_data_column(data, c("p", "p_value", "p.value")) else p
  }

  validate_numeric_vector(estimate, "estimate")
  validate_numeric_vector(ci_low, "ci_low")
  validate_numeric_vector(ci_high, "ci_high")
  validate_same_length(ci_low, estimate, "ci_low", "estimate")
  validate_same_length(ci_high, estimate, "ci_high", "estimate")

  if (is.null(labels)) {
    labels <- names(estimate)
  }

  if (is.null(labels)) {
    labels <- as.character(seq_along(estimate))
  }

  validate_character_vector(labels, "labels")
  validate_same_length(labels, estimate, "labels", "estimate")

  if (!is.null(n)) {
    validate_count_vector(n, "n")
    validate_same_length(n, estimate, "n", "estimate")
  } else {
    n <- rep(NA_real_, length(estimate))
  }

  if (!is.null(p)) {
    validate_numeric_vector(p, "p")
    validate_same_length(p, estimate, "p", "estimate")
  } else {
    p <- rep(NA_real_, length(estimate))
  }

  data.frame(
    label = as.character(labels),
    estimate = as.numeric(estimate),
    ci_low = as.numeric(ci_low),
    ci_high = as.numeric(ci_high),
    n = as.numeric(n),
    p = as.numeric(p),
    stringsAsFactors = FALSE
  )
}

forest_data_column <- function(data, candidates, arg) {
  found <- intersect(candidates, names(data))

  if (length(found) == 0) {
    cli::cli_abort("`estimate` data frame must include a `{candidates[[1]]}` column for `{arg}`.")
  }

  data[[found[[1]]]]
}

forest_optional_data_column <- function(data, candidates) {
  found <- intersect(candidates, names(data))

  if (length(found) == 0) {
    return(NULL)
  }

  data[[found[[1]]]]
}

forest_stars <- function(data, zero) {
  has_p <- any(!is.na(data$p))

  if (has_p) {
    return(!is.na(data$p) & data$p < 0.05)
  }

  data$ci_low > zero | data$ci_high < zero
}

format_inline_forest_row <- function(label, estimate, ci_low, ci_high, n, star,
                                     width, digits, label_width, n_width, show_n,
                                     x_min, x_max, zero) {
  plot <- rep(" ", width)
  p_low <- scale_inline_value(ci_low, x_min, x_max, width)
  p_high <- scale_inline_value(ci_high, x_min, x_max, width)
  p_estimate <- scale_inline_value(estimate, x_min, x_max, width)

  plot[p_low:p_high] <- "\u2500"

  if (zero >= x_min && zero <= x_max) {
    plot[scale_inline_value(zero, x_min, x_max, width)] <- "\u2502"
  }

  plot[p_estimate] <- "\u25cf"

  paste0(
    formatC(label, width = label_width, flag = "-"),
    "  ",
    paste(plot, collapse = ""),
    "  ",
    if (isTRUE(show_n)) paste0(format_inline_count(n, n_width), "  ") else "",
    format_number(estimate, digits),
    " [",
    format_number(ci_low, digits),
    ", ",
    format_number(ci_high, digits),
    "]",
    if (isTRUE(star)) " *" else ""
  )
}

format_inline_count <- function(n, width) {
  if (is.na(n)) {
    return(strrep(" ", width))
  }

  formatC(n, width = width, format = "f", digits = 0)
}

new_birtir_inline_forest <- function(lines, rows, data, title) {
  validate_character_vector(lines, "lines")
  validate_character_vector(rows, "rows")
  validate_character_scalar(title, "title")

  structure(
    list(
      lines = lines,
      rows = rows,
      data = data,
      title = title
    ),
    class = "birtir_inline_forest"
  )
}

#' @export
print.birtir_inline_forest <- function(x, ...) {
  cat(paste(x$lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' Inline scatter plot
#'
#' @param x Numeric vector for the horizontal axis.
#' @param y Numeric vector for the vertical axis.
#' @param width Width of the plot in characters.
#' @param height Height of the plot in characters.
#'
#' @returns A `birtir_inline_scatter` object.
#' @export
#'
#' @examples
#' inline_scatter(rnorm(50), rnorm(50))
inline_scatter <- function(x, y, width = 30, height = 8) {
  validate_numeric_vector(x, "x")
  validate_numeric_vector(y, "y")

  if (length(x) != length(y)) {
    cli::cli_abort("`x` and `y` must have the same length.")
  }

  validate_positive_integerish_scalar(width, "width", minimum = 3)
  validate_positive_integerish_scalar(height, "height", minimum = 3)

  keep <- !is.na(x) & !is.na(y)
  x <- x[keep]
  y <- y[keep]

  if (length(x) == 0) {
    return(new_birtir_inline_scatter(character(), "", "", NA_real_, NA_real_, NA_real_, NA_real_))
  }

  width <- as.integer(width)
  height <- as.integer(height)

  mn_x <- min(x)
  mx_x <- max(x)
  mn_y <- min(y)
  mx_y <- max(y)

  if (identical(mn_x, mx_x)) {
    mn_x <- mn_x - 1
  }

  if (identical(mn_y, mx_y)) {
    mn_y <- mn_y - 1
  }

  col <- scale_inline_value(x, mn_x, mx_x, width)
  row <- scale_inline_value(y, mn_y, mx_y, height)
  row <- (height + 1L) - row

  density_char <- c("\u00b7", "\u2591", "\u2592", "\u2593", "\u2588")
  grid <- matrix(" ", nrow = height, ncol = width)
  count_grid <- matrix(0L, nrow = height, ncol = width)

  for (i in seq_along(col)) {
    r <- row[[i]]
    cc <- col[[i]]
    count_grid[r, cc] <- count_grid[r, cc] + 1L
    idx <- min(count_grid[r, cc], length(density_char))
    grid[r, cc] <- density_char[[idx]]
  }

  y_labels <- rep("     ", height)
  y_labels[[1]] <- formatC(round(mx_y, 1), width = 5)
  y_labels[[max(height %/% 2, 1L)]] <- formatC(round((mn_y + mx_y) / 2, 1), width = 5)
  y_labels[[height]] <- formatC(round(mn_y, 1), width = 5)

  lines <- vapply(
    seq_len(height),
    function(i) paste0(y_labels[[i]], " \u2502 ", paste(grid[i, ], collapse = "")),
    character(1)
  )

  x_axis <- paste0("      \u2514", paste(rep("\u2500", width + 1), collapse = ""))
  x_lab_l <- as.character(round(mn_x, 1))
  x_lab_r <- as.character(round(mx_x, 1))
  padding <- max(width - nchar(x_lab_l) - nchar(x_lab_r), 1L)
  x_labels <- paste0("       ", x_lab_l, strrep(" ", padding), x_lab_r)

  new_birtir_inline_scatter(
    lines,
    x_axis,
    x_labels,
    round(mn_x, 1),
    round(mx_x, 1),
    round(mn_y, 1),
    round(mx_y, 1)
  )
}

scale_inline_value <- function(v, mn, mx, n) {
  round((v - mn) / (mx - mn) * (n - 1)) + 1
}

new_birtir_inline_scatter <- function(lines, x_axis, x_labels,
                                       mn_x, mx_x, mn_y, mx_y) {
  validate_character_vector(lines, "lines")
  validate_character_scalar(x_axis, "x_axis", allow_empty = TRUE)
  validate_character_scalar(x_labels, "x_labels", allow_empty = TRUE)

  structure(
    list(
      lines = lines,
      x_axis = x_axis,
      x_labels = x_labels,
      mn_x = mn_x,
      mx_x = mx_x,
      mn_y = mn_y,
      mx_y = mx_y
    ),
    class = "birtir_inline_scatter"
  )
}

#' @export
print.birtir_inline_scatter <- function(x, ...) {
  cat(paste(x$lines, collapse = "\n"), "\n", sep = "")
  cat(x$x_axis, "\n", sep = "")
  cat(x$x_labels, "\n", sep = "")
  invisible(x)
}

validate_numeric_vector <- function(x, arg) {
  if (!is.numeric(x)) {
    cli::cli_abort("`{arg}` must be numeric.")
  }

  invisible(x)
}

validate_count_vector <- function(x, arg) {
  validate_numeric_vector(x, arg)

  invalid <- !is.na(x) & (!is.finite(x) | x < 0 | x != round(x))
  if (any(invalid)) {
    cli::cli_abort("`{arg}` must be a non-negative whole-number vector.")
  }

  invisible(x)
}

validate_numeric_scalar <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
    cli::cli_abort("`{arg}` must be a single finite number.")
  }

  invisible(x)
}

validate_logical_scalar <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("`{arg}` must be `TRUE` or `FALSE`.")
  }

  invisible(x)
}

validate_non_negative_integerish_scalar <- function(x, arg) {
  if (!is_whole_number(x) || x < 0) {
    cli::cli_abort("`{arg}` must be a single non-negative whole number.")
  }

  invisible(x)
}

validate_positive_integerish_scalar <- function(x, arg, minimum = 1) {
  if (!is_whole_number(x) || x < minimum) {
    cli::cli_abort("`{arg}` must be a single whole number greater than or equal to {minimum}.")
  }

  invisible(x)
}

is_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x == round(x)
}

validate_character_vector <- function(x, arg) {
  if (!is.character(x)) {
    cli::cli_abort("`{arg}` must be a character vector.")
  }

  invisible(x)
}

validate_character_scalar <- function(x, arg, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("`{arg}` must be a single character string.")
  }

  if (!allow_empty && !nzchar(x)) {
    cli::cli_abort("`{arg}` must not be empty.")
  }

  invisible(x)
}

validate_same_length <- function(x, y, x_arg, y_arg) {
  if (length(x) != length(y)) {
    cli::cli_abort("`{x_arg}` must have the same length as `{y_arg}`.")
  }

  invisible(x)
}
