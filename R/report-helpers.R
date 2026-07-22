# Report compatibility helpers --------------------------------------------

#' Convert a birtir description to report text
#'
#' @param x A `birtir_description` object.
#' @param ... Reserved for future options.
#'
#' @return A single character string.
#' @export
#'
#' @examples
#' df <- data.frame(score = c(10, 20, 30))
#' desc <- describe_data(df, score ~ 1)
#' as_report_text(desc)
as_report_text <- function(x, ...) {
  UseMethod("as_report_text")
}

#' @export
as_report_text.birtir_description <- function(x, ...) {
  lines <- utils::capture.output(print(x))
  text <- paste(lines, collapse = "\n")
  text <- gsub("\\bMean = ", "*M* = ", text)
  text <- gsub("\\bSD = ", "*SD* = ", text)
  text <- gsub("\\bSE = ", "*SE* = ", text)
  text <- gsub("\\bn = ", "*n* = ", text)
  text <- gsub("\\br = ", "*r* = ", text)
  text
}

#' Convert a birtir description to a report table
#'
#' @param x A `birtir_description` object.
#' @param ... Reserved for future options.
#'
#' @return A tibble suitable for report rendering.
#' @export
#'
#' @examples
#' df <- data.frame(score = c(10, 20, 30))
#' desc <- describe_data(df, score ~ 1)
#' as_report_table(desc)
as_report_table <- function(x, ...) {
  UseMethod("as_report_table")
}

#' @export
as_report_table.birtir_description <- function(x, ...) {
  if (inherits(x, "birtir_relationship")) {
    return(report_relationship_table(x))
  }

  if (inherits(x, "birtir_grouped")) {
    return(report_grouped_table(x))
  }

  report_summary_table(x$summary, variable = x$variable, type = x$type)
}

report_summary_table <- function(summary, variable, type) {
  if (identical(type, "continuous")) {
    return(tibble::tibble(
      variable = variable,
      type = type,
      n = summary$n,
      n_missing = summary$n_missing,
      pct_missing = summary$pct_missing,
      mean = summary$mean,
      median = summary$median,
      sd = summary$sd,
      se = summary$se,
      ci_low = summary$ci_low,
      ci_high = summary$ci_high,
      min = summary$min,
      max = summary$max
    ))
  }

  if (type %in% c("nominal", "ordinal", "logical")) {
    levels <- summary$levels[[1]]
    return(tibble::tibble(
      variable = variable,
      type = type,
      level = levels$level,
      n = levels$n,
      pct = levels$pct,
      n_missing = summary$n_missing,
      pct_missing = summary$pct_missing
    ))
  }

  tibble::as_tibble(summary)
}

report_grouped_table <- function(x) {
  summary <- x$summary
  keep <- setdiff(names(summary), c("hist", "box", "box_labels", "outliers", "outlier_summary"))
  out <- summary[keep]
  out$variable <- x$variable
  out[, c("variable", setdiff(names(out), "variable")), drop = FALSE]
}

report_relationship_table <- function(x) {
  relationships <- x$relationships
  keep <- setdiff(names(relationships), "detail")
  relationships[keep]
}
