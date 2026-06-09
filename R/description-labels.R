# Label helpers -----------------------------------------------------------

#' Create output labels for birtir summaries
#'
#' @param table Label for tables.
#' @param figure Label for figures.
#' @param variable Label for variables.
#' @param summary Label for summaries.
#' @param type Label for variable type.
#' @param formula Label for formulas.
#' @param missing Label for missingness.
#'
#' @return A `description_labels` object.
#' @export
description_labels <- function(table = "Table",
                           figure = "Figure",
                           variable = "Variable",
                           summary = "Summary",
                           type = "Type",
                           formula = "Formula",
                           missing = "Missing") {
  labels <- list(
    table = table,
    figure = figure,
    variable = variable,
    summary = summary,
    type = type,
    formula = formula,
    missing = missing
  )

  validate_description_labels(labels)
  structure(labels, class = "description_labels")
}

validate_description_labels <- function(labels) {
  if (!is.list(labels)) {
    cli::cli_abort("`labels` must be created with `description_labels()`.")
  }

  required <- c("table", "figure", "variable", "summary", "type", "formula", "missing")
  missing <- setdiff(required, names(labels))

  if (length(missing) > 0) {
    cli::cli_abort("`labels` is missing required fields: {paste(missing, collapse = ', ')}.")
  }

  invalid <- !vapply(labels[required], is_label_value, logical(1))

  if (any(invalid)) {
    cli::cli_abort("All `labels` fields must be non-empty character strings.")
  }

  invisible(labels)
}

is_label_value <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}
