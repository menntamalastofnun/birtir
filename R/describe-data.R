#' Describe data using formula syntax
#'
#' `describe_data()` is the main entry point for formula-based data
#' description. It supports univariate summaries, grouped continuous
#' summaries, relationship summaries such as `y ~ x` and `y ~ x + g`,
#' interactions such as `y ~ x:g`, and random-intercept-style summaries such
#' as `x ~ (1 | g)`.
#'
#' @param data A data frame.
#' @param formula A formula describing the variable and optional grouping
#'   variables.
#' @param labels A label object created with `description_labels()`.
#' @param show_random_effects Logical; if `TRUE`, print compact summaries for
#'   random-effect-style grouping terms such as `(1 | g)`.
#' @param max_random_effects Maximum number of grouping levels to print per
#'   random-effect term.
#'
#' @return A `birtir_description` object.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   y = c(12, 15, 18, 22, 25, 30),
#'   group = factor(c("A", "A", "A", "B", "B", "B")),
#'   x = c(1, 2, 3, 4, 5, 6)
#' )
#' describe_data(df, y ~ 1)
#' describe_data(df, y ~ group)
#' describe_data(df, y ~ x)
describe_data <- function(data,
                          formula,
                          labels = description_labels(),
                          show_random_effects = TRUE,
                          max_random_effects = 8) {
  validate_data_frame(data)
  validate_formula(formula)
  validate_description_labels(labels)
  validate_logical_scalar(show_random_effects, "show_random_effects")
  validate_positive_integerish_scalar(max_random_effects, "max_random_effects", minimum = 1)

  data <- coerce_formula_characters(data, formula)
  variable <- formula_lhs_name(formula)

  if (!variable %in% names(data)) {
    cli::cli_abort("Variable `{variable}` was not found in `data`.")
  }

  type <- read_variable_type(data[[variable]])

  if (!is_ungrouped_formula(formula)) {
    random_terms <- formula_random_effect_terms(formula)

    if (length(random_terms) > 0) {
      if (!identical(type, "continuous")) {
        cli::cli_abort(c(
          "Random-effect descriptions currently require a continuous left-hand side.",
          "i" = "Variable `{variable}` is `{type}`."
        ))
      }

      rhs_terms <- random_effect_rhs_terms(formula, random_terms)
      validate_grouping_terms(data, rhs_terms$groups)

      summary <- summarise_grouped_continuous(data, variable, rhs_terms$groups)
      overall_summary <- summarise_continuous(data[[variable]])

      return(new_birtir_description(
        formula = formula,
        variable = variable,
        type = type,
        summary = summary,
        overall_summary = overall_summary,
        notes = character(),
        labels = labels,
        groups = rhs_terms$groups,
        rhs_terms = rhs_terms,
        random_effects = random_terms,
        show_random_effects = show_random_effects,
        max_random_effects = max_random_effects,
        subclass = c("birtir_random_effects", "birtir_grouped")
      ))
    }

    rhs_terms <- formula_rhs_terms(formula)

    if (identical(type, "continuous") && formula_terms_are_grouping_variables(data, rhs_terms)) {
      validate_grouping_terms(data, rhs_terms$groups)

      summary <- summarise_grouped_continuous(data, variable, rhs_terms$groups)
      overall_summary <- summarise_continuous(data[[variable]])

      return(new_birtir_description(
        formula = formula,
        variable = variable,
        type = type,
        summary = summary,
        overall_summary = overall_summary,
        notes = character(),
        labels = labels,
        groups = rhs_terms$groups,
        rhs_terms = rhs_terms,
        subclass = "birtir_grouped"
      ))
    }

    relationship <- summarise_relationship_description(data, variable, type, formula, rhs_terms)

    return(new_birtir_description(
      formula = formula,
      variable = variable,
      type = type,
      summary = relationship$outcome_summary,
      variable_summaries = relationship$variable_summaries,
      relationships = relationship$relationships,
      notes = relationship$notes,
      labels = labels,
      groups = NULL,
      rhs_terms = rhs_terms,
      subclass = "birtir_relationship"
    ))
  }

  summary <- summarise_variable(data[[variable]], type)

  new_birtir_description(
    formula = formula,
    variable = variable,
    type = type,
    summary = summary,
    notes = character(),
    labels = labels,
    groups = NULL,
    rhs_terms = NULL,
    subclass = "birtir_univariate"
  )
}

validate_data_frame <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  }

  invisible(data)
}

coerce_formula_characters <- function(data, formula) {
  variables <- all.vars(formula)
  variables <- intersect(variables, names(data))

  for (variable in variables) {
    if (is.character(data[[variable]])) {
      data[[variable]] <- factor(data[[variable]])
    }
  }

  data
}

validate_grouping_terms <- function(data, groups) {
  if (length(groups) == 0) {
    cli::cli_abort(c(
      "`formula` must include at least one grouping variable on the right-hand side.",
      "i" = "Use `x ~ 1` for an ungrouped description."
    ))
  }

  grouping_variables <- unique(unlist(groups, use.names = FALSE))

  for (variable in grouping_variables) {
    if (!variable %in% names(data)) {
      cli::cli_abort("Grouping variable `{variable}` was not found in `data`.")
    }

    type <- read_variable_type(data[[variable]])

    if (!type %in% c("nominal", "ordinal", "logical")) {
      cli::cli_abort(c(
        "Grouped descriptions require categorical grouping variables.",
        "i" = "Grouping variable `{variable}` is `{type}`.",
        "i" = "Use factors, ordered factors, or logical variables for grouped summaries.",
        "i" = "Continuous right-hand side variables are planned for bivariate summaries."
      ))
    }
  }

  invisible(groups)
}

formula_terms_are_grouping_variables <- function(data, rhs_terms) {
  variables <- unique(unlist(rhs_terms$groups, use.names = FALSE))

  if (length(variables) == 0) {
    return(FALSE)
  }

  if (any(!variables %in% names(data))) {
    return(FALSE)
  }

  types <- vapply(variables, function(variable) read_variable_type(data[[variable]]), character(1))
  all(types %in% c("nominal", "ordinal", "logical"))
}

validate_rhs_variables <- function(data, rhs_terms) {
  variables <- unique(unlist(rhs_terms$groups, use.names = FALSE))

  for (variable in variables) {
    if (!variable %in% names(data)) {
      cli::cli_abort("Right-hand side variable `{variable}` was not found in `data`.")
    }
  }

  invisible(variables)
}

formula_random_effect_terms <- function(formula) {
  collect_random_terms(rlang::f_rhs(formula))
}

random_effect_rhs_terms <- function(formula, random_terms) {
  fixed_terms <- fixed_formula_terms(formula)$terms

  if (length(fixed_terms) > 0) {
    cli::cli_abort(c(
      "`describe_data()` currently supports random-effect formulas only when the right-hand side contains random intercept terms.",
      "i" = "Use a formula such as `x ~ (1 | g)`.",
      "i" = "Fixed effects together with random terms are planned for the modeling layer."
    ))
  }

  groups <- list()

  for (term in random_terms) {
    if (!identical(term$effects, "1")) {
      cli::cli_abort(c(
        "`describe_data()` currently supports random intercept terms only.",
        "i" = "Use `(1 | g)` rather than random-slope terms such as `(1 + x | g)`."
      ))
    }

    term_label <- paste0("(1 | ", term$group, ")")
    groups[[term_label]] <- split_interaction_term(term$group)
  }

  list(
    main = character(),
    interactions = character(),
    all = names(groups),
    groups = groups,
    random = random_terms
  )
}

#' Summarise a continuous variable
#'
#' `summarise_continuous()` returns a compact one-row summary table for a
#' numeric vector. Missingness is always reported, while summary statistics are
#' computed from observed values.
#'
#' @param x Numeric vector.
#'
#' @return A tibble with summary statistics.
#' @export
#'
#' @examples
#' x <- c(41, 53, 49, 52, NA, 47)
#' summarise_continuous(x)
#' inline_hist(x)
#' inline_boxplot(x)
summarise_continuous <- function(x) {
  if (!is.numeric(x)) {
    cli::cli_abort("`x` must be numeric.")
  }

  n <- length(x)
  n_missing <- sum(is.na(x))
  observed <- x[!is.na(x)]
  n_observed <- length(observed)
  x_mean <- if (n_observed > 0) mean(observed) else NA_real_
  x_sd <- if (n_observed > 1) stats::sd(observed) else NA_real_
  x_se <- if (n_observed > 1) x_sd / sqrt(n_observed) else NA_real_
  ci <- normal_ci(x_mean, x_se)
  box_plot <- inline_boxplot(x)
  outliers <- tukey_outliers(observed)
  outlier_summary <- summarise_outliers(outliers)

  tibble::tibble(
    n = n,
    n_missing = n_missing,
    pct_missing = if (n > 0) n_missing / n * 100 else NA_real_,
    mean = x_mean,
    median = if (n_observed > 0) stats::median(observed) else NA_real_,
    sd = x_sd,
    se = x_se,
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    iqr = if (n_observed > 0) stats::IQR(observed) else NA_real_,
    min = if (n_observed > 0) min(observed) else NA_real_,
    max = if (n_observed > 0) max(observed) else NA_real_,
    p25 = if (n_observed > 0) unname(stats::quantile(observed, 0.25)) else NA_real_,
    p75 = if (n_observed > 0) unname(stats::quantile(observed, 0.75)) else NA_real_,
    skewness = moment_skewness(observed),
    kurtosis = moment_kurtosis(observed),
    hist = inline_hist(x),
    box = box_plot$line1,
    box_labels = box_plot$line2,
    n_outliers = length(outliers),
    outliers = list(outliers),
    outlier_summary = list(outlier_summary)
  )
}

summarise_variable <- function(x, type = read_variable_type(x)) {
  switch(
    type,
    continuous = summarise_continuous(x),
    nominal = summarise_categorical(x, type = type),
    ordinal = summarise_categorical(x, type = type),
    logical = summarise_categorical(x, type = type),
    datetime = summarise_datetime(x),
    summarise_unknown(x)
  )
}

summarise_categorical <- function(x, type) {
  n <- length(x)
  n_missing <- sum(is.na(x))
  observed <- x[!is.na(x)]
  values <- categorical_levels(x)

  counts <- vapply(values, function(value) sum(as.character(observed) == value), integer(1))
  pct <- if (length(observed) > 0) counts / length(observed) * 100 else rep(NA_real_, length(counts))
  mode_index <- if (length(counts) > 0 && sum(counts) > 0) which.max(counts) else NA_integer_

  tibble::tibble(
    type = type,
    n = n,
    n_missing = n_missing,
    pct_missing = if (n > 0) n_missing / n * 100 else NA_real_,
    n_levels = length(values),
    mode = if (!is.na(mode_index)) values[[mode_index]] else NA_character_,
    mode_n = if (!is.na(mode_index)) counts[[mode_index]] else NA_integer_,
    levels = list(tibble::tibble(
      level = values,
      n = counts,
      pct = pct
    ))
  )
}

categorical_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  }

  if (is.logical(x)) {
    return(c("FALSE", "TRUE"))
  }

  sort(unique(as.character(x[!is.na(x)])))
}

summarise_datetime <- function(x) {
  n <- length(x)
  n_missing <- sum(is.na(x))
  observed <- x[!is.na(x)]

  tibble::tibble(
    type = "datetime",
    n = n,
    n_missing = n_missing,
    pct_missing = if (n > 0) n_missing / n * 100 else NA_real_,
    min = if (length(observed) > 0) min(observed) else as.POSIXct(NA),
    max = if (length(observed) > 0) max(observed) else as.POSIXct(NA)
  )
}

summarise_unknown <- function(x) {
  n <- length(x)
  n_missing <- sum(is.na(x))

  tibble::tibble(
    type = "unknown",
    n = n,
    n_missing = n_missing,
    pct_missing = if (n > 0) n_missing / n * 100 else NA_real_
  )
}

normal_ci <- function(mean, se, level = 0.95) {
  if (is.na(mean) || is.na(se)) {
    return(c(NA_real_, NA_real_))
  }

  z <- stats::qnorm(1 - (1 - level) / 2)
  c(mean - z * se, mean + z * se)
}

moment_skewness <- function(x) {
  n <- length(x)

  if (n < 3) {
    return(NA_real_)
  }

  centered <- x - mean(x)
  m2 <- mean(centered^2)

  if (identical(m2, 0)) {
    return(NA_real_)
  }

  mean(centered^3) / m2^(3 / 2)
}

moment_kurtosis <- function(x) {
  n <- length(x)

  if (n < 4) {
    return(NA_real_)
  }

  centered <- x - mean(x)
  m2 <- mean(centered^2)

  if (identical(m2, 0)) {
    return(NA_real_)
  }

  mean(centered^4) / m2^2
}

tukey_outliers <- function(x) {
  if (length(x) == 0) {
    return(numeric())
  }

  grDevices::boxplot.stats(x)$out
}

summarise_outliers <- function(x) {
  if (length(x) == 0) {
    return(tibble::tibble(value = numeric(), n = integer(), pct = numeric()))
  }

  values <- sort(unique(x))
  counts <- vapply(values, function(value) sum(x == value), integer(1))

  tibble::tibble(
    value = values,
    n = counts,
    pct = counts / length(x) * 100
  )
}

summarise_grouped_continuous <- function(data, variable, groups) {
  rows <- list()
  row_index <- 0L

  for (term in names(groups)) {
    group_vars <- groups[[term]]
    group_grid <- grouped_term_grid(data, group_vars)

    for (group_row in seq_len(nrow(group_grid))) {
      row_index <- row_index + 1L
      values <- as.list(group_grid[group_row, , drop = FALSE])
      in_group <- rows_in_group(data, group_vars, values)

      rows[[row_index]] <- cbind(
        tibble::tibble(
          term = term,
          group = format_group_label(values, group_vars)
        ),
        summarise_continuous(data[[variable]][in_group])
      )
    }
  }

  if (length(rows) == 0) {
    return(empty_grouped_summary())
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

grouped_term_grid <- function(data, group_vars) {
  values <- lapply(group_vars, function(group_var) group_levels(data[[group_var]]))
  names(values) <- group_vars

  grid <- do.call(
    expand.grid,
    c(values, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  )

  tibble::as_tibble(grid)
}

group_levels <- function(x) {
  values <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))

  if (any(is.na(x))) {
    values <- c(values, "(missing)")
  }

  values
}

rows_in_group <- function(data, group_vars, values) {
  in_group <- rep(TRUE, nrow(data))

  for (group_var in group_vars) {
    observed <- format_group_value(data[[group_var]])
    in_group <- in_group & observed == values[[group_var]]
  }

  in_group
}

empty_grouped_summary <- function() {
  out <- cbind(
    tibble::tibble(term = character(), group = character()),
    summarise_continuous(numeric())[0, ]
  )

  tibble::as_tibble(out)
}

format_group_value <- function(x) {
  value <- as.character(x)
  value[is.na(value)] <- "(missing)"
  value
}

format_group_label <- function(values, group_vars) {
  values <- unlist(values, use.names = FALSE)

  if (length(group_vars) == 1) {
    return(values[[1]])
  }

  paste0(group_vars, " = ", values, collapse = ", ")
}

summarise_relationship_description <- function(data, outcome, outcome_type, formula, rhs_terms) {
  rhs_variables <- validate_rhs_variables(data, rhs_terms)
  variable_summaries <- summarise_formula_variables(data, outcome, rhs_variables)
  relationships <- lapply(
    rhs_terms$all,
    summarise_relationship_term,
    data = data,
    outcome = outcome,
    outcome_type = outcome_type,
    rhs_terms = rhs_terms
  )
  relationships <- tibble::as_tibble(do.call(rbind, relationships))
  notes <- relationship_notes(relationships)

  list(
    outcome_summary = summarise_variable(data[[outcome]], outcome_type),
    variable_summaries = variable_summaries,
    relationships = relationships,
    notes = notes
  )
}

summarise_formula_variables <- function(data, outcome, rhs_variables) {
  variables <- unique(c(outcome, rhs_variables))
  roles <- ifelse(variables == outcome, "outcome", "predictor")

  rows <- lapply(seq_along(variables), function(i) {
    variable <- variables[[i]]
    type <- read_variable_type(data[[variable]])
    summary <- summarise_variable(data[[variable]], type)

    tibble::tibble(
      variable = variable,
      role = roles[[i]],
      type = type,
      n = summary$n,
      n_missing = summary$n_missing,
      pct_missing = summary$pct_missing
    )
  })

  tibble::as_tibble(do.call(rbind, rows))
}

summarise_relationship_term <- function(term, data, outcome, outcome_type, rhs_terms) {
  predictors <- rhs_terms$groups[[term]]
  predictor_types <- vapply(predictors, function(predictor) read_variable_type(data[[predictor]]), character(1))

  if (length(predictors) > 1) {
    return(summarise_interaction_relationship(data, outcome, outcome_type, term, predictors, predictor_types))
  }

  predictor <- predictors[[1]]
  predictor_type <- predictor_types[[1]]

  if (identical(outcome_type, "continuous") && identical(predictor_type, "continuous")) {
    return(summarise_continuous_continuous_relationship(data, outcome, predictor, term))
  }

  if (identical(outcome_type, "continuous") && predictor_type %in% c("nominal", "ordinal", "logical")) {
    return(summarise_continuous_group_relationship(data, outcome, predictor, predictor_type, term))
  }

  if (outcome_type %in% c("nominal", "ordinal", "logical") && identical(predictor_type, "continuous")) {
    return(summarise_continuous_by_outcome_relationship(data, outcome, outcome_type, predictor, term))
  }

  if (outcome_type %in% c("nominal", "ordinal", "logical") && predictor_type %in% c("nominal", "ordinal", "logical")) {
    return(summarise_categorical_categorical_relationship(data, outcome, outcome_type, predictor, predictor_type, term))
  }

  make_relationship_row(
    term = term,
    predictors = predictor,
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    relationship = "unsupported",
    n = length(data[[outcome]]),
    note = "This variable-type combination is recognized but not summarized yet."
  )
}

summarise_continuous_continuous_relationship <- function(data, outcome, predictor, term) {
  y <- data[[outcome]]
  x <- data[[predictor]]
  keep <- !is.na(y) & !is.na(x)
  n <- sum(keep)
  x_obs <- x[keep]
  y_obs <- y[keep]
  has_variation <- n > 1 && stats::sd(x_obs) > 0 && stats::sd(y_obs) > 0

  correlation <- if (has_variation) stats::cor(x_obs, y_obs) else NA_real_
  slope <- NA_real_
  ci <- c(NA_real_, NA_real_)

  if (has_variation) {
    x_centered <- x_obs - mean(x_obs)
    y_centered <- y_obs - mean(y_obs)
    ss_x <- sum(x_centered^2)
    slope <- sum(x_centered * y_centered) / ss_x
    intercept <- mean(y_obs) - slope * mean(x_obs)
    residuals <- y_obs - (intercept + slope * x_obs)
    se <- if (n > 2) sqrt(sum(residuals^2) / (n - 2) / ss_x) else NA_real_
    ci <- normal_ci(slope, se)
  }

  make_relationship_row(
    term = term,
    predictors = predictor,
    outcome_type = "continuous",
    predictor_type = "continuous",
    relationship = "continuous-continuous",
    n = n,
    estimate = slope,
    statistic = correlation,
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    detail = list(scatter = if (n > 0) inline_scatter(x_obs, y_obs) else NULL),
    note = relationship_strength_note(correlation)
  )
}

summarise_continuous_group_relationship <- function(data, outcome, predictor, predictor_type, term) {
  groups <- stats::setNames(list(predictor), predictor)
  grouped <- summarise_grouped_continuous(data, outcome, groups)
  estimate <- NA_real_
  ci <- c(NA_real_, NA_real_)

  if (nrow(grouped) == 2) {
    estimate <- grouped$mean[[2]] - grouped$mean[[1]]
    se <- if (any(is.na(grouped$se))) NA_real_ else sqrt(sum(grouped$se^2))
    ci <- normal_ci(estimate, se)
  }

  make_relationship_row(
    term = term,
    predictors = predictor,
    outcome_type = "continuous",
    predictor_type = predictor_type,
    relationship = "continuous-group",
    n = sum(grouped$n),
    estimate = estimate,
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    detail = list(groups = grouped),
    note = if (nrow(grouped) == 2) "Mean difference is second level minus first level." else "Group means are summarized by level."
  )
}

summarise_continuous_by_outcome_relationship <- function(data, outcome, outcome_type, predictor, term) {
  groups <- stats::setNames(list(outcome), outcome)
  grouped <- summarise_grouped_continuous(data, predictor, groups)

  make_relationship_row(
    term = term,
    predictors = predictor,
    outcome_type = outcome_type,
    predictor_type = "continuous",
    relationship = "continuous-by-outcome",
    n = sum(grouped$n),
    detail = list(groups = grouped),
    note = "Continuous predictor summarized within outcome levels."
  )
}

summarise_categorical_categorical_relationship <- function(data, outcome, outcome_type, predictor, predictor_type, term) {
  outcome_values <- format_group_value(data[[outcome]])
  predictor_values <- format_group_value(data[[predictor]])
  counts <- as.data.frame(table(outcome = outcome_values, predictor = predictor_values), stringsAsFactors = FALSE)
  names(counts) <- c("outcome", "predictor", "n")
  counts$pct <- counts$n / sum(counts$n) * 100

  make_relationship_row(
    term = term,
    predictors = predictor,
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    relationship = "contingency",
    n = sum(counts$n),
    detail = list(counts = tibble::as_tibble(counts)),
    note = "Categorical relationship summarized as a contingency table."
  )
}

summarise_interaction_relationship <- function(data, outcome, outcome_type, term, predictors, predictor_types) {
  continuous_predictors <- predictors[predictor_types == "continuous"]
  categorical_predictors <- predictors[predictor_types %in% c("nominal", "ordinal", "logical")]

  if (identical(outcome_type, "continuous") && length(continuous_predictors) == 1 && length(categorical_predictors) == 1) {
    return(summarise_continuous_by_group_slopes(
      data = data,
      outcome = outcome,
      continuous_predictor = continuous_predictors[[1]],
      group_predictor = categorical_predictors[[1]],
      term = term
    ))
  }

  make_relationship_row(
    term = term,
    predictors = paste(predictors, collapse = ":"),
    outcome_type = outcome_type,
    predictor_type = paste(predictor_types, collapse = ":"),
    relationship = "interaction",
    n = length(data[[outcome]]),
    note = "Interaction recognized; detailed conditional summaries are planned for this variable-type combination."
  )
}

summarise_continuous_by_group_slopes <- function(data, outcome, continuous_predictor, group_predictor, term) {
  levels <- group_levels(data[[group_predictor]])
  rows <- lapply(levels, function(level) {
    in_group <- format_group_value(data[[group_predictor]]) == level
    relationship <- summarise_continuous_continuous_relationship(
      data = data[in_group, , drop = FALSE],
      outcome = outcome,
      predictor = continuous_predictor,
      term = term
    )

    tibble::tibble(
      group = level,
      n = relationship$n,
      correlation = relationship$statistic,
      slope = relationship$estimate,
      ci_low = relationship$ci_low,
      ci_high = relationship$ci_high
    )
  })
  slopes <- tibble::as_tibble(do.call(rbind, rows))

  make_relationship_row(
    term = term,
    predictors = paste(c(continuous_predictor, group_predictor), collapse = ":"),
    outcome_type = "continuous",
    predictor_type = "continuous:categorical",
    relationship = "continuous-by-group interaction",
    n = sum(slopes$n),
    detail = list(slopes = slopes),
    note = "Interaction summarized as continuous slopes within grouping levels."
  )
}

make_relationship_row <- function(term,
                                  predictors,
                                  outcome_type,
                                  predictor_type,
                                  relationship,
                                  n,
                                  estimate = NA_real_,
                                  statistic = NA_real_,
                                  ci_low = NA_real_,
                                  ci_high = NA_real_,
                                  detail = list(),
                                  note = NA_character_) {
  tibble::tibble(
    term = term,
    predictors = paste(predictors, collapse = ", "),
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    relationship = relationship,
    n = n,
    estimate = estimate,
    statistic = statistic,
    ci_low = ci_low,
    ci_high = ci_high,
    detail = list(detail),
    note = note
  )
}

relationship_strength_note <- function(correlation) {
  if (is.na(correlation)) {
    return("Correlation could not be computed.")
  }

  strength <- cut(
    abs(correlation),
    breaks = c(-Inf, 0.1, 0.3, 0.5, Inf),
    labels = c("negligible", "weak", "moderate", "strong")
  )
  direction <- if (correlation >= 0) "positive" else "negative"
  paste0(strength, " ", direction, " relationship")
}

relationship_notes <- function(relationships) {
  notes <- unique(stats::na.omit(relationships$note))
  unname(notes)
}

new_birtir_description <- function(formula,
                                    variable,
                                    type,
                                    summary,
                                    overall_summary = NULL,
                                    variable_summaries = NULL,
                                    relationships = NULL,
                                    notes = character(),
                                    labels = description_labels(),
                                    groups = NULL,
                                    rhs_terms = NULL,
                                    random_effects = NULL,
                                    show_random_effects = TRUE,
                                    max_random_effects = 8,
                                    subclass = NULL) {
  validate_description_labels(labels)

  structure(
    list(
      formula = formula,
      variable = variable,
      type = type,
      summary = summary,
      overall_summary = overall_summary,
      variable_summaries = variable_summaries,
      relationships = relationships,
      notes = notes,
      labels = labels,
      groups = groups,
      rhs_terms = rhs_terms,
      random_effects = random_effects,
      show_random_effects = show_random_effects,
      max_random_effects = max_random_effects
    ),
    class = unique(c(subclass, "birtir_description"))
  )
}

#' @export
print.birtir_description <- function(x, ...) {
  print_variable_summary(
    summary = x$summary,
    variable = x$variable,
    type = x$type,
    notes = x$notes
  )

  invisible(x)
}

print_variable_summary <- function(summary, variable, type, notes = character()) {
  switch(
    type,
    continuous = print_continuous_summary(
      summary = summary,
      title = paste0(variable, " (", title_case(type), ")"),
      notes = notes
    ),
    nominal = print_categorical_summary(summary, paste0(variable, " (Nominal)"), notes),
    ordinal = print_categorical_summary(summary, paste0(variable, " (Ordinal)"), notes),
    logical = print_categorical_summary(summary, paste0(variable, " (Logical)"), notes),
    datetime = print_datetime_summary(summary, paste0(variable, " (Datetime)"), notes),
    print_unknown_summary(summary, paste0(variable, " (Unknown)"), notes)
  )
}

print_categorical_summary <- function(summary, title, notes = character()) {
  cat(print_rule(title), "\n\n", sep = "")
  cat(
    "n = ", summary$n,
    "  missing = ", summary$n_missing,
    " (", format_number(summary$pct_missing, 1), "%)",
    "  levels = ", summary$n_levels,
    "\n\n",
    sep = ""
  )

  levels <- summary$levels[[1]]
  values <- stats::setNames(levels$n, levels$level)
  print(inline_bar(values, width = 16, digits = 0))

  if (!is.na(summary$mode)) {
    cat("\nMode = ", summary$mode, " (n = ", summary$mode_n, ")\n", sep = "")
  }

  if (length(notes) > 0) {
    cat("\nNotes: ", paste(notes, collapse = "; "), "\n", sep = "")
  }

  invisible(summary)
}

print_datetime_summary <- function(summary, title, notes = character()) {
  cat(print_rule(title), "\n\n", sep = "")
  cat(
    "n = ", summary$n,
    "  missing = ", summary$n_missing,
    " (", format_number(summary$pct_missing, 1), "%)\n\n",
    sep = ""
  )
  cat("Range = ", as.character(summary$min), " to ", as.character(summary$max), "\n", sep = "")

  if (length(notes) > 0) {
    cat("\nNotes: ", paste(notes, collapse = "; "), "\n", sep = "")
  }

  invisible(summary)
}

print_unknown_summary <- function(summary, title, notes = character()) {
  cat(print_rule(title), "\n\n", sep = "")
  cat(
    "n = ", summary$n,
    "  missing = ", summary$n_missing,
    " (", format_number(summary$pct_missing, 1), "%)\n",
    sep = ""
  )

  if (length(notes) > 0) {
    cat("\nNotes: ", paste(notes, collapse = "; "), "\n", sep = "")
  }

  invisible(summary)
}

#' @export
print.birtir_relationship <- function(x, ...) {
  print_variable_summary(
    summary = x$summary,
    variable = x$variable,
    type = x$type
  )

  cat("\n", print_rule("Relationships"), "\n\n", sep = "")

  for (row in seq_len(nrow(x$relationships))) {
    print_relationship_row(x$relationships[row, , drop = FALSE])

    if (row < nrow(x$relationships)) {
      cat("\n")
    }
  }

  invisible(x)
}

print_relationship_row <- function(row) {
  relationship <- row$relationship[[1]]
  term <- row$term[[1]]

  cat(print_rule(paste0("~ ", term)), "\n\n", sep = "")

  if (identical(relationship, "continuous-continuous")) {
    cat(
      "n = ", row$n,
      "  r = ", format_number(row$statistic),
      "  slope = ", format_number(row$estimate),
      " [", format_number(row$ci_low), ", ", format_number(row$ci_high), "]\n",
      sep = ""
    )
    detail <- row$detail[[1]]
    if (!is.null(detail$scatter)) {
      print(detail$scatter)
    }
  } else if (identical(relationship, "continuous-group") || identical(relationship, "continuous-by-outcome")) {
    detail <- row$detail[[1]]
    groups <- detail$groups
    print(inline_forest(
      data.frame(
        term = groups$group,
        estimate = groups$mean,
        ci_low = groups$ci_low,
        ci_high = groups$ci_high,
        n = groups$n,
        stringsAsFactors = FALSE
      ),
      title = "Group means",
      show_stars = FALSE
    ))
  } else if (identical(relationship, "contingency")) {
    detail <- row$detail[[1]]
    print(detail$counts)
  } else if (identical(relationship, "continuous-by-group interaction")) {
    detail <- row$detail[[1]]
    print(inline_forest(
      data.frame(
        term = detail$slopes$group,
        estimate = detail$slopes$slope,
        ci_low = detail$slopes$ci_low,
        ci_high = detail$slopes$ci_high,
        n = detail$slopes$n,
        stringsAsFactors = FALSE
      ),
      title = "Conditional slopes",
      show_stars = FALSE
    ))
  }

  if (!is.na(row$note[[1]])) {
    cat("\n", row$note[[1]], "\n", sep = "")
  }

  invisible(row)
}

print_continuous_summary <- function(summary, title, notes = character()) {
  cat(print_rule(title), "\n\n", sep = "")
  cat(
    "n = ", summary$n,
    "  missing = ", summary$n_missing,
    " (", format_number(summary$pct_missing, 1), "%)\n\n",
    sep = ""
  )
  cat(
    "Mean = ", format_number(summary$mean, 1),
    "   SD = ", format_number(summary$sd, 1),
    "   95% CI [", format_number(summary$ci_low), ", ", format_number(summary$ci_high), "]",
    "   SE = ", format_number(summary$se),
    "\n",
    sep = ""
  )
  cat(summary$box, "\n", summary$box_labels, "\n\n", sep = "")
  cat(
    summary$hist,
    "   Skewness = ", format_number(summary$skewness),
    "  Kurtosis = ", format_number(summary$kurtosis),
    "\n",
    sep = ""
  )

  if (summary$n_outliers > 0) {
    cat("\n")
    cat(
      "\u26a0 Potential outliers -- ",
      summary$n_outliers,
      " detected (1.5\u00d7IQR)\n",
      sep = ""
    )
    cat("values: ", format_outlier_summary(summary$outlier_summary[[1]]), "\n", sep = "")
    print(outlier_bar(summary$outlier_summary[[1]]))
  }

  if (length(notes) > 0) {
    cat("\nNotes: ", paste(notes, collapse = "; "), "\n", sep = "")
  }

  invisible(summary)
}

print_rule <- function(title, width = 58) {
  prefix <- paste0("\u2500\u2500 ", title, " ")
  paste0(prefix, paste(rep("\u2500", max(width - nchar(prefix), 1L)), collapse = ""))
}

title_case <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

format_outlier_summary <- function(x) {
  if (nrow(x) == 0) {
    return("[]")
  }

  digits <- outlier_value_digits(x$value)
  values <- paste0(format_number(x$value, digits), " (n = ", x$n, ")")
  paste0("[", paste(values, collapse = ", "), "]")
}

outlier_bar <- function(x, width = 16) {
  values <- stats::setNames(x$n, format_number(x$value, outlier_value_digits(x$value)))
  inline_bar(values, width = width, digits = 0)
}

outlier_value_digits <- function(x) {
  if (all(is.na(x))) {
    return(2)
  }

  if (all(abs(x - round(x)) < sqrt(.Machine$double.eps), na.rm = TRUE)) {
    return(0)
  }

  2
}

#' @export
print.birtir_random_effects <- function(x,
                                         ...,
                                         show_random_effects = x$show_random_effects,
                                         max_random_effects = x$max_random_effects) {
  summary <- x$summary

  print_continuous_summary(
    summary = x$overall_summary,
    title = paste0("Total: ", x$variable, " (", title_case(x$type), ")"),
    notes = x$notes
  )

  if (nrow(summary) == 0) {
    cat("\nNo random-effect groups to summarize.\n")
    return(invisible(x))
  }

  cat("\n", print_rule("Random effects"), "\n\n", sep = "")

  if (!isTRUE(show_random_effects)) {
    for (term in unique(summary$term)) {
      rows <- summary[summary$term == term, , drop = FALSE]
      cat(term, ": ", nrow(rows), " levels\n", sep = "")
    }

    return(invisible(x))
  }

  terms <- unique(summary$term)
  max_random_effects <- as.integer(max_random_effects)

  for (term_index in seq_along(terms)) {
    term <- terms[[term_index]]
    rows <- summary[summary$term == term, , drop = FALSE]
    n_show <- min(nrow(rows), max_random_effects)
    visible_rows <- rows[seq_len(n_show), , drop = FALSE]

    forest <- inline_forest(
      data.frame(
        term = c("Grand mean", visible_rows$group),
        estimate = c(x$overall_summary$mean, visible_rows$mean),
        ci_low = c(x$overall_summary$ci_low, visible_rows$ci_low),
        ci_high = c(x$overall_summary$ci_high, visible_rows$ci_high),
        n = c(x$overall_summary$n, visible_rows$n),
        stringsAsFactors = FALSE
      ),
      title = term,
      show_stars = FALSE
    )

    print(forest)

    if (nrow(rows) > n_show) {
      cat("... ", nrow(rows) - n_show, " more levels\n", sep = "")
    }

    if (term_index < length(terms)) {
      cat("\n")
    }
  }

  invisible(x)
}

#' @export
print.birtir_grouped <- function(x, ...) {
  summary <- x$summary

  print_continuous_summary(
    summary = x$overall_summary,
    title = paste0("Total: ", x$variable, " (", title_case(x$type), ")"),
    notes = x$notes
  )

  if (nrow(summary) == 0) {
    cat("\nNo groups to summarize.\n")
    return(invisible(x))
  }

  for (term in unique(summary$term)) {
    rows <- summary[summary$term == term, , drop = FALSE]
    n_show <- min(nrow(rows), 8L)

    cat("\n", print_rule(paste0(x$variable, " ~ ", term)), "\n\n", sep = "")

    for (row in seq_len(n_show)) {
      print_continuous_summary(
        summary = rows[row, , drop = FALSE],
        title = paste0(term, ": ", rows$group[[row]])
      )

      if (row < n_show) {
        cat("\n")
      }
    }

    if (nrow(rows) > n_show) {
      cat("\n... ", nrow(rows) - n_show, " more groups\n", sep = "")
    }
  }

  invisible(x)
}
