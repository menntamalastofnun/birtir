# Formula helpers ---------------------------------------------------------

is_ungrouped_formula <- function(formula) {
  validate_formula(formula)
  rhs <- rlang::f_rhs(formula)
  identical(rhs, 1) || identical(rhs, 1L)
}

formula_lhs_name <- function(formula) {
  validate_formula(formula)

  lhs <- rlang::f_lhs(formula)

  if (is.null(lhs)) {
    cli::cli_abort("`formula` must have a left-hand side, such as `x ~ 1`.")
  }

  if (!rlang::is_symbol(lhs)) {
    cli::cli_abort("The left-hand side of `formula` must be a single variable name.")
  }

  rlang::as_string(lhs)
}

formula_rhs_terms <- function(formula) {
  validate_formula(formula)
  labels <- attr(stats::terms(formula), "term.labels")
  interactions <- grepl(":", labels, fixed = TRUE)

  groups <- stats::setNames(
    lapply(labels, split_interaction_term),
    labels
  )

  list(
    main = labels[!interactions],
    interactions = labels[interactions],
    all = labels,
    groups = groups
  )
}

split_interaction_term <- function(term) {
  strsplit(term, ":", fixed = TRUE)[[1]]
}

validate_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    cli::cli_abort("`formula` must be an R formula, such as `x ~ 1`.")
  }

  invisible(formula)
}

formula_text <- function(formula) {
  paste(deparse(formula), collapse = "")
}

#' Convert a formula to a LaTeX equation
#'
#' `formula_to_latex()` turns an R formula into a compact LaTeX equation for
#' reports, notes, and model-planning output.
#'
#' @param formula An R formula.
#' @param subscript Logical; if `TRUE`, add `_i` to the outcome and terms.
#' @param error_term Logical; if `TRUE`, add an error term to the equation.
#' @param notation Equation notation. `"expanded"` gives a readable
#'   observation-level equation. `"matrix"` gives compact model-matrix notation.
#' @param family Model family. Currently used to decide whether to print a
#'   Gaussian residual-error equation or a link-function equation.
#' @param link Optional link-function name. If `NULL`, a common default is used
#'   for known families.
#'
#' @return A character string containing a LaTeX equation.
#'
#' @details
#' Mixed-effect formulas follow lme4/brms-style syntax such as `(1 | group)`,
#' `(1 + x | group)`, and `(x || group)`. In lme4 terminology these are
#' random-effect terms; in brms terminology they are group-level or varying
#' effects. The expanded equation prints the linear predictor. Correlation
#' structure, including the difference between `|` and `||`, belongs to the
#' random-effect covariance model and is represented in matrix notation by
#' `B \sim N(0, \Sigma)`.
#' @export
#'
#' @examples
#' formula_to_latex(x ~ 1)
#' formula_to_latex(y ~ x + g)
#' formula_to_latex(y ~ x * g)
#' formula_to_latex(y ~ x + (1 | group))
#' formula_to_latex(y ~ x + (1 + x | group))
#' formula_to_latex(y ~ x + (x || group))
#' formula_to_latex(y ~ x + (1 | subject) + (1 | item))
#' formula_to_latex(y ~ x + (1 | group), notation = "matrix")
#' formula_to_latex(y ~ x + (1 | group), family = "binomial")
formula_to_latex <- function(formula,
                             subscript = TRUE,
                             error_term = TRUE,
                             notation = c("expanded", "matrix"),
                             family = "gaussian",
                             link = NULL) {
  validate_formula(formula)
  notation <- match.arg(notation)
  validate_character_scalar(family, "family")

  if (!is.logical(subscript) || length(subscript) != 1 || is.na(subscript)) {
    cli::cli_abort("`subscript` must be `TRUE` or `FALSE`.")
  }

  if (!is.logical(error_term) || length(error_term) != 1 || is.na(error_term)) {
    cli::cli_abort("`error_term` must be `TRUE` or `FALSE`.")
  }

  if (!is.null(link)) {
    validate_character_scalar(link, "link")
  }

  parsed <- parse_model_formula(formula)
  link <- link %||% default_link(family)

  if (identical(notation, "matrix")) {
    return(formula_to_matrix_latex(parsed, family = family, link = link))
  }

  lhs <- formula_lhs_name(formula)
  sub_i <- if (isTRUE(subscript)) "_i" else ""
  rhs <- formula_latex_rhs(parsed, sub_i)

  if (uses_link_equation(family, link)) {
    return(paste0(
      "$",
      latex_link(link, paste0("\\mu", sub_i)),
      " = \\eta", sub_i,
      " = ",
      rhs,
      "$"
    ))
  }

  error <- if (isTRUE(error_term)) paste0(" + \\epsilon", sub_i) else ""
  paste0("$", lhs, sub_i, " = ", rhs, error, "$")
}

formula_latex_rhs <- function(parsed, sub_i) {
  terms <- character()

  if (isTRUE(parsed$intercept)) {
    terms <- c(terms, "\\beta_0")
  }

  if (length(parsed$fixed_terms) > 0) {
    fixed_terms <- vapply(parsed$fixed_terms, formula_term_to_latex, character(1), sub_i = sub_i)
    terms <- c(terms, fixed_terms)
  }

  if (length(parsed$random_terms) > 0) {
    random_terms <- unlist(
      lapply(parsed$random_terms, random_term_to_latex, sub_i = sub_i),
      use.names = FALSE
    )
    terms <- c(terms, random_terms)
  }

  if (length(terms) == 0) {
    return("0")
  }

  paste(terms, collapse = " + ")
}

formula_term_to_latex <- function(term, sub_i) {
  pieces <- split_interaction_term(term)
  term_text <- paste0(pieces, sub_i, collapse = " \\times ")
  paste0("\\beta_{", latex_escape_term(term), "} ", term_text)
}

latex_escape_term <- function(term) {
  gsub(":", "\\times ", term, fixed = TRUE)
}

formula_to_matrix_latex <- function(parsed, family, link) {
  has_random <- length(parsed$random_terms) > 0

  if (uses_link_equation(family, link)) {
    linear <- if (has_random) "X\\beta + Zb" else "X\\beta"
    random <- if (has_random) ";\\ B \\sim N(0, \\Sigma)" else ""
    return(paste0("$", latex_link(link, "\\mu"), " = ", linear, random, "$"))
  }

  if (has_random) {
    return("$Y \\mid B = b \\sim N(X\\beta + Zb, \\sigma^2 W^{-1});\\ B \\sim N(0, \\Sigma)$")
  }

  "$Y \\sim N(X\\beta, \\sigma^2 W^{-1})$"
}

parse_model_formula <- function(formula) {
  lhs <- formula_lhs_name(formula)
  rhs <- rlang::f_rhs(formula)
  random_terms <- collect_random_terms(rhs)
  fixed_terms <- fixed_formula_terms(formula)

  list(
    lhs = lhs,
    fixed_terms = fixed_terms$terms,
    intercept = fixed_terms$intercept,
    random_terms = random_terms
  )
}

fixed_formula_terms <- function(formula) {
  tt <- stats::terms(formula)
  labels <- attr(tt, "term.labels")
  random <- grepl("|", labels, fixed = TRUE)

  list(
    terms = labels[!random],
    intercept = identical(attr(tt, "intercept"), 1L)
  )
}

collect_random_terms <- function(expr) {
  expr <- unwrap_parens(expr)

  if (is_call_name(expr, "+")) {
    return(c(
      collect_random_terms(expr[[2]]),
      collect_random_terms(expr[[3]])
    ))
  }

  if (is_call_name(expr, "-")) {
    return(collect_random_terms(expr[[2]]))
  }

  if (is_random_effect_call(expr)) {
    return(parse_random_effect_call(expr))
  }

  list()
}

parse_random_effect_call <- function(expr) {
  operator <- as.character(expr[[1]])
  effects <- parse_random_effects(expr[[2]])
  groups <- expand_group_expression(expr[[3]])

  lapply(groups, function(group) {
    list(
      effects = effects,
      group = group,
      correlated = identical(operator, "|")
    )
  })
}

parse_random_effects <- function(expr) {
  expr_text <- deparse_expression(expr)
  tt <- stats::terms(stats::as.formula(paste("~", expr_text)))
  labels <- attr(tt, "term.labels")
  effects <- labels

  if (identical(attr(tt, "intercept"), 1L)) {
    effects <- c("1", effects)
  }

  if (length(effects) == 0) {
    effects <- "1"
  }

  effects
}

expand_group_expression <- function(expr) {
  expr <- unwrap_parens(expr)

  if (is_call_name(expr, "/")) {
    parts <- flatten_group_nesting(expr)
    return(vapply(seq_along(parts), function(i) paste(parts[seq_len(i)], collapse = ":"), character(1)))
  }

  deparse_expression(expr)
}

flatten_group_nesting <- function(expr) {
  expr <- unwrap_parens(expr)

  if (is_call_name(expr, "/")) {
    return(c(flatten_group_nesting(expr[[2]]), flatten_group_nesting(expr[[3]])))
  }

  deparse_expression(expr)
}

random_term_to_latex <- function(term, sub_i) {
  vapply(term$effects, random_effect_to_latex, character(1), group = term$group, sub_i = sub_i)
}

random_effect_to_latex <- function(effect, group, sub_i) {
  if (identical(effect, "1")) {
    return(paste0("b_{0,", group, "[i]}"))
  }

  term_text <- paste0(split_interaction_term(effect), sub_i, collapse = " \\times ")
  paste0("b_{", latex_escape_term(effect), ",", group, "[i]} ", term_text)
}

unwrap_parens <- function(expr) {
  while (is_call_name(expr, "(")) {
    expr <- expr[[2]]
  }

  expr
}

is_random_effect_call <- function(expr) {
  is_call_name(expr, "|") || is_call_name(expr, "||")
}

is_call_name <- function(expr, name) {
  is.call(expr) && identical(as.character(expr[[1]]), name)
}

deparse_expression <- function(expr) {
  paste(deparse(expr), collapse = "")
}

uses_link_equation <- function(family, link) {
  !identical(tolower(family), "gaussian") || !identical(tolower(link), "identity")
}

default_link <- function(family) {
  switch(
    tolower(family),
    gaussian = "identity",
    binomial = "logit",
    poisson = "log",
    gamma = "log",
    inverse.gaussian = "inverse",
    "identity"
  )
}

latex_link <- function(link, mu) {
  switch(
    tolower(link),
    identity = mu,
    logit = paste0("\\operatorname{logit}(", mu, ")"),
    log = paste0("\\log(", mu, ")"),
    inverse = paste0(mu, "^{-1}"),
    paste0("g(", mu, ")")
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
