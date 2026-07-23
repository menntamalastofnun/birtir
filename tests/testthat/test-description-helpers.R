test_that("ungrouped formulas are recognized", {
  expect_true(birtir:::is_ungrouped_formula(x ~ 1))
  expect_false(birtir:::is_ungrouped_formula(x ~ g))
})

test_that("formula lhs names are extracted", {
  expect_identical(birtir:::formula_lhs_name(x ~ 1), "x")
})

test_that("formula rhs terms are extracted", {
  additive <- birtir:::formula_rhs_terms(x ~ g + h)

  expect_identical(additive$main, c("g", "h"))
  expect_identical(additive$interactions, character())
  expect_identical(additive$all, c("g", "h"))
  expect_identical(additive$groups, list(g = "g", h = "h"))
})

test_that("formula rhs terms include interactions and groups", {
  crossed <- birtir:::formula_rhs_terms(x ~ g * h)
  interaction_only <- birtir:::formula_rhs_terms(x ~ g:h)

  expect_identical(crossed$main, c("g", "h"))
  expect_identical(crossed$interactions, "g:h")
  expect_identical(crossed$all, c("g", "h", "g:h"))
  expect_identical(crossed$groups, list(g = "g", h = "h", "g:h" = c("g", "h")))

  expect_identical(interaction_only$main, character())
  expect_identical(interaction_only$interactions, "g:h")
  expect_identical(interaction_only$all, "g:h")
  expect_identical(interaction_only$groups, list("g:h" = c("g", "h")))
})

test_that("formula_to_latex creates simple equations", {
  expect_identical(
    formula_to_latex(x ~ 1),
    "$x_i = \\beta_0 + \\epsilon_i$"
  )
  expect_identical(
    formula_to_latex(y ~ x + g),
    "$y_i = \\beta_0 + \\beta_{x} x_i + \\beta_{g} g_i + \\epsilon_i$"
  )
  expect_identical(
    formula_to_latex(y ~ x:g, subscript = FALSE, error_term = FALSE),
    "$y = \\beta_0 + \\beta_{x\\times g} x \\times g$"
  )
})

test_that("formula_to_latex creates mixed-effect expanded equations", {
  expect_identical(
    formula_to_latex(y ~ x + (1 | g)),
    "$y_i = \\beta_0 + \\beta_{x} x_i + b_{0,g[i]} + \\epsilon_i$"
  )
  expect_identical(
    formula_to_latex(y ~ x + (x | g)),
    "$y_i = \\beta_0 + \\beta_{x} x_i + b_{0,g[i]} + b_{x,g[i]} x_i + \\epsilon_i$"
  )
  expect_identical(
    formula_to_latex(y ~ x + (1 + x | g)),
    "$y_i = \\beta_0 + \\beta_{x} x_i + b_{0,g[i]} + b_{x,g[i]} x_i + \\epsilon_i$"
  )
  expect_identical(
    formula_to_latex(y ~ x + (x || g)),
    "$y_i = \\beta_0 + \\beta_{x} x_i + b_{0,g[i]} + b_{x,g[i]} x_i + \\epsilon_i$"
  )
})

test_that("formula_to_latex handles nested and crossed grouping factors", {
  expect_identical(
    formula_to_latex(y ~ x + (1 | g1/g2)),
    "$y_i = \\beta_0 + \\beta_{x} x_i + b_{0,g1[i]} + b_{0,g1:g2[i]} + \\epsilon_i$"
  )
  expect_identical(
    formula_to_latex(y ~ x + (1 | subject) + (1 | item)),
    "$y_i = \\beta_0 + \\beta_{x} x_i + b_{0,subject[i]} + b_{0,item[i]} + \\epsilon_i$"
  )
})

test_that("formula_to_latex creates matrix and link notation", {
  expect_identical(
    formula_to_latex(y ~ x + (1 | g), notation = "matrix"),
    "$Y \\mid B = b \\sim N(X\\beta + Zb, \\sigma^2 W^{-1});\\ B \\sim N(0, \\Sigma)$"
  )
  expect_identical(
    formula_to_latex(y ~ x + (1 | g), family = "binomial"),
    "$\\operatorname{logit}(\\mu_i) = \\eta_i = \\beta_0 + \\beta_{x} x_i + b_{0,g[i]}$"
  )
})

test_that("formula_to_latex validates options", {
  expect_error(
    formula_to_latex(y ~ x, subscript = NA),
    "`subscript` must be `TRUE` or `FALSE`"
  )
  expect_error(
    formula_to_latex(y ~ x, notation = "wide"),
    "'arg' should be one of"
  )
})

test_that("lavaan_to_latex formats CFA and SEM model equations", {
  single <- lavaan_to_latex("F1 =~ y1 + y2 + y3")
  expect_length(single, 3)
  expect_identical(single[[1]], "$y1_i = \\lambda_{y1} F1_i + \\epsilon_{y1,i}$")

  cfa <- lavaan_to_latex("Visual =~ x1 + x2 + x3\n Text =~ x4 + x5 + x6\n Visual ~~ Text")
  expect_length(cfa, 7)
  expect_true(any(grepl("Visual_i", cfa)))
  expect_true(any(grepl("Text_i", cfa)))
  expect_true(any(grepl("Cov", cfa, fixed = TRUE)))

  matrix_cfa <- lavaan_to_latex("F =~ y1 + y2", notation = "matrix")
  expect_identical(matrix_cfa, "$X = \\Lambda \\eta + \\epsilon$")

  routed <- formula_to_latex("Visual =~ x1 + x2")
  expect_length(routed, 2)
  expect_true(any(grepl("Visual_i", routed)))

  with_comments <- lavaan_to_latex("F =~ y1 + y2 # inline comment\n # header comment")
  expect_length(with_comments, 2)
  expect_identical(with_comments[[1]], "$y1_i = \\lambda_{y1} F_i + \\epsilon_{y1,i}$")
})


test_that("variable types are read from R classes", {
  expect_identical(birtir:::read_variable_type(c(1, 2, 3)), "continuous")
  expect_identical(birtir:::read_variable_type(1:3), "continuous")
  expect_identical(birtir:::read_variable_type(factor(c("a", "b"))), "nominal")
  expect_identical(birtir:::read_variable_type(c("a", "b")), "nominal")
  expect_identical(birtir:::read_variable_type(ordered(c("a", "b"))), "ordinal")
  expect_identical(birtir:::read_variable_type(c(TRUE, FALSE)), "logical")
})

test_that("fmt_num formats APA and plain numbers", {
  expect_identical(fmt_num(c(0.123, -0.5, 1), digits = 2), c("0.12", "-0.50", "1.00"))
  expect_identical(fmt_num(c(0.123, -0.5, 1), digits = 2, drop_leading_zero = TRUE), c(".12", "-.50", "1.00"))
  expect_identical(fmt_num(0.123, digits = 2, style = "plain"), "0.12")
  expect_identical(fmt_num(0.1, digits = 2, trailing_zeros = FALSE, drop_leading_zero = TRUE), ".1")
  expect_identical(fmt_num(0.123, digits = 2, decimal_mark = ","), "0,12")
  expect_identical(fmt_num(NA_real_), "NA")
})

test_that("fmt_num validates input clearly", {
  expect_error(
    fmt_num(factor(c("a", "b"))),
    "`x` must be numeric"
  )
  expect_error(
    fmt_num(1.23, digits = 1.5),
    "`digits` must be a single non-negative whole number"
  )
})

test_that("fmt_p formats p-values", {
  expect_identical(fmt_p(c(0.05, 0.0005, NA_real_)), c("= .050", "< .001", "NA"))
  expect_identical(fmt_p(0.05, decimal_mark = ","), "= ,050")
  expect_identical(fmt_p(2), "2")
})

test_that("inline_hist returns compact bars", {
  out <- birtir:::inline_hist(c(1, 2, 3, 4, 5), n_bins = 5)

  expect_type(out, "character")
  expect_equal(nchar(out), 5)
  expect_identical(birtir:::inline_hist(c(NA_real_, NA_real_)), "")
})

test_that("inline_hist validates whole-number bins", {
  expect_error(
    inline_hist(c(1, 2, 3), n_bins = 2.5),
    "`n_bins` must be a single whole number"
  )
})

test_that("inline_boxplot returns two text lines", {
  out <- birtir:::inline_boxplot(c(1, 2, 3, 4, 5), width = 20)

  expect_s3_class(out, "birtir_inline_boxplot")
  expect_named(out, c("line1", "line2"))
  expect_equal(nchar(out$line1), 20)
  expect_equal(nchar(out$line2), 20)

  printed <- capture.output(returned <- print(out))
  expect_identical(returned, out)
  expect_length(printed, 2)
})

test_that("inline_boxplot handles constant and missing vectors", {
  constant <- birtir:::inline_boxplot(c(5, 5, 5), width = 10)
  missing <- birtir:::inline_boxplot(c(NA_real_, NA_real_), width = 10)

  expect_s3_class(constant, "birtir_inline_boxplot")
  expect_equal(nchar(constant$line1), 10)

  expect_s3_class(missing, "birtir_inline_boxplot")
  expect_identical(missing$line1, "[no observed values]")
  expect_identical(missing$line2, "")
})

test_that("inline_boxplot shows Tukey outliers", {
  out <- inline_boxplot(c(1:10, 100), width = 30)

  expect_s3_class(out, "birtir_inline_boxplot")
  expect_true(grepl("*", out$line1, fixed = TRUE))
})

test_that("inline_boxplot keeps tight labels readable", {
  out <- inline_boxplot(c(25, 42, 45, 47, 48, 50, 52, 56, 60, 74), width = 30)

  expect_match(out$line2, "42 +45 +49 +56 +60")
})

test_that("inline_boxplot validates input clearly", {
  expect_error(
    inline_boxplot(factor(c("a", "b"))),
    "`x` must be numeric"
  )
  expect_error(
    inline_boxplot(c(1, 2, 3), width = 5.5),
    "`width` must be a single whole number"
  )
})

test_that("inline_bar returns and prints bar rows", {
  out <- inline_bar(c(g0 = 49.1, g1 = 51.4), width = 10)

  expect_s3_class(out, "birtir_inline_bar")
  expect_named(out, c("rows", "labels"))
  expect_identical(out$labels, c("g0", "g1"))
  expect_length(out$rows, 2)

  printed <- capture.output(returned <- print(out))
  expect_identical(returned, out)
  expect_length(printed, 2)
})

test_that("inline_bar can print integer counts", {
  out <- inline_bar(c(`1.00` = 120, `19.00` = 370), width = 10, digits = 0)

  expect_true(any(grepl("370", out$rows, fixed = TRUE)))
  expect_false(any(grepl("370.0", out$rows, fixed = TRUE)))
})

test_that("inline_bar uses magnitude for signed values", {
  out <- inline_bar(c(a = -10, b = 5), width = 10, digits = 0)

  expect_true(grepl("-10", out$rows[[1]], fixed = TRUE))
  expect_true(grepl("5", out$rows[[2]], fixed = TRUE))
  expect_true(nchar(gsub("[^█▓]", "", out$rows[[1]])) > nchar(gsub("[^█▓]", "", out$rows[[2]])))
})

test_that("inline_bar aligns labels with different lengths", {
  out <- inline_bar(c(control = 49.3, treatment = 49.3), width = 10)

  separators <- regexpr("\u2502", out$rows, fixed = TRUE)

  expect_identical(as.integer(separators), c(11L, 11L))
})

test_that("inline_bar handles all missing values", {
  out <- inline_bar(c(g0 = NA_real_, g1 = NA_real_))

  expect_s3_class(out, "birtir_inline_bar")
  expect_identical(out$rows, character())
  expect_identical(out$labels, character())
})

test_that("inline_bar validates named numeric input clearly", {
  expect_error(
    inline_bar(c(1, 2)),
    "`values` must be a named numeric vector"
  )
})

test_that("inline_forest returns and prints coefficient rows", {
  out <- inline_forest(
    data.frame(
      term = c("x1", "x2", "g"),
      estimate = c(0.31, 0.18, 0.94),
      ci_low = c(0.12, 0.05, 0.41),
      ci_high = c(0.50, 0.31, 1.47)
    ),
    width = 12,
    digits = 2
  )

  expect_s3_class(out, "birtir_inline_forest")
  expect_named(out, c("lines", "rows", "data", "title"))
  expect_true(any(grepl("Coefficients", out$lines, fixed = TRUE)))
  expect_true(any(grepl("estimate    [95% CI]", out$lines, fixed = TRUE)))
  expect_true(any(grepl("x1", out$rows, fixed = TRUE)))
  expect_true(any(grepl("0.31 [0.12, 0.50] *", out$rows, fixed = TRUE)))

  printed <- capture.output(returned <- print(out))
  expect_identical(returned, out)
  expect_length(printed, length(out$lines))
})

test_that("inline_forest accepts named vectors", {
  out <- inline_forest(
    c(x1 = 0.31, x2 = 0.18),
    ci_low = c(0.12, 0.05),
    ci_high = c(0.50, 0.31),
    width = 10
  )

  expect_s3_class(out, "birtir_inline_forest")
  expect_identical(out$data$label, c("x1", "x2"))
})

test_that("inline_forest can include row counts", {
  out <- inline_forest(
    data.frame(
      term = c("Grand mean", "female"),
      estimate = c(10.25, 10.40),
      ci_low = c(10.21, 10.34),
      ci_high = c(10.29, 10.46),
      n = c(100, 55)
    ),
    show_stars = FALSE
  )

  expect_true(any(grepl("n  estimate", out$lines, fixed = TRUE)))
  expect_true(any(grepl("100  10.25", out$rows, fixed = TRUE)))
  expect_true(any(grepl("55  10.40", out$rows, fixed = TRUE)))
})

test_that("inline_forest validates interval inputs clearly", {
  expect_error(
    inline_forest(c(x1 = 0.31), ci_low = c(0.12, 0.05), ci_high = 0.50),
    "`ci_low` must have the same length as `estimate`"
  )
  expect_error(
    inline_forest(c(x1 = 0.31), ci_low = 0.50, ci_high = 0.12),
    "`ci_low` must be less than or equal to `ci_high`"
  )
})

test_that("inline_scatter returns and prints a compact plot", {
  out <- inline_scatter(1:5, c(1, 2, 3, 2, 1), width = 12, height = 5)

  expect_s3_class(out, "birtir_inline_scatter")
  expect_named(out, c("lines", "x_axis", "x_labels", "mn_x", "mx_x", "mn_y", "mx_y"))
  expect_length(out$lines, 5)
  expect_true(nchar(out$x_axis) > 12)

  printed <- capture.output(returned <- print(out))
  expect_identical(returned, out)
  expect_length(printed, 7)
})

test_that("inline_scatter handles missing pairs", {
  out <- inline_scatter(c(1, NA, 3), c(2, 3, NA), width = 8, height = 4)

  expect_s3_class(out, "birtir_inline_scatter")
  expect_length(out$lines, 4)
})

test_that("inline_scatter validates vector lengths clearly", {
  expect_error(
    inline_scatter(1:3, 1:2),
    "`x` and `y` must have the same length"
  )
})
