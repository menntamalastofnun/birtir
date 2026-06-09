test_that("summarise_continuous returns expected summary columns", {
  out <- summarise_continuous(c(1, 2, NA, 4))

  expect_named(out, c(
    "n", "n_missing", "pct_missing", "mean", "median", "sd", "se",
    "ci_low", "ci_high", "iqr", "min", "max", "p25", "p75",
    "skewness", "kurtosis", "hist", "box", "box_labels", "n_outliers",
    "outliers", "outlier_summary"
  ))
  expect_equal(out$n, 4)
  expect_equal(out$n_missing, 1)
  expect_equal(out$pct_missing, 25)
  expect_equal(out$mean, mean(c(1, 2, 4)))
  expect_equal(out$median, stats::median(c(1, 2, 4)))
  expect_equal(out$sd, stats::sd(c(1, 2, 4)))
  expect_type(out$hist, "character")
  expect_type(out$box, "character")
  expect_type(out$outliers[[1]], "double")
  expect_named(out$outlier_summary[[1]], c("value", "n", "pct"))
})

test_that("summarise_continuous validates numeric input", {
  expect_error(
    summarise_continuous(factor(c("a", "b"))),
    "`x` must be numeric"
  )
})

test_that("describe_data returns a univariate S3 object", {
  df <- data.frame(x = c(1, 2, NA, 4))

  desc <- df |>
    describe_data(x ~ 1)

  expect_s3_class(desc, "birtir_univariate")
  expect_s3_class(desc, "birtir_description")
  expect_identical(desc$variable, "x")
  expect_identical(desc$type, "continuous")
  expect_equal(desc$summary$n_missing, 1)
})

test_that("print method is compact and returns invisibly", {
  df <- data.frame(x = c(1, 2, NA, 4))
  desc <- df |>
    describe_data(x ~ 1)

  out <- capture.output(returned <- print(desc))

  expect_identical(returned, desc)
  expect_true(any(grepl("x (Continuous)", out, fixed = TRUE)))
  expect_true(any(grepl("95% CI", out, fixed = TRUE)))
  expect_true(any(grepl("Skewness", out, fixed = TRUE)))
})

test_that("print method reports potential outliers", {
  df <- data.frame(x = c(1:10, 100, 100))
  desc <- df |>
    describe_data(x ~ 1)

  out <- capture.output(print(desc))

  expect_true(any(grepl("Potential outliers", out, fixed = TRUE)))
  expect_true(any(grepl("100 (n = 2)", out, fixed = TRUE)))
  expect_false(any(grepl("100, 100", out, fixed = TRUE)))
})

test_that("describe_data errors clearly for missing variables", {
  df <- data.frame(x = c(1, 2, 3))

  expect_error(
    df |> describe_data(y ~ 1),
    "Variable `y` was not found"
  )
})

test_that("describe_data returns grouped summaries for one grouping variable", {
  df <- data.frame(x = c(1, 2, 3), g = factor(c("a", "b", "a"), levels = c("a", "b")))

  desc <- df |>
    describe_data(x ~ g)

  expect_s3_class(desc, "birtir_grouped")
  expect_s3_class(desc, "birtir_description")
  expect_identical(desc$variable, "x")
  expect_identical(names(desc$groups), "g")
  expect_equal(desc$overall_summary$n, 3)
  expect_equal(nrow(desc$summary), 2)
  expect_identical(desc$summary$term, c("g", "g"))
  expect_identical(desc$summary$group, c("a", "b"))
  expect_equal(desc$summary$mean, c(2, 2))
})

test_that("grouped describe_data shows empty factor levels", {
  df <- data.frame(
    x = c(1, 2, 3),
    g = factor(c("a", "b", "a"), levels = c("a", "b", "c"))
  )

  desc <- df |>
    describe_data(x ~ g)

  expect_identical(desc$summary$group, c("a", "b", "c"))
  expect_equal(desc$summary$n, c(2, 1, 0))
  expect_true(is.na(desc$summary$mean[[3]]))
})

test_that("grouped describe_data shows missing groups", {
  df <- data.frame(
    x = c(1, 2, 3),
    g = factor(c("a", NA, "a"), levels = c("a", "b"))
  )

  desc <- df |>
    describe_data(x ~ g)

  expect_identical(desc$summary$group, c("a", "b", "(missing)"))
  expect_equal(desc$summary$n, c(2, 0, 1))
  expect_equal(desc$summary$mean, c(2, NA, 2))
})

test_that("describe_data summarizes non-continuous univariate variables", {
  df <- data.frame(g = factor(c("a", "b", "a")))

  desc <- df |>
    describe_data(g ~ 1)

  expect_s3_class(desc, "birtir_univariate")
  expect_identical(desc$type, "nominal")
  expect_equal(desc$summary$n_levels, 2)
  expect_identical(desc$summary$mode, "a")
})

test_that("describe_data returns separate grouped summaries for additive terms", {
  df <- data.frame(
    x = c(1, 2, 3, 4),
    g = factor(c("a", "a", "b", "b")),
    h = factor(c("u", "v", "u", "v"))
  )

  desc <- df |>
    describe_data(x ~ g + h)

  expect_s3_class(desc, "birtir_grouped")
  expect_equal(nrow(desc$summary), 4)
  expect_identical(desc$summary$term, c("g", "g", "h", "h"))
  expect_identical(desc$summary$group, c("a", "b", "u", "v"))
})

test_that("describe_data returns crossed summaries for interaction terms", {
  df <- data.frame(
    x = c(1, 2, 3, 4),
    g = factor(c("a", "a", "b", "b")),
    h = factor(c("u", "v", "u", "v"))
  )

  interaction_only <- df |>
    describe_data(x ~ g:h)

  expanded <- df |>
    describe_data(x ~ g * h)

  expect_equal(nrow(interaction_only$summary), 4)
  expect_identical(interaction_only$summary$term, rep("g:h", 4))
  expect_identical(
    interaction_only$summary$group,
    c("g = a, h = u", "g = b, h = u", "g = a, h = v", "g = b, h = v")
  )

  expect_equal(nrow(expanded$summary), 8)
  expect_identical(names(expanded$groups), c("g", "h", "g:h"))
})

test_that("interaction grouped summaries show empty factor combinations", {
  df <- data.frame(
    x = c(1, 2),
    g = factor(c("a", "b"), levels = c("a", "b")),
    h = factor(c("u", "u"), levels = c("u", "v"))
  )

  desc <- df |>
    describe_data(x ~ g:h)

  expect_identical(
    desc$summary$group,
    c("g = a, h = u", "g = b, h = u", "g = a, h = v", "g = b, h = v")
  )
  expect_equal(desc$summary$n, c(1, 1, 0, 0))
})

test_that("grouped describe_data print method returns invisibly", {
  df <- data.frame(x = c(1, 2, 3), g = factor(c("a", "b", "a")))
  desc <- df |>
    describe_data(x ~ g)

  out <- capture.output(returned <- print(desc))

  expect_identical(returned, desc)
  expect_true(any(grepl("Total: x (Continuous)", out, fixed = TRUE)))
  expect_true(any(grepl("x ~ g", out, fixed = TRUE)))
  expect_true(any(grepl("g: a", out, fixed = TRUE)))
  expect_true(any(grepl("95% CI", out, fixed = TRUE)))
})

test_that("describe_data errors clearly for missing grouping variables", {
  df <- data.frame(x = c(1, 2, 3))

  expect_error(
    df |> describe_data(x ~ g),
    "Right-hand side variable `g` was not found"
  )
})

test_that("describe_data returns continuous-continuous relationship summaries", {
  df <- data.frame(x = c(1, 2, 3), y = c(2, 4, 6))

  desc <- df |>
    describe_data(x ~ y)

  expect_s3_class(desc, "birtir_relationship")
  expect_identical(desc$relationships$relationship, "continuous-continuous")
  expect_equal(desc$relationships$statistic, 1)
  expect_equal(desc$relationships$estimate, 0.5)
})

test_that("describe_data converts character grouping variables to factors internally", {
  df <- data.frame(x = c(1, 2, 3), g = c("a", "b", "a"))

  desc <- df |>
    describe_data(x ~ g)

  expect_s3_class(desc, "birtir_grouped")
  expect_identical(desc$summary$group, c("a", "b"))
  expect_equal(desc$summary$mean, c(2, 2))
  expect_type(df$g, "character")
})

test_that("describe_data summarizes random intercept formulas", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    g = factor(c("a", "a", "b", "b", "c", "c"))
  )

  desc <- df |>
    describe_data(x ~ (1 | g))

  expect_s3_class(desc, "birtir_random_effects")
  expect_s3_class(desc, "birtir_grouped")
  expect_identical(names(desc$groups), "(1 | g)")
  expect_identical(desc$groups[["(1 | g)"]], "g")
  expect_equal(desc$overall_summary$n, 6)
  expect_identical(desc$summary$term, rep("(1 | g)", 3))
  expect_identical(desc$summary$group, c("a", "b", "c"))
  expect_equal(desc$summary$mean, c(1.5, 3.5, 5.5))
})

test_that("random intercept print method uses compact forest output", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    g = factor(c("a", "a", "b", "b", "c", "c"))
  )

  desc <- df |>
    describe_data(x ~ (1 | g), max_random_effects = 2)

  out <- capture.output(returned <- print(desc))

  expect_identical(returned, desc)
  expect_true(any(grepl("Random effects", out, fixed = TRUE)))
  expect_true(any(grepl("(1 | g)", out, fixed = TRUE)))
  expect_true(any(grepl("Grand mean", out, fixed = TRUE)))
  expect_true(any(grepl("n  estimate", out, fixed = TRUE)))
  expect_true(any(grepl("1 more levels", out, fixed = TRUE)))
})

test_that("random intercept forest prints grand mean before group levels", {
  df <- data.frame(
    x = c(1, 2, 3, 4),
    g = factor(c("a", "a", "b", "b"))
  )

  desc <- df |>
    describe_data(x ~ (1 | g))

  out <- capture.output(print(desc))
  grand_mean_row <- grep("Grand mean", out, fixed = TRUE)
  group_row <- grep("^a\\s", out)

  expect_length(grand_mean_row, 1)
  expect_length(group_row, 1)
  expect_lt(grand_mean_row, group_row)
})

test_that("random intercept print method can hide level details", {
  df <- data.frame(
    x = c(1, 2, 3, 4),
    g = factor(c("a", "a", "b", "b"))
  )

  desc <- df |>
    describe_data(x ~ (1 | g), show_random_effects = FALSE)

  out <- capture.output(print(desc))

  expect_true(any(grepl("(1 | g): 2 levels", out, fixed = TRUE)))
  expect_false(any(grepl("estimate    [95% CI]", out, fixed = TRUE)))
})

test_that("describe_data rejects random slopes for now", {
  df <- data.frame(
    x = c(1, 2, 3, 4),
    z = c(0, 1, 0, 1),
    g = factor(c("a", "a", "b", "b"))
  )

  expect_error(
    df |> describe_data(x ~ (1 + z | g)),
    "random intercept terms only"
  )
})

test_that("describe_data returns multivariable relationship summaries", {
  df <- data.frame(
    y = c(1, 2, 3, 4, 5, 6),
    x = c(1, 2, 3, 4, 5, 6),
    g = factor(c("a", "a", "b", "b", "a", "b"))
  )

  desc <- df |>
    describe_data(y ~ x + g)

  expect_s3_class(desc, "birtir_relationship")
  expect_identical(desc$relationships$term, c("x", "g"))
  expect_identical(desc$relationships$relationship, c("continuous-continuous", "continuous-group"))
  expect_named(desc$variable_summaries, c("variable", "role", "type", "n", "n_missing", "pct_missing"))
})

test_that("describe_data recognizes continuous-by-group interactions", {
  df <- data.frame(
    y = c(1, 2, 3, 4, 2, 4, 6, 8),
    x = c(1, 2, 3, 4, 1, 2, 3, 4),
    g = factor(rep(c("a", "b"), each = 4))
  )

  desc <- df |>
    describe_data(y ~ x:g)

  expect_s3_class(desc, "birtir_relationship")
  expect_identical(desc$relationships$relationship, "continuous-by-group interaction")
  expect_named(desc$relationships$detail[[1]]$slopes, c("group", "n", "correlation", "slope", "ci_low", "ci_high"))
})

test_that("describe_data summarizes categorical outcome relationships", {
  df <- data.frame(
    y = factor(c("no", "yes", "yes", "no")),
    g = factor(c("a", "a", "b", "b"))
  )

  desc <- df |>
    describe_data(y ~ g)

  expect_s3_class(desc, "birtir_relationship")
  expect_identical(desc$type, "nominal")
  expect_identical(desc$relationships$relationship, "contingency")
  expect_named(desc$relationships$detail[[1]]$counts, c("outcome", "predictor", "n", "pct"))
})

test_that("relationship print method returns invisibly", {
  df <- data.frame(y = c(2, 4, 6), x = c(1, 2, 3))
  desc <- df |>
    describe_data(y ~ x)

  out <- capture.output(returned <- print(desc))

  expect_identical(returned, desc)
  expect_true(any(grepl("Relationships", out, fixed = TRUE)))
  expect_true(any(grepl("slope", out, fixed = TRUE)))
})

test_that("report helpers return text and tables", {
  df <- data.frame(y = c(2, 4, 6), x = c(1, 2, 3))
  desc <- df |>
    describe_data(y ~ x)

  text <- as_report_text(desc)
  table <- as_report_table(desc)

  expect_type(text, "character")
  expect_length(text, 1)
  expect_s3_class(table, "tbl_df")
  expect_named(table, c(
    "term", "predictors", "outcome_type", "predictor_type",
    "relationship", "n", "estimate", "statistic", "ci_low", "ci_high", "note"
  ))
})
