test_that("render_analysis_md captures output, directives, and helpers", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "example_report.R")

  writeLines(
    c(
      "#| h1: Example report",
      "#| text: Intro paragraph.",
      "",
      "x <- 1 + 1",
      "x",
      "",
      "md_table(data.frame(term = 'x', value = x), caption = 'Values')"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^# Example report$", md_lines)))
  expect_true(any(grepl("^Intro paragraph\\.$", md_lines)))
  expect_true(any(grepl("^\\[Table: example-report_tbl-001\\.md\\]", md_lines)))
  expect_true(any(grepl("^\\[1\\] 2$", md_lines)))
  expect_true(file.exists(file.path(dirname(output_path), "tables", "example-report_tbl-001.md")))
})

test_that("render_analysis_md supports namespaced helper usage inside scripts", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "namespaced_helpers.R")

  writeLines(
    c(
      "#| h1: Namespaced helpers",
      "tbl <- data.frame(term = 'x', value = 2)",
      "birtir::md_table(tbl, caption = 'Namespaced table')",
      "",
      "library(ggplot2)",
      "p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()",
      "birtir::md_plot(p, caption = 'Namespaced plot')"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)
  report_dir <- dirname(output_path)

  expect_true(any(grepl("^# Namespaced helpers$", md_lines)))
  expect_true(any(grepl("^\\*\\*Namespaced table\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*\\*Figure 1\\. Namespaced plot\\*\\*$", md_lines)))
  expect_true(file.exists(file.path(report_dir, "tables", "namespaced-helpers_tbl-001.md")))
  expect_true(file.exists(file.path(report_dir, "images", "namespaced-helpers_fig-001.png")))
})

test_that("md_table prints a markdown preview outside render mode", {
  output <- capture.output(
    result <- md_table(
      data.frame(term = "x", value = 2),
      caption = "Values"
    )
  )

  expect_true(any(grepl("^\\*\\*Values\\*\\*$", output)))
  expect_true(any(grepl("^\\|term\\s*\\|", output)))
  expect_true(is.character(result))
})

test_that("md_plot returns the plot object outside render mode", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  result <- md_plot(plot)

  expect_s3_class(result, "ggplot")
})

test_that("render_analysis_md keeps code blocks intact across blank lines", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "multiline_expression.R")

  writeLines(
    c(
      "result <- (",
      "  1 +",
      "",
      "  2",
      ")",
      "result"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^\\[1\\] 3$", md_lines)))
})

test_that("render_analysis_md supports scripts that attach packages", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "library_script.R")

  writeLines(
    c(
      "library(ggplot2)",
      "",
      "p <- ggplot(mtcars, aes(wt, mpg)) +",
      "  geom_point()",
      "",
      "birtir::md_plot(p, caption = 'Scatter')"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^\\*\\*Figure 1\\. Scatter\\*\\*$", md_lines)))
  expect_true(file.exists(file.path(dirname(output_path), "images", "library-script_fig-001.png")))
})

test_that("render_analysis_md reports invalid directives clearly", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "bad_directive.R")

  writeLines(
    c(
      "#| text: ok",
      "#| Prufa"
    ),
    script_path
  )

  expect_error(
    render_analysis_md(script_path, output_dir = script_dir),
    "Invalid birtir directive.*Supported directives"
  )
})

test_that("render_analysis_md captures messages, warnings, and errors", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "conditions.R")

  writeLines(
    c(
      "message('hello')",
      "warning('careful')",
      "stop('boom')"
    ),
    script_path
  )

  expect_error(
    render_analysis_md(script_path, output_dir = script_dir),
    "near line 3: boom"
  )

  md_path <- file.path(script_dir, "conditions", "conditions.md")
  md_lines <- readLines(md_path, warn = FALSE)

  expect_true(any(grepl("^Message: hello$", md_lines)))
  expect_true(any(grepl("^Warning: careful$", md_lines)))
  expect_true(any(grepl("^Error: boom$", md_lines)))
})

test_that("legacy helpers emit deprecation warnings", {
  expect_warning(
    fa_heildartolu(),
    "deprecated"
  )
})
