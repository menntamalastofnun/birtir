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
  expect_true(any(grepl("^\\*\\*Table 1\\. Values\\*\\*$", md_lines)))
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
  expect_true(any(grepl("^\\*\\*Table 1\\. Namespaced table\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*\\*Figure 1\\. Namespaced plot\\*\\*$", md_lines)))
  expect_true(file.exists(file.path(report_dir, "tables", "namespaced-helpers_tbl-001.md")))
  expect_true(file.exists(file.path(report_dir, "images", "namespaced-helpers_fig-001.png")))
})

test_that("render_analysis_md uses custom report labels", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "custom_labels.R")

  writeLines(
    c(
      "tbl <- data.frame(term = 'x', value = 2)",
      "birtir::md_table(tbl, caption = 'Tafla lysing')",
      "",
      "library(ggplot2)",
      "p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()",
      "birtir::md_plot(p, caption = 'Mynd lysing')"
    ),
    script_path
  )

  labels_is <- report_labels(
    table = "Tafla",
    figure = "Mynd"
  )

  output_path <- render_analysis_md(
    script_path,
    output_dir = script_dir,
    labels = labels_is
  )
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^\\*\\*Tafla 1\\. Tafla lysing\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*\\*Mynd 1\\. Mynd lysing\\*\\*$", md_lines)))
})

test_that("render_analysis_md supports a custom report name", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "item_analysis_template.R")

  writeLines(
    c(
      "x <- 1 + 1",
      "x"
    ),
    script_path
  )

  output_path <- render_analysis_md(
    script_path,
    output_dir = script_dir,
    report_name = "file01"
  )

  expect_match(basename(output_path), "^file01\\.md$")
  expect_true(file.exists(file.path(script_dir, "file01", "file01.md")))
})

test_that("render_analysis_md supports book layout with shared assets", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "book_layout.R")

  writeLines(
    c(
      "#| h1: Book layout",
      "tbl <- data.frame(term = 'x', value = 2)",
      "birtir::md_table(tbl, caption = 'Shared table')",
      "",
      "library(ggplot2)",
      "p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()",
      "birtir::md_plot(p, caption = 'Shared plot')"
    ),
    script_path
  )

  output_path <- render_analysis_md(
    script_path,
    output_dir = script_dir,
    layout = "book"
  )
  md_lines <- readLines(output_path, warn = FALSE)

  expect_identical(normalizePath(dirname(output_path), winslash = "/"), normalizePath(script_dir, winslash = "/"))
  expect_true(file.exists(file.path(script_dir, "book-layout.md")))
  expect_true(file.exists(file.path(script_dir, "tables", "book-layout_tbl-001.md")))
  expect_true(file.exists(file.path(script_dir, "images", "book-layout_fig-001.png")))
  expect_false(dir.exists(file.path(script_dir, "book-layout")))
  expect_true(any(grepl("^\\[Table: book-layout_tbl-001\\.md\\]\\(tables/book-layout_tbl-001\\.md\\)$", md_lines)))
  expect_true(any(grepl("^!\\[\\]\\(images/book-layout_fig-001\\.png\\)$", md_lines)))
})

test_that("render_analysis_md defaults to report layout", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "default_layout.R")

  writeLines("1 + 1", script_path)

  output_path <- render_analysis_md(script_path, output_dir = script_dir)

  expect_identical(
    normalizePath(dirname(output_path), winslash = "/"),
    normalizePath(file.path(script_dir, "default-layout"), winslash = "/")
  )
  expect_true(file.exists(file.path(script_dir, "default-layout", "default-layout.md")))
})

test_that("render_analysis_md exposes params inside the script", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "item_analysis_template.R")

  writeLines(
    c(
      "#| h1: Param example",
      "id",
      "params$id",
      "md_table(data.frame(id = id), caption = paste('Report for', id))"
    ),
    script_path
  )

  output_path <- render_analysis_md(
    script_path,
    output_dir = script_dir,
    report_name = "file01",
    params = list(id = "LES_04_A1")
  )

  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^\\[1\\] \"LES_04_A1\"$", md_lines)))
  expect_true(any(grepl("^\\*\\*Table 1\\. Report for LES_04_A1\\*\\*$", md_lines)))
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
  withr::local_dir(withr::local_tempdir())

  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  result <- md_plot(plot)

  expect_s3_class(result, "ggplot")
})

test_that("fmt_num formats numbers for inline reporting", {
  expect_identical(fmt_num(c(0.1234, 2.5), digits = 2), c(".12", "2.50"))
  expect_identical(
    fmt_num(0.1234, digits = 2, drop_leading_zero = FALSE),
    "0.12"
  )
  expect_identical(
    fmt_num(c(0.1234, 2.5), digits = 2, decimal_mark = ","),
    c(",12", "2,50")
  )
})

test_that("fmt_p formats p-values for inline reporting", {
  expect_identical(fmt_p(0.0004), "< .001")
  expect_identical(fmt_p(0.0234), "= .023")
  expect_identical(fmt_p(0.0234, drop_leading_zero = FALSE), "= 0.023")
  expect_identical(fmt_p(0.0234, decimal_mark = ","), "= ,023")
})

test_that("md_text formats inline numeric values outside render mode", {
  output <- capture.output(
    result <- md_text("This is _R_ = {0.1234} and _M_ = {2.5}", digits = 2)
  )

  expect_identical(result, "This is _R_ = .12 and _M_ = 2.50")
  expect_identical(output, "This is _R_ = .12 and _M_ = 2.50")
})

test_that("md_text can keep the leading zero when requested", {
  result <- md_text(
    "This is _R_ = {0.1234}",
    digits = 2,
    drop_leading_zero = FALSE
  )

  expect_identical(result, "This is _R_ = 0.12")
})

test_that("md_text formats p-values in APA style", {
  result_small <- md_text("p {0.0004}", style = "p", digits = 3)
  result_large <- md_text("p {0.0234}", style = "p", digits = 3)

  expect_identical(result_small, "p < .001")
  expect_identical(result_large, "p = .023")
})

test_that("render_analysis_md supports md_text inside scripts", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "md_text_script.R")

  writeLines(
    c(
      "#| h1: Inline text",
      "x <- 0.4567",
      "md_text('This is _R_ = {x}', digits = 2)"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^# Inline text$", md_lines)))
  expect_true(any(grepl("^This is _R_ = \\.46$", md_lines)))
})

test_that("render_analysis_md can set decimal_mark globally for md_text", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "md_text_comma.R")

  writeLines(
    c(
      "#| h1: Inline text",
      "x <- 0.4567",
      "p <- 0.0234",
      "md_text('p {p}', style = 'p', digits = 3)",
      "md_text('Estimate = {x}', digits = 2)"
    ),
    script_path
  )

  output_path <- render_analysis_md(
    script_path,
    output_dir = script_dir,
    decimal_mark = ","
  )
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^p = ,023$", md_lines)))
  expect_true(any(grepl("^Estimate = ,46$", md_lines)))
})

test_that("render_analysis_md reserves the md_text helper name", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "reserved_md_text.R")

  writeLines("42", script_path)

  expect_error(
    render_analysis_md(
      script_path,
      output_dir = script_dir,
      params = list(md_text = "conflict")
    ),
    "reserved names: md_text"
  )
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
