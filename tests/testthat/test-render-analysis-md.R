test_that("render_analysis_md captures output, text, and helpers", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "example_report.R")

  writeLines(
    c(
      "md_text('# Example report')",
      "md_text('Intro paragraph.')",
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
  expect_true(any(grepl("^\\*\\*Table 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Values\\*$", md_lines)))
  expect_true(any(grepl("^\\[Table: example-report_tbl-001\\.md\\]", md_lines)))
  expect_true(any(grepl("^\\[1\\] 2$", md_lines)))
  expect_true(file.exists(file.path(dirname(output_path), "tables", "example-report_tbl-001.md")))
})

test_that("render_analysis_md supports namespaced helper usage inside scripts", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "namespaced_helpers.R")

  writeLines(
    c(
      "birtir::md_text('# Namespaced helpers')",
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
  expect_true(any(grepl("^\\*\\*Table 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Namespaced table\\*$", md_lines)))
  expect_true(any(grepl("^\\*\\*Figure 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Namespaced plot\\*$", md_lines)))
  expect_true(file.exists(file.path(report_dir, "tables", "namespaced-helpers_tbl-001.md")))
  expect_true(file.exists(file.path(report_dir, "images", "namespaced-helpers_fig-001.png")))
})

test_that("render_analysis_md keeps helper output in script order", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "script_order.R")

  writeLines(
    c(
      "data <- data.frame(score = c(1, 2, 3))",
      "summary(data)",
      "md_table(data, caption = 'Scores')",
      "'after table'"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  summary_line <- grep("Min\\.", md_lines)[[1]]
  table_line <- grep("^\\*\\*Table 1\\*\\*$", md_lines)[[1]]
  after_line <- grep("^\\[1\\] \"after table\"$", md_lines)[[1]]

  expect_lt(summary_line, table_line)
  expect_lt(table_line, after_line)
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

  expect_true(any(grepl("^\\*\\*Tafla 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Tafla lysing\\*$", md_lines)))
  expect_true(any(grepl("^\\*\\*Mynd 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Mynd lysing\\*$", md_lines)))
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
      "birtir::md_text('# Book layout')",
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
      "md_text('# Param example')",
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
  expect_true(any(grepl("^\\[1\\] \"LES_04_A1\"$", md_lines)))
  expect_true(any(grepl("^\\*\\*Table 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Report for LES_04_A1\\*$", md_lines)))
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

test_that("md_table formats APA 7 table notes in preview and render mode", {
  output <- capture.output(
    md_table(
      data.frame(term = "x", value = 2),
      caption = "Values",
      note = "All tests were two-tailed."
    )
  )
  expect_true(any(grepl("^\\*Note\\.\\* All tests were two-tailed\\.$", output)))

  output_explicit <- capture.output(
    md_table(
      data.frame(term = "x", value = 2),
      note = "*Note.* Already formatted."
    )
  )
  expect_true(any(grepl("^\\*Note\\.\\* Already formatted\\.$", output_explicit)))

  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "note-table.R")

  writeLines(
    c(
      "tbl <- data.frame(a = 1)",
      "birtir::md_table(tbl, caption = 'Noted table', note = 'Custom note text.')"
    ),
    script_path
  )

  output_path <- render_analysis_md(
    script_path,
    output_dir = script_dir,
    report_name = "report_note"
  )

  md_lines <- readLines(output_path, warn = FALSE)
  expect_true(any(grepl("^\\*Note\\.\\* Custom note text\\.$", md_lines)))

  table_file <- file.path(dirname(output_path), "tables", "report-note_tbl-001.md")
  table_lines <- readLines(table_file, warn = FALSE)
  expect_true(any(grepl("^\\*Note\\.\\* Custom note text\\.$", table_lines)))
})

test_that("md_plot formats APA 7 figure notes in preview and render mode", {
  output <- capture.output(
    md_plot(
      ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point(),
      caption = "Scatter plot",
      note = "Values measured at baseline."
    )
  )
  expect_true(any(grepl("^\\*Note\\.\\* Values measured at baseline\\.$", output)))

  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "note-plot.R")

  writeLines(
    c(
      "library(ggplot2)",
      "p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()",
      "birtir::md_plot(p, caption = 'Noted figure', note = 'Custom plot note.')"
    ),
    script_path
  )

  output_path <- render_analysis_md(
    script_path,
    output_dir = script_dir,
    report_name = "report_fig_note"
  )

  md_lines <- readLines(output_path, warn = FALSE)
  expect_true(any(grepl("^\\*\\*Figure 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Noted figure\\*$", md_lines)))
  expect_true(any(grepl("^\\*Note\\.\\* Custom plot note\\.$", md_lines)))
})

test_that("md_plot returns the plot object outside render mode", {
  withr::local_dir(withr::local_tempdir())

  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  result <- md_plot(plot)

  expect_s3_class(result, "ggplot")
})

test_that("md_plot saves non-ggplot plot objects during rendering", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "custom-plot.R")

  writeLines(
    c(
      "print.qgraph_mock <- function(x, ...) {",
      "  plot(1:3, c(3, 1, 2), type = 'b')",
      "}",
      "",
      "p <- structure(list(), class = 'qgraph_mock')",
      "birtir::md_plot(p, caption = 'Custom plot')"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^\\*\\*Figure 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Custom plot\\*$", md_lines)))
  expect_true(file.exists(file.path(dirname(output_path), "images", "custom-plot_fig-001.png")))
})

test_that("md_plot redraws qgraph objects with plot.qgraph during rendering", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "qgraph-plot.R")
  marker_path <- file.path(script_dir, "qgraph-method.txt")
  marker_path_script <- gsub("\\\\", "/", marker_path)

  writeLines(
    c(
      "assign('plot.qgraph', function(x, ...) {",
      "  stopifnot(isTRUE(x$plotOptions$plot))",
      "  stopifnot(identical(x$plotOptions$filetype, 'default'))",
      paste0("  writeLines('plot', '", marker_path_script, "')"),
      "  plot.new()",
      "  rect(0.2, 0.2, 0.8, 0.8, col = 'black')",
      "}, envir = .GlobalEnv)",
      "",
      "assign('print.qgraph', function(x, ...) {",
      paste0("  writeLines('print', '", marker_path_script, "')"),
      "}, envir = .GlobalEnv)",
      "",
      "p <- structure(",
      "  list(plotOptions = list(plot = FALSE, filetype = 'png')),",
      "  class = 'qgraph'",
      ")",
      "birtir::md_plot(p, caption = 'Network plot')"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^\\*\\*Figure 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Network plot\\*$", md_lines)))
  expect_true(file.exists(file.path(dirname(output_path), "images", "qgraph-plot_fig-001.png")))
  expect_identical(readLines(marker_path, warn = FALSE), "plot")
})

test_that("md_text formats inline numeric values outside render mode", {
  output <- capture.output(
    result <- md_text("This is _R_ = {0.1234} and _M_ = {2.5}", digits = 2)
  )

  expect_identical(result, "This is _R_ = 0.12 and _M_ = 2.50")
  expect_identical(output, "This is _R_ = 0.12 and _M_ = 2.50")
})

test_that("md_text can drop the leading zero when requested", {
  result <- md_text(
    "This is _R_ = {0.1234}",
    digits = 2,
    drop_leading_zero = TRUE
  )

  expect_identical(result, "This is _R_ = .12")
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
      "md_text('# Inline text')",
      "x <- 0.4567",
      "md_text('This is _R_ = {x}', digits = 2)"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)

  expect_true(any(grepl("^# Inline text$", md_lines)))
  expect_true(any(grepl("^This is _R_ = 0\\.46$", md_lines)))
})

test_that("render_analysis_md can set decimal_mark globally for md_text", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "md_text_comma.R")

  writeLines(
    c(
      "md_text('# Inline text')",
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
  expect_true(any(grepl("^Estimate = 0,46$", md_lines)))
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

test_that("render_analysis_md renders description helper output", {
  script_dir <- withr::local_tempdir()
  script_path <- file.path(script_dir, "description_report.R")

  writeLines(
    c(
      "md_text('# Description report')",
      "df <- data.frame(score = c(1, 2, 3, NA), group = factor(c('a', 'a', 'b', 'b')))",
      "desc <- describe_data(df, score ~ group)",
      "md_text(as_report_text(desc))",
      "md_table(as_report_table(desc), caption = 'Score by group', digits = 2)"
    ),
    script_path
  )

  output_path <- render_analysis_md(script_path, output_dir = script_dir)
  md_lines <- readLines(output_path, warn = FALSE)
  table_path <- file.path(dirname(output_path), "tables", "description-report_tbl-001.md")

  expect_true(any(grepl("^# Description report$", md_lines)))
  expect_true(any(grepl("^\\*\\*Table 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Score by group\\*$", md_lines)))
  expect_true(any(grepl("score ~ group", md_lines, fixed = TRUE)))
  expect_true(file.exists(table_path))
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

  expect_true(any(grepl("^\\*\\*Figure 1\\*\\*$", md_lines)))
  expect_true(any(grepl("^\\*Scatter\\*$", md_lines)))
  expect_true(file.exists(file.path(dirname(output_path), "images", "library-script_fig-001.png")))
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

test_that("convert_md rejects non-markdown input", {
  input_dir <- withr::local_tempdir()
  txt_path <- file.path(input_dir, "report.txt")
  writeLines("hello", txt_path)

  expect_error(
    convert_md(txt_path),
    "`path` must point to an existing `.md` file."
  )
})

test_that("convert_md errors clearly when pandoc is unavailable", {
  input_dir <- withr::local_tempdir()
  md_path <- file.path(input_dir, "report.md")
  writeLines("# Hello", md_path)

  local_mocked_bindings(
    birtir_find_pandoc = function() "",
    .package = "birtir"
  )

  expect_error(
    convert_md(md_path),
    "Pandoc is required for `convert_md\\(\\)` but was not found on this system."
  )
})

test_that("convert_md calls pandoc and returns the output path", {
  input_dir <- withr::local_tempdir()
  md_path <- file.path(input_dir, "report.md")
  writeLines("# Hello", md_path)

  call_args <- NULL

  local_mocked_bindings(
    birtir_find_pandoc = function() "C:/Pandoc/pandoc.exe",
    birtir_run_pandoc = function(pandoc, input, output, workdir) {
      call_args <<- list(pandoc = pandoc, input = input, output = output, workdir = workdir)
      0L
    },
    .package = "birtir"
  )

  output_path <- convert_md(md_path, to = "docx")
  expected_output <- gsub("\\\\", "/", file.path(input_dir, "report.docx"))

  expect_identical(as.character(output_path), expected_output)
  expect_identical(call_args$pandoc, "C:/Pandoc/pandoc.exe")
  expect_identical(call_args$input, "report.md")
  expect_identical(as.character(call_args$output), expected_output)
  expect_identical(call_args$workdir, gsub("\\\\", "/", input_dir))
})

test_that("convert_md resolves relative assets from the markdown directory", {
  input_dir <- withr::local_tempdir()
  md_path <- file.path(input_dir, "report.md")
  image_dir <- file.path(input_dir, "images")
  image_path <- file.path(image_dir, "plot.png")

  dir.create(image_dir)
  writeLines("![](images/plot.png)", md_path)
  writeBin(as.raw(c(137, 80, 78, 71)), image_path)

  call_args <- NULL

  local_mocked_bindings(
    birtir_find_pandoc = function() "C:/Pandoc/pandoc.exe",
    birtir_run_pandoc = function(pandoc, input, output, workdir) {
      call_args <<- list(pandoc = pandoc, input = input, output = output, workdir = workdir)
      0L
    },
    .package = "birtir"
  )

  convert_md(md_path, to = "html")

  expect_identical(call_args$input, "report.md")
  expect_identical(call_args$workdir, gsub("\\\\", "/", input_dir))
})

test_that("convert_md strips redundant table-file links only in the temporary export copy", {
  input_dir <- withr::local_tempdir()
  md_path <- file.path(input_dir, "report.md")
  original_lines <- c(
    "**Tafla 4. Meðaltöl og staðalfrávik eftir stærð fjölliðu - ALG**",
    "",
    "[Table: 02-presmooth-grade-10-stf-10-a1_tbl-004.md](tables/02-presmooth-grade-10-stf-10-a1_tbl-004.md)",
    "",
    "|col|value|",
    "|---|---|",
    "|x|1|"
  )
  writeLines(original_lines, md_path)

  call_args <- NULL
  temp_lines <- NULL

  local_mocked_bindings(
    birtir_find_pandoc = function() "C:/Pandoc/pandoc.exe",
    birtir_run_pandoc = function(pandoc, input, output, workdir) {
      call_args <<- list(pandoc = pandoc, input = input, output = output, workdir = workdir)
      temp_lines <<- readLines(file.path(workdir, input), warn = FALSE)
      0L
    },
    .package = "birtir"
  )

  convert_md(md_path, to = "html")

  expect_false(any(grepl("^\\[Table:", temp_lines)))
  expect_true(any(grepl("^\\*\\*Tafla 4\\.", temp_lines)))
  expect_true(any(grepl("^\\|col\\|value\\|$", temp_lines)))
  expect_identical(readLines(md_path, warn = FALSE), original_lines)
  expect_false(identical(call_args$input, "report.md"))
  expect_true(grepl("^report-convert-.*\\.md$", call_args$input))
})

test_that("render_analysis_md supports nested rendering without state pollution", {
  script_dir <- withr::local_tempdir()
  inner_script_path <- file.path(script_dir, "inner.R")
  outer_script_path <- file.path(script_dir, "outer.R")

  writeLines(
    c(
      "md_text('# Inner report')",
      "md_text('Inner body.')"
    ),
    inner_script_path
  )

  inner_path_escaped <- gsub("\\\\", "/", inner_script_path)

  writeLines(
    c(
      "md_text('# Outer report')",
      paste0("birtir::render_analysis_md('", inner_path_escaped, "', output_dir = params$output_dir)"),
      "md_text('Outer body.')"
    ),
    outer_script_path
  )

  expect_length(birtir:::birtir_runtime$states, 0)

  # Render outer script
  output_path <- render_analysis_md(
    outer_script_path,
    output_dir = script_dir,
    params = list(output_dir = script_dir)
  )

  expect_length(birtir:::birtir_runtime$states, 0)

  # Verify inner output
  inner_md_path <- file.path(script_dir, "inner", "inner.md")
  expect_true(file.exists(inner_md_path))
  inner_lines <- readLines(inner_md_path, warn = FALSE)
  expect_true(any(grepl("^# Inner report$", inner_lines)))
  expect_true(any(grepl("^Inner body\\.$", inner_lines)))

  # Verify outer output
  outer_lines <- readLines(output_path, warn = FALSE)
  expect_true(any(grepl("^# Outer report$", outer_lines)))
  expect_true(any(grepl("^Outer body\\.$", outer_lines)))
  expect_false(any(grepl("Inner report", outer_lines)))
})

test_that("render_analysis_md cleans up stack on successful and failed rendering runs", {
  script_dir <- withr::local_tempdir()
  
  # 1. Success case
  success_script <- file.path(script_dir, "success.R")
  writeLines("md_text('OK')", success_script)
  
  expect_length(birtir:::birtir_runtime$states, 0)
  render_analysis_md(success_script, output_dir = script_dir)
  expect_length(birtir:::birtir_runtime$states, 0)

  # 2. Failure case
  fail_script <- file.path(script_dir, "fail.R")
  writeLines(c("md_text('Before')", "stop('boom')"), fail_script)

  expect_length(birtir:::birtir_runtime$states, 0)
  expect_error(render_analysis_md(fail_script, output_dir = script_dir), "boom")
  expect_length(birtir:::birtir_runtime$states, 0)
})

