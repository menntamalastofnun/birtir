
<!-- README.md is generated from README.Rmd. Please edit that file -->

# birtir

<!-- badges: start -->

<!-- badges: end -->

Render R analysis scripts into clean, plain Markdown reports.

`birtir` turns a plain `.R` analysis script into a plain `.md` report.
It captures console output, saves plots and tables, and writes a
structured Markdown artifact without requiring Quarto, R Markdown, or
HTML rendering.

## Installation

You can install the development version of birtir from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("auv2/birtir")
```

## Basic usage

Write a normal analysis script with minimal structure:

``` r
birtir::md_text("# Regression example\n\nSimple linear model using mtcars.")

model <- lm(mpg ~ wt, data = mtcars)
summary(model)

coef_tbl <- as.data.frame(summary(model)$coefficients) |>
  tibble::rownames_to_column("term")

birtir::md_table(coef_tbl, caption = "Coefficient table", digits = 3)

birtir::md_text("## Plot")

p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

birtir::md_plot(p, caption = "MPG vs weight")
```

Render it with:

``` r
birtir::render_analysis_md("scripts/regression_example.R")
```

Use `report_name` when the output should use a different name from the
script:

``` r
birtir::render_analysis_md(
  script = "scripts/regression_example.R",
  report_name = "regression-mtcars",
  params = list(dataset = "mtcars")
)
```

This creates:

``` text
outputs/
  regression-mtcars/
    regression-mtcars.md
    images/
      regression-mtcars_fig-001.png
    tables/
      regression-mtcars_tbl-001.md
```

If `report_name` is omitted, `birtir` uses the script file name without
the extension.

Use `layout = "book"` when multiple rendered reports should share one
output root:

``` r
birtir::render_analysis_md(
  script = "scripts/regression_example.R",
  report_name = "regression-mtcars",
  layout = "book"
)
```

This creates:

``` text
outputs/
  regression-mtcars.md
  images/
    regression-mtcars_fig-001.png
  tables/
    regression-mtcars_tbl-001.md
```

## Template workflows

`report_name` and `params` are especially useful when rendering one
analysis template for many files:

``` r
purrr::pwalk(
  list(
    report_name = df$name,
    id = df$id
  ),
  \(report_name, id) {
    birtir::render_analysis_md(
      script = "dev/item_analysis_template.R",
      report_name = report_name,
      labels = birtir::report_labels("Tafla", "Mynd"),
      params = list(id = id)
    )
  }
)
```

For example, `df` could look like:

``` r
df |> dplyr::select(name, id)
```

Inside `item_analysis_template.R`, those values are available directly:

``` r
md_text("# Item analysis")

id
params$id
```

Each param is available by name in the script environment, and the full
list is also available as `params`.

If you only need custom naming, you can still use `report_name` on its
own:

``` r
purrr::walk2(
  data_files,
  tools::file_path_sans_ext(basename(data_files)),
  \(data_file, report_name) {
    birtir::render_analysis_md(
      script = "dev/item_analysis_template.R",
      report_name = report_name
    )
  }
)
```

## Interactive use

The same helpers also work outside rendering:

``` r
coef_tbl |> birtir::md_table(caption = "Coefficient table", digits = 3)
birtir::md_plot(p, caption = "MPG vs weight")
birtir::md_text("Model fit: _R_ = {0.4567}, p {0.0234}", style = "apa", digits = 2)
birtir::fmt_num(c(0.4567, 2.5), digits = 2)
birtir::fmt_p(c(0.0234, 0.0004))
```

Outside `render_analysis_md()`:

- `birtir::md_table()` prints a pipe-table Markdown preview
- `birtir::md_plot()` prints the ggplot normally
- `birtir::md_text()` prints Markdown-ready inline text with glue
  formatting
- `birtir::fmt_num()` returns formatted numbers for inline reporting
- `birtir::fmt_p()` returns APA-style p-value strings such as `= .023`
  or `< .001`
- `birtir::convert_md()` converts an existing `.md` file with Pandoc

## Custom labels

Use `report_labels()` to customize rendered caption names:

``` r
labels_is <- birtir::report_labels(
  table = "Tafla",
  figure = "Mynd"
)

birtir::render_analysis_md(
  "scripts/regression_example.R",
  labels = labels_is
)
```

For convenience, you can define your own preset helper:

``` r
report_labels_is <- function() {
  birtir::report_labels(
    table = "Tafla",
    figure = "Mynd"
  )
}
```

## Convert Markdown

Use `convert_md()` as a secondary step when you want to convert a
rendered Markdown report to another file format with Pandoc:

``` r
md_path <- birtir::render_analysis_md("scripts/regression_example.R")
birtir::convert_md(md_path, to = "docx")
```

`convert_md()` only accepts `.md` input and will fail clearly if Pandoc
is not installed or not available on `PATH`.

## Current limitations

- `birtir` is currently designed for one render at a time in a normal
  interactive R session.
- Rendering the same report name in parallel can still collide at the
  report-folder level.
- Analysis scripts can affect the current R session through `library()`,
  `options()`, random seeds, and globals.

## Design

- Plain Markdown only
- Output-first, not code-first
- Minimal syntax in scripts
- Explicit helpers for plots and tables
- Report-ready statistical descriptions with `describe_data()`
- Two layouts: standalone `"report"` or shared-assets `"book"`
- One helper API for both manual work and report rendering

## Statistical descriptions

`birtir` includes formula-based helpers for statistical descriptions:

``` r
desc <- mtcars |> birtir::describe_data(mpg ~ cyl)

birtir::as_report_text(desc)
birtir::as_report_table(desc)
```

Inside `render_analysis_md()`, those objects can be rendered with
`md_text()` and `md_table()`.

## Reiknir methods

The formula-description methods from the former `reiknir` work now live
in `birtir::describe_data()`. Use formulas to describe the shape of the
data before writing the report:

``` r
# One variable
mtcars |> birtir::describe_data(mpg ~ 1)

# Continuous variable by groups
mtcars |>
  transform(cyl = factor(cyl)) |>
  birtir::describe_data(mpg ~ cyl)

# Continuous relationships
mtcars |> birtir::describe_data(mpg ~ wt)
mtcars |>
  transform(cyl = factor(cyl)) |>
  birtir::describe_data(mpg ~ wt + cyl)

# Random-intercept-style grouped descriptions
mtcars |>
  transform(cyl = factor(cyl)) |>
  birtir::describe_data(mpg ~ (1 | cyl))
```

These description objects provide the same reporting surface:

``` r
desc <- mtcars |>
  transform(cyl = factor(cyl)) |>
  birtir::describe_data(mpg ~ cyl)

birtir::as_report_text(desc)
birtir::as_report_table(desc)
```

For compact console and Markdown summaries, the same workflow can use
inline helpers such as `inline_hist()`, `inline_boxplot()`,
`inline_bar()`, `inline_scatter()`, and `inline_forest()`.
