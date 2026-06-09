# reiknir and birtir

`reiknir` and `birtir` should be friend packages with different jobs.

## Roles

`reiknir` is for statistical thinking:

- describe data with formula syntax
- validate variable types
- work with simulated and observed data
- compare simulated and observed summaries
- fit or check models
- summarize model and posterior checks

`birtir` is for reporting:

- run ordinary `.R` scripts
- capture printed output
- save tables
- save plots
- write plain Markdown reports
- convert Markdown to other formats

The friendship rule:

> `reiknir` should create clean R objects. `birtir` should render those objects.

## Integration Principles

- Do not make `reiknir` a report renderer.
- Do not make `birtir` know statistical details about `reiknir`.
- Keep both packages useful on their own.
- Prefer ordinary R objects: data frames, tibbles, character text, and ggplot objects.
- Avoid a hard dependency if possible. `reiknir` can support `birtir` through optional helpers and examples.
- Put language and labels in output layers, not statistical logic.

## Shared Label Objects

`birtir::render_analysis_md()` accepts a label object with at least:

```r
table = "Table"
figure = "Figure"
```

`reiknir` labels should stay compatible with that shape, while adding labels needed by `reiknir`.

Possible interface:

```r
labels_is <- reiknir_labels(
  table = "Tafla",
  figure = "Mynd",
  variable = "Breyta",
  model = "Líkan",
  simulation = "Hermun",
  observed = "Raungögn"
)

birtir::render_analysis_md(
  script = "scripts/describe_example.R",
  labels = labels_is
)
```

This works best if `reiknir_labels()` returns an ordinary named list. Then `birtir` can use `table` and `figure`, while `reiknir` can use the extra fields.

## Reporting Surface

`reiknir` objects should expose report-friendly pieces.

Possible helpers:

```r
as_report_text(desc)
as_report_table(desc)
plot(desc)
```

These should return ordinary objects:

- `as_report_text()` returns a character string or character vector.
- `as_report_table()` returns a tibble/data frame.
- `plot()` returns a ggplot object.

Then a `birtir` script can be plain and readable:

```r
#| h1: Data description

library(reiknir)
library(birtir)

df <- data.frame(
  x = rnorm(100, 50, 10)
)

desc <- df |> describe_data(x ~ 1)

md_text(as_report_text(desc))
md_table(as_report_table(desc), caption = "Description of x", digits = 2)
md_plot(plot(desc), caption = "Distribution of x")
```

## Formatting

`birtir` already has:

- `fmt_num()`
- `fmt_p()`
- `md_text()`
- `md_table()`
- `md_plot()`

`reiknir` should not duplicate report rendering. For formatted prose, `reiknir` can either:

- return raw numbers in tables and let `birtir::md_table()` handle rounding
- use compatible formatting conventions for console output
- optionally use `birtir` formatting helpers in examples when `birtir` is installed

## Suggested Package Relationship

For v0.1:

- `reiknir` should not import `birtir`.
- `birtir` should not import `reiknir`.
- Add examples showing how they work together.
- Add `birtir` to `Suggests` only if tests or examples need it.

Later:

- Add optional tests that render a small `reiknir` analysis through `birtir`.
- Consider a small helper like `as_birtir_report()` only if repeated usage proves it is needed.

## What Not To Copy From birtir

Do not copy the renderer into `reiknir`. `render_analysis_md()` belongs in `birtir`.

Do not copy the legacy plotting workflow. The legacy Icelandic helpers in `birtir` are useful historical context, but `reiknir` should build its own formula-based description and checking workflow.

Do not copy output state management. `birtir` owns render state; `reiknir` should just return objects.

## First Compatibility Target

The first compatibility target should be this script:

```r
#| h1: reiknir and birtir example

library(reiknir)
library(birtir)

labels_is <- reiknir_labels(
  table = "Tafla",
  figure = "Mynd",
  variable = "Breyta"
)

df <- data.frame(
  x = rnorm(100, 50, 10)
)

desc <- df |> describe_data(x ~ 1, labels = labels_is)

md_text(as_report_text(desc))
md_table(as_report_table(desc), caption = "Lýsing á breytu", digits = 2)
md_plot(plot(desc), caption = "Dreifing breytu")
```

Rendered with:

```r
birtir::render_analysis_md(
  script = "scripts/reiknir_birtir_example.R",
  labels = labels_is
)
```

That is enough friendship for the first milestone.
