# Reiknir Integration & Style Guide

This document consolidates the design history, integration principles, and style guidelines from the former `reiknir` package, which has been integrated into `birtir`.

## 1. Overview & Friendship Rule

`reiknir` (statistical data descriptions) and `birtir` (report rendering) are designed as companion layers with distinct responsibilities:

> **The Friendship Rule**: Statistical code should create clean R objects. Reporting code (`birtir`) should render those objects.

### Core Principles
- **No report rendering in stats**: The statistical description logic should not format markdown files directly.
- **No statistical details in renderer**: `birtir`'s rendering core should not need to understand statistical distributions or complex formula logic.
- **Clean interface**: Communication between layers is done via ordinary R objects: data frames, tibbles, character vectors, and `ggplot` objects.
- **Separation of language**: Grouping and formatting labels are defined at the output/rendering layer, not embedded deep within the stats logic.

---

## 2. Core API Design

### Formula-based Descriptions
Formula notation is the primary user interface to specify variable roles and relationships:
```r
df |> describe_data(y ~ x)
```
- **Data-first signatures**: All key statistical helpers use the signature `helper(data, formula, ...)` to remain fully pipeable.
- **Explicit types**: Variable types must be declared explicitly in R classes (`numeric`, `factor`, `ordered factor`, `logical`). The package should not quietly guess or coerce types (e.g., character vectors are not automatically treated as factors; they should raise a warning).

### Reporting Surface
Statistical description objects expose a standard reporting surface:
- `as_report_text()`: Returns a plain text/character string.
- `as_report_table()`: Returns a structured data frame/tibble.
- `plot()`: Returns a `ggplot` object.

This allows clean integration in a rendering script:
```r
library(birtir)

desc <- mtcars |> describe_data(mpg ~ cyl)

md_text(as_report_text(desc))
md_table(as_report_table(desc), caption = "MPG description")
```

---

## 3. Console & Output Style

### Restrained Console Visuals
Output should be clear, readable, and functional.
- **Semantic Palette**:
  - Text: Near-black (`#1E1E1E`) / Muted text (`#495057`)
  - Warning: Orange/Yellow (`#F08C00`)
  - Error: Red (`#C92A2A`)
- **Compactness**: Default print methods should fit within a reasonable console viewport.
- **Validation errors**: Use `cli::cli_abort()` for warnings and errors rather than silent coercions or raw `stopifnot()` calls.

### Inline Visualization Helpers
For compact console summaries, `birtir` supports text-based terminal visualizations:
- `inline_hist(x)`: Unicode vertical bars histogram.
- `inline_boxplot(x)`: Horizontal Tukey boxplot with whisker and outlier markers.
- `inline_bar(values)`: Aligned horizontal bar representation for frequency tables.
- `inline_scatter(x, y)`: Compact dot-matrix text scatter plot.
- `inline_forest(estimate, ci_low, ci_high)`: Aligned dot-and-whisker coefficient plot.
