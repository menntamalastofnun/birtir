# reiknir Style Guide

This guide collects the package's design, output, visual, and code style decisions.

## Purpose

`reiknir` should feel clear, calm, and teachable. The package is for people thinking carefully about data, models, simulation, and uncertainty. Output should help users understand what is happening without making the console noisy.

`reiknir` should also be a good friend to `birtir`: `reiknir` creates statistical objects, summaries, text, tables, and plots; `birtir` turns ordinary R scripts into Markdown reports.

## Code Style

- Follow tidyverse style for R code.
- Use clear function names with verbs: `describe_data()`, `compare_data()`, `check_model_data()`.
- Use tidy naming conventions: lowercase words separated by underscores.
- Prefer names that are readable but not verbose. `desc()` is too short; `describe_the_data()` is too long.
- Prefer explicit arguments over hidden inference when the argument carries a statistical assumption.
- Keep functions small enough that each function has one job.
- Use roxygen2 documentation for exported functions.
- Use tests for parsing, type validation, summaries, and printed output.

Decision: use tidyverse style, but add strict tooling after the first core functions exist.

Pros:

- Code will look familiar to R users.
- `styler` and `lintr` can automate many decisions.
- Consistent naming and formatting make the package easier to teach and review.

Cons:

- Strict linting too early can slow exploratory work.
- Some tidyverse rules are stylistic rather than statistical or functional.
- Tooling should support development, not become the work itself.

Recommended tools:

- `styler::style_pkg()` for formatting.
- `lintr::lint_package()` for linting.
- `testthat` for tests.

## Data Type Style

Variable types should be declared in the data by R classes:

- continuous variables: `numeric` or `integer`
- categorical variables: `factor`
- ordinal variables: `ordered factor`
- binary/logical variables: `logical` or two-level `factor`
- dates: `Date`, `POSIXct`, or `POSIXlt`

`reiknir` should validate these classes and explain them. It should not silently reinterpret a user's data.

Character variables should usually trigger a message asking the user to convert them to `factor` if they are categorical.

The package can be clever, but it should be clear when it is being clever. If a choice changes statistical meaning, `reiknir` should ask the user to fix or clarify the data rather than silently guessing.

## Formula Style

Formula syntax is the main user interface:

```r
df |> describe_data(x ~ 1)
df |> describe_data(y ~ x)
df |> describe_data(y ~ x + g)
df |> describe_data(y ~ x * g)
```

The formula describes the relationship to summarize or model. It should not hide data type or distribution assumptions.

Use data-first function signatures when pipeability matters. The preferred shape is:

```r
describe_data(data, formula, ...)
```

so users can write:

```r
df |> describe_data(y ~ x)
```

Interaction syntax should follow ordinary R formula rules:

- `y ~ x:z` means an interaction term.
- `y ~ x * z` means `y ~ x + z + x:z`.

Printed output should explain interactions as conditional relationships in plain language.

## Model Style

When `reiknir` fits models, `glm()` is the basic workhorse.

The user should state the distribution family:

```r
model_data(y ~ x + g, data = df, family = gaussian())
model_data(y ~ x + g, data = df, family = binomial())
model_data(count ~ x + g, data = df, family = poisson())
```

The formula declares structure. Data classes declare variable types. The `family` declares the outcome distribution and link function.

## Console Output

Output should be readable first, decorative second.

- Output should be descriptive and teachable, but compact enough that the main print method fits on about half the screen of a common laptop.
- Use black or near-black text for ordinary output.
- Use color sparingly for status, warnings, and emphasis.
- Use icons or symbols only when they add meaning. Do not make output emoji-first.
- Keep headings short.
- Prefer aligned tables or tibbles for summaries.
- Do not print large objects by default.
- Explain assumptions and validations in plain language.
- Use `cli` for messages, warnings, and structured console output.

Suggested message roles:

- success: something was validated or completed
- info: assumption or interpretation
- warning: user should inspect or change something
- error: package cannot continue safely

Warnings should be direct and instructional. They should say what is wrong, why it matters, and what the user can do next.

Avoid overly chatty output. A good print method should make the next action obvious.

## Color Style

Use restrained, readable colors. Black text should remain the default.

Suggested semantic palette:

- text: `#1E1E1E`
- muted text: `#495057`
- simulation: `#1864AB`
- observed data: `#2B8A3E`
- description/formula: `#5F3DC4`
- comparison/checking: `#E67700`
- model/posterior: `#C2255C`
- warning: `#F08C00`
- error: `#C92A2A`

For plots, avoid relying on color alone. Use labels, facets, shapes, or line types where useful.

## Plot Style

Plots should be simple, model-relevant, and clean in an APA-ish style. Detailed plot design can evolve separately from the core package logic.

- Use readable black text.
- Use light grid lines or no grid when possible.
- Keep legends clear and close to the data.
- Return ggplot objects so users can modify them.
- Choose plot types from validated variable classes.
- Use the same color meanings across simulation, observed data, model checks, and posterior checks.

## Language Style

Package output, documentation, and examples should default to English.

Later, `reiknir` should support optional Icelandic labels/messages where useful, similar in spirit to the `birtir` package. The important idea from `birtir` is pragmatic: keep ordinary R usage simple, but centralize the user-facing words that appear in output.

Do:

- Keep internal object names and function names in English.
- Keep examples and primary documentation in English.
- Store user-facing labels in small translation tables or label objects.
- Use centralized helpers for labels, similar in spirit to `birtir::report_labels()`.
- Pass label objects into output-producing functions when useful.
- Let users create any language preset they want.
- Keep tests for both English and Icelandic text if translation becomes part of the package.

Do not:

- Translate function names.
- Scatter Icelandic/English conditionals throughout the codebase.
- Make every message bilingual by default.
- Let language support make the ordinary workflow harder to read.

Preferred interface:

```r
labels_is <- reiknir_labels(
  table = "Tafla",
  figure = "Mynd",
  variable = "Breyta",
  model = "Líkan"
)

df |> describe_data(x ~ 1, labels = labels_is)
plot(description, labels = labels_is)
```

Users can define their own presets:

```r
reiknir_labels_is <- function() {
  reiknir_labels(
    table = "Tafla",
    figure = "Mynd",
    variable = "Breyta",
    model = "Líkan"
  )
}

df |> describe_data(x ~ 1, labels = reiknir_labels_is())
```

The exact function names can change. The design principle is that language belongs in a thin output layer, not inside the statistical logic. A `language = "is"` convenience option can come later, but the label-object pattern should be the base because it is more flexible and matches `birtir`.

## Documentation Style

Documentation should teach the workflow:

1. Declare variable types in the data.
2. Describe with formulas.
3. Simulate when useful.
4. Compare simulated and observed data.
5. Fit a model with an explicit family.
6. Check the model or posterior against the data.

Examples should be small, realistic, and runnable. Start with simple simulated data, for example:

```r
df <- data.frame(
  x = rnorm(100, 50, 10)
)

df |> describe_data(x ~ 1)
```

Then add richer examples as the package grows.

## Friend Package Style

`birtir` owns report rendering. `reiknir` should not duplicate `render_analysis_md()`, `md_table()`, `md_plot()`, or `md_text()`.

Instead, `reiknir` should return objects that are easy for `birtir` to render:

- print methods for interactive use
- `as_report_text()` for prose
- `as_report_table()` for tables
- `plot()` methods that return ggplot objects

The two packages should meet through ordinary R scripts:

```r
desc <- df |> reiknir::describe_data(x ~ 1)

birtir::md_text(reiknir::as_report_text(desc))
birtir::md_table(reiknir::as_report_table(desc), caption = "Description")
birtir::md_plot(plot(desc), caption = "Distribution")
```

Keep this relationship loose unless a stronger integration proves necessary.

## Where Enforceable Style Lives

This file is the human-readable guide.

Enforceable style should eventually live in:

- `.lintr`: linting rules
- `.styler.R`: formatting preferences
- `R/palette.R`: package colors
- `R/theme-reiknir.R`: ggplot theme
- `R/cli-theme.R`: console output conventions
- tests: printed output, warnings, and errors
