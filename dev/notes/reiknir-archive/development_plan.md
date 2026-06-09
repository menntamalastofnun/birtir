# reiknir Development Plan

This plan reflects the current direction after discussion: the first version of `reiknir` should focus on describing data using formula notation. 
## Core Direction

The initial package identity is:

> Use familiar R formula syntax to describe variables, relationships, and model-relevant data structure before and after modeling.

The core workflow starts with formulas and can begin from either simulated data or observed data:

```r
df |> describe_data(x ~ 1)
df |> describe_data(y ~ x)
df |> describe_data(y ~ x + g)
df |> describe_data(y ~ x * g)
```

Conceptually:

```text
simulation -> simulated data -> description -> model -> posterior/checks
real world  -> observed data  -> description -> model -> posterior/checks
```

The same descriptive machinery should work for both paths. A simulated dataset and a real dataset are both data-like objects, but simulated data can also carry known truth: parameter values, data-generating assumptions, and expected patterns.

The formula is not only a model specification. In `reiknir`, it is a compact way to ask:

- What kind of variables are involved?
- What does each variable look like?
- What kind of relationship is implied?
- Are there interaction terms, meaning one relationship may depend on another variable?
- What summaries and plots are appropriate?
- What should we check before fitting a model?
- If this data was simulated, does it recover the known structure?
- If this data was observed, does it resemble what our simulation said was plausible?

DAG support remains important, but it should be a second layer:

```r
formula <- define_dag(...) |>
  to_formula(outcome = "y", exposure = "x")

df |> describe_data(formula)
```

That means v0.1 should be excellent without requiring a DAG.

## Audience

The first users are:

- You as the first power user.
- Your team.
- Students learning model-based statistics.

This matters because the package should be explicit, readable, and teachable. It should not hide statistical assumptions behind clever automation.

## Design Principles

- Formula notation is the main interface.
- Description comes before modeling.
- Variable types should be explicit in the data, not silently guessed. Continuous variables should be numeric. Categorical variables should be factors. Ordinal variables should be ordered factors.
- `reiknir` should validate and explain variable types, but it should not quietly reinterpret a user's data.
- When `reiknir` fits models, `glm()` should be the basic workhorse. The user should state the outcome distribution through the `family` argument.
- Simulation is a first-class path. The package should help users simulate from assumptions, describe the simulated data, then compare those expectations with real data.
- `birtir` is the friend package for reporting. `reiknir` should create clean objects, tables, text, and ggplots that `birtir` can render.
- Model fitting can remain external when useful. Users may call `lm()`, `lme4::lmer()`, or `brms::brm()` directly.
- The package should help with what happens before and after the model: description, checking, summarizing, plotting, and interpreting model-relevant structure.
- Structured result objects are useful, especially because the same workflow will repeat across formula types and model families.
- Keep structure efficient. Use the smallest object system that gives reliable print, plot, tidy, and extension behavior.

## Object System Recommendation

Use simple S3 objects for the first working version unless S7 starts paying for itself immediately.

Reason:

- S3 is lighter and fits common R package workflows.
- `print()`, `plot()`, `summary()`, `tidy()`, and `augment()` are straightforward.
- The object design can still be disciplined.
- S7 can be introduced later if validation and formal class contracts become valuable.

Suggested first classes:

- `reiknir_description`
- `reiknir_univariate`
- `reiknir_bivariate`
- `reiknir_multivariable`
- `reiknir_simulation`
- `reiknir_data_comparison`
- `reiknir_model_check`
- `reiknir_dag`, later
- `reiknir_formula`, later if DAG metadata becomes important

## Current Code Notes

- `R/create_models.R` currently contains useful exploratory code for `formula_to_latex()`, DAG parsing, DAG construction, and formula derivation.
- For the new package direction, split out and stabilize `formula_to_latex()` first.
- DAG code should be kept, but moved behind the first description layer.
- Functions currently use unqualified helpers such as `f_lhs()`, `list2()`, `map()`, `list_rbind()`, `glue()`, and `dagitty()`. Either import these deliberately or use explicit namespaces.
- `parse_dag_formula()` should eventually reject unsupported formula features early.

## Milestones

### M0: Package Hygiene

Goal: make the package installable, documented, and testable.

- Fill in `DESCRIPTION`: title, author, license, package description.
- Choose initial dependencies.
- Add `testthat`.
- Add roxygen documentation.
- Decide exported function names for v0.1.

Suggested initial dependencies:

- `rlang`
- `cli`
- `glue`
- `tibble`
- `dplyr`
- `purrr`

Suggested later or optional dependencies:

- `ggplot2`
- `broom`
- `moments`
- `lme4`
- `brms`
- `dagitty`

Done when:

- `devtools::load_all()` works.
- `devtools::test()` runs.
- Public functions have roxygen stubs.

### M1: Formula Description Grammar

Goal: define exactly what each formula shape means for descriptive statistics.

Implement `classify_formula()`:

- `x ~ 1`: univariate description.
- `y ~ x`: bivariate relationship.
- `y ~ x + z`: multivariable description.
- `y ~ x:z`: interaction-only description.
- `y ~ x * z`: main effects plus interaction, equivalent to `y ~ x + z + x:z`.
- `y ~ x | g`: optional grouped description, if we choose to support this syntax.
- `y ~ x + (1 | g)`: model-oriented mixed structure, probably later.

Implement formula helpers:

- `formula_lhs()`
- `formula_rhs_terms()`
- `formula_variables()`
- `formula_to_latex()`
- `formula_interactions()`
- `has_intercept()`
- `has_random_effect()`

Done when:

- Formula behavior is documented.
- Unsupported formula features fail clearly.
- Tests cover `x ~ 1`, `y ~ x`, `y ~ x + z`, `y ~ x:z`, `y ~ x * z`, transformations, and missing LHS/RHS cases.

### M2: Variable Type Validation

Goal: use the data's R classes as explicit declarations of variable type.

Implement `validate_variable_types()` or `read_variable_types()`:

- numeric or integer: continuous by default
- factor: categorical/nominal
- ordered factor: ordinal
- logical: binary/logical
- Date/POSIXct/POSIXlt: date/time
- character: invalid or requires conversion to factor

The package can still report what it sees, but it should not silently decide that a numeric variable is really a grouping variable.

Expected user behavior:

```r
df$group <- factor(df$group)
df$rating <- ordered(df$rating)
df$score <- as.numeric(df$score)
```

Done when:

- Type declarations are stored in the returned object.
- Character variables produce helpful messages asking the user to convert them.
- Numeric variables with very few unique values produce a note, not an automatic conversion.
- Ordered factors are treated differently from unordered factors.

### M3: Core `describe_data()`

Goal: make the first package feature genuinely useful.

Implement:

```r
describe_data(data, formula, types = NULL, na_rm = FALSE)
```

Data comes first so users can write native-pipe workflows:

```r
df |> describe_data(x ~ 1)
df |> describe_data(y ~ x + g)
```

Return structured objects with:

- original formula
- variables used
- declared/validated variable types
- univariate summaries
- relationship summaries
- missingness summaries
- notes or warnings

Initial summaries:

- continuous: n, missing, mean, sd, median, IQR, min, max, skew if available
- nominal/binary: n, missing, counts, proportions, balance
- ordinal: counts, proportions, median/range if meaningful
- date/time: range, gaps if useful

Relationship summaries:

- continuous-continuous: correlation, slope from simple regression, rough strength label
- continuous-group: group means, group SDs, mean differences
- group-group: contingency table, proportions
- binary-continuous: group summaries or logistic-oriented summaries
- interaction terms: describe the main variables and report that the relationship is conditional

Interaction summary strategy:

- `y ~ x * g`, where `g` is a factor: summarize `y ~ x` within each level of `g`.
- `y ~ x * z`, where `z` is continuous: start with a clear message that the interaction is continuous-by-continuous and needs conditional summaries, such as low/typical/high values of `z`.
- `y ~ g1 * g2`, where both are factors: summarize cells for each factor combination.

- `df |> describe_data(x ~ 1)` works.
- `df |> describe_data(y ~ x)` works across common type combinations.
- `df |> describe_data(y ~ x + g)` gives useful variable and relationship summaries.
- Interaction formulas are recognized and reported clearly, even if the first interaction summaries are conservative.
- Print output is clear enough for students.

### M4: Birtir Compatibility Surface

Goal: make `reiknir` outputs easy to render with `birtir`.

The packages should remain independent:

- `reiknir` should not duplicate `birtir::render_analysis_md()`.
- `birtir` should not need to know statistical details about `reiknir`.
- The handoff should use ordinary objects.

Possible helpers:

```r
as_report_text(desc)
as_report_table(desc)
plot(desc)
```

Then a `birtir` report can use:

```r
desc <- df |> reiknir::describe_data(x ~ 1)

birtir::md_text(reiknir::as_report_text(desc))
birtir::md_table(reiknir::as_report_table(desc), caption = "Description")
birtir::md_plot(plot(desc), caption = "Distribution")
```

Done when:

- Description objects can produce report-ready text.
- Description objects can produce report-ready tables.
- Plot methods return ggplot objects.
- `reiknir_labels()` is compatible with the label object style used by `birtir::report_labels()`.
- A small example script can be rendered with `birtir::render_analysis_md()`.

### M5: Simulation Layer

Goal: support simulated data as a first-class starting point.

At first, do not build a huge simulation language. Start with small helpers that make simulation explicit and comparable to observed data.

Possible interfaces:

```r
sim <- simulate_data(
  n = 200,
  formula = y ~ x + g,
  family = gaussian(),
  parameters = list(
    intercept = 0,
    x = 0.5,
    g = c(a = 0, b = 1),
    sigma = 1
  )
)

sim |> describe_data(y ~ x + g)
compare_data(y ~ x + g, simulated = sim, observed = df)
```

Alternative lighter first step:

```r
sim <- as_simulation(sim_df, truth = list(beta_x = 0.5, sigma = 1))
sim |> describe_data(y ~ x + g)
```

Recommended v0.1 approach:

- First support user-created simulated data through `as_simulation()`.
- Store known truth and assumptions as metadata.
- Reuse `describe_data()` for simulated and observed data.
- Add `compare_data()` to compare simulated and observed summaries.
- Add full `simulate_data()` later once the descriptive layer is stable.

Done when:

- Simulated data can carry known truth.
- `describe_data()` works the same way for simulated and observed data.
- `compare_data()` can show whether observed summaries look plausible under the simulation.

### M6: Plot Methods

Goal: make descriptions visual without forcing users into a plotting grammar.

Implement:

```r
plot(df |> describe_data(...))
```

Likely plots:

- histogram/density for continuous variables
- bar chart for categorical variables
- scatterplot for continuous-continuous relationships
- box/violin/dot plot for continuous-group relationships
- mosaic or proportion plot for categorical relationships

Done when:

- Plot methods return ggplot objects.
- The plot choice follows declared and validated variable types.
- Users can still modify plots with ggplot syntax.
- Simulated and observed data can be compared visually where useful.

### M7: GLM Modeling and Posterior/Post-Model Helpers

Goal: make `glm()` the core modeling workhorse when `reiknir` fits models, while still supporting models users fit explicitly. For Bayesian models, support posterior-oriented summaries and checks.

The modeling interface should require or strongly encourage an explicit family:

```r
model_data(y ~ x + g, data = df, family = gaussian())
model_data(y ~ x + g, data = df, family = binomial())
model_data(count ~ x + g, data = df, family = poisson())
```

Why:

- Formula declares the structural relationship.
- Data classes declare variable types.
- `family` declares the outcome distribution and link function.

Also support already-fit model objects:

```r
fit <- glm(y ~ x + g, data = df, family = gaussian())
check_model_data(fit, data = df)
describe_model(fit)
```

Possible functions:

- `model_data()`: fit a GLM with an explicit family.
- `check_model_data()`: compare fitted model formula to data description.
- `describe_model()`: compact model summary with readable interpretation.
- `augment_description()`: add fitted values/residuals summaries if available.
- `describe_posterior()`: summarize posterior draws when available.
- `posterior_predict_check()`: compare observed data to posterior predictive simulations when available.

Later:

- `lme4::lmer`
- `brms::brm`

Done when:

- `model_data()` uses `glm()` internally.
- The selected family is stored and printed.
- The package warns when data types and family look inconsistent.
- Users can also fit models however they prefer.
- `reiknir` can still describe and check the data/model relationship.
- Bayesian fits can enter the same posterior/checking workflow without becoming mandatory.
- No heavy modeling dependency is required for core use.

### M8: DAG Layer

Goal: add causal structure once formula-based description is stable.

Implement:

- `define_dag(...)`
- `to_formula()`
- DAG-aware metadata on formulas or descriptions
- adjustment set reporting

Defer unresolved design questions:

- What should happen when there are multiple adjustment sets?
- Should `to_formula()` auto-select one or return alternatives?
- How should users choose among statistically valid but substantively different adjustment sets?

Done when:

- DAG-derived formulas work with `describe_data()`.
- Empty valid adjustment sets are handled correctly.
- Unidentifiable effects produce helpful errors.

### M9: Teaching Materials and v0.1

Goal: make the package usable by you, your team, and students.

- Write `README.md` around formula-driven data description.
- Include the two-path workflow: simulated data and observed data.
- Include a short example showing `reiknir` output rendered through `birtir`.
- Add a short "Getting started" vignette.
- Add examples using small built-in datasets.
- Add examples showing pre-model and post-model use.
- Add one example where simulated data defines expectations before real data is analyzed.
- Keep DAG material as an advanced vignette or v0.2 preview.

Done when:

- A new user understands the package from the README.
- All examples run.
- Tests cover the central formula/type combinations.

## Suggested Initial File Layout

Start small:

```text
R/
  helpers.R
  describe-data.R
  describe-summary.R
  simulation.R
  compare-data.R
  methods-print.R
  methods-plot.R
tests/testthat/
  test-helpers.R
  test-describe-data.R
  test-simulation.R
```

One `helpers.R` file is enough at the beginning. Split it later into more specific files like `formula-utils.R` or `variable-utils.R` only when the file becomes hard to navigate or the boundaries become obvious.

Add later:

```text
R/
  model-check.R
  dag-utils.R
  define-dag.R
  to-formula.R
tests/testthat/
  test-model-check.R
  test-dag.R
```

## Immediate Next Steps

1. Update `DESCRIPTION`.
2. Create tests for `classify_formula()`.
3. Move the useful formula/type helpers from `ideas.R` into `R/helpers.R`.
4. Implement variable type validation from R classes.
5. Implement the first version of `df |> describe_data(x ~ 1)`.
6. Extend to `df |> describe_data(y ~ x)`.
7. Add `as_simulation()` for user-created simulated data.
8. Add print methods.

## Open Questions

1. Should grouped descriptions use ordinary formula syntax like `y ~ g`, or should we also support a conditioning syntax such as `y ~ x | g`?
2. Should `describe_data(y ~ x + g)` summarize all pairwise relationships with `y`, or also relationships among predictors?
3. How opinionated should the printed interpretation be? For example, should it say "weak positive relationship", or only report statistics?
4. Should plots be part of the first working milestone, or come immediately after text summaries?
5. Should `model_data()` require `family`, or default to `gaussian()` with a clear message?
6. Should `simulate_data()` be part of v0.1, or should v0.1 only support simulated data that the user creates?
7. For continuous-by-continuous interactions, should summaries use low/mean/high values, quantiles, or user-specified values?
