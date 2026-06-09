# reiknir Project Status

Date: 2026-05-26

## Short Status

The project is ready to start coding the first core milestone.

The main package direction is now clear enough:

- Start with formula-based data description.
- Use explicit R classes as data type declarations.
- Treat simulation and observed data as parallel inputs.
- Use `glm()` with explicit `family` when `reiknir` fits models.
- Keep DAGs as a later causal layer.
- Keep `birtir` as the report-rendering friend package.

The main thing to fix before or during the first coding pass is documentation alignment: `project_draft.md` still describes the older DAG-first and S7-first idea, while `development_plan.md`, `STYLEGUIDE.md`, and `BIRTIR_INTEGRATION.md` describe the newer direction.

## Current Artifacts

- `project_draft.md`: original draft. Useful historically, but partly outdated.
- `development_plan.md`: current roadmap and milestone plan.
- `STYLEGUIDE.md`: current style, output, language, plot, and naming guide.
- `BIRTIR_INTEGRATION.md`: package friendship contract with `birtir`.
- `workflow.excalidraw`: visual workflow showing simulation and real data paths.
- `R/create_models.R`: exploratory code for formula-to-LaTeX and DAG helpers.

## Settled Decisions

### Package Identity

`reiknir` starts as a formula-driven data description package.

The first useful workflow is:

```r
df |> describe_data(x ~ 1)
df |> describe_data(y ~ x)
df |> describe_data(y ~ x + g)
df |> describe_data(y ~ x * g)
```

DAG-to-formula workflows are important, but not the first layer.

### Data Types

Variable types should be declared by R classes:

- `numeric` or `integer`: continuous
- `factor`: categorical
- `ordered factor`: ordinal
- `logical` or two-level factor: binary/logical
- `Date` / `POSIXct` / `POSIXlt`: date/time

`reiknir` should validate types and explain them. It should not silently convert a numeric variable into a grouping variable.

### Simulation

Simulation is a first-class path:

```text
simulation -> simulated data -> description -> model -> posterior/checks
real world  -> observed data  -> description -> model -> posterior/checks
```

The recommended first implementation is `as_simulation()`, not a full `simulate_data()` language.

### Modeling

When `reiknir` fits models, `glm()` should be the workhorse.

The formula declares structure. Data classes declare variable types. `family` declares the outcome distribution and link.

Recommended:

```r
model_data(y ~ x + g, data = df, family = gaussian())
```

Do not center v0.1 around replacing `lm()`, `lmer()`, or `brms()`.

### Output Style

Printed output should be descriptive but compact enough to fit on about half of a common laptop screen.

Use black or near-black text by default. Use symbols only when they add meaning. Warnings should be direct and instructional.

### Naming

Use tidyverse-style names:

- verb-based
- lowercase
- underscore-separated
- readable but not verbose

Good examples:

- `describe_data()`
- `compare_data()`
- `check_model_data()`

Avoid:

- `desc()`
- `describe_the_data()`

### Language and Labels

Default package output and docs should be English.

Optional Icelandic output should use label objects, similar to `birtir::report_labels()`, rather than translated function names or scattered language conditionals.

### birtir Integration

`reiknir` should not render reports. `birtir` owns report rendering.

`reiknir` should return objects that are easy for `birtir` to render:

- `as_report_text()`
- `as_report_table()`
- `plot()`

## Recommended First Coding Milestone

Build the smallest useful version of formula-based description.

1. Update `DESCRIPTION`.
2. Move useful exploratory code out of `ideas.R` and `R/create_models.R`.
3. Implement formula helpers:
   - `formula_lhs()`
   - `formula_rhs_terms()`
   - `formula_variables()`
   - `formula_interactions()`
   - `classify_formula()`
4. Implement variable type validation:
   - `read_variable_types()` or `validate_variable_types()`
5. Implement:
   - `df |> describe_data(x ~ 1)`
6. Add S3 print methods.
7. Add tests for formula parsing and type validation.

This is enough to start.

Start with one `R/helpers.R` file for formula and type helpers. Split into more specific files only when the helper file becomes too broad.

## Recommended Technical Decisions

These are not all permanently settled, but they are good enough to begin.

### Object System

Use S3 first.

Reason: it is lighter, faster to build, and fits the print/plot/report workflow. S7 can come later if formal validation becomes worth the extra structure.

### Formula Scope

Start with:

- `x ~ 1`: univariate
- `y ~ x`: bivariate
- `y ~ x + g`: multivariable, outcome-focused
- `y ~ x:z`: interaction-only
- `y ~ x * g`: main effects plus interaction, outcome-focused

Do not support `y ~ x | g` in the first pass unless the need appears quickly.

For interactions, parse them early. Rich summaries can be conservative at first:

- factor interaction: summarize within factor levels or cells
- continuous interaction: report that the relationship is conditional and needs chosen values

### Relationship Summaries

For `y ~ x + g`, start by summarizing relationships between the outcome and each predictor:

- `y` with `x`
- `y` with `g`

Do not summarize all predictor-predictor relationships by default. Add that later through an argument if needed.

Possible future argument:

```r
df |> describe_data(y ~ x + g, scope = "outcome")
df |> describe_data(y ~ x + g, scope = "all")
```

### `model_data()` Family

Require `family` when `model_data()` is implemented.

This matches the package philosophy: distribution assumptions should be explicit.

### Simulation

Start with:

```r
sim <- as_simulation(sim_df, truth = list(...))
```

Defer:

```r
simulate_data(...)
```

until formula description and comparison are stable.

## Open Decisions

These are the decisions that still need your input, but none of them block starting M0/M1.

1. Should `project_draft.md` be rewritten to match the new direction, or kept as the original historical draft?
2. Should the first object system be officially S3, with S7 deferred?
3. Should `describe_data(y ~ x + g)` be outcome-focused by default? Recommendation: yes.
4. Should `y ~ x | g` grouped/conditional syntax be deferred? Recommendation: yes.
5. How much interpretation should print methods include? Recommendation: conservative labels plus numbers.
6. Should plots be built immediately after text summaries, or only after the core summary objects are stable? Recommendation: after text summaries.
7. Should `model_data()` require `family`? Recommendation: yes.
8. Should v0.1 include only `as_simulation()`, not `simulate_data()`? Recommendation: yes.
9. For continuous-by-continuous interactions, should summaries use low/mean/high values, quantiles, or user-specified values?

## Not Ready Yet

The package is not yet ready for use because:

- `DESCRIPTION` still has placeholder metadata.
- There are no tests.
- There is no implemented `describe_data()`.
- Existing code is exploratory and DAG-heavy.
- Imports are not declared.
- `project_draft.md` conflicts with the newer plan.

## Ready To Start?

Yes.

The project is ready to start implementation if we treat the first target as:

> Build formula parsing, variable type validation, and `df |> describe_data(x ~ 1)`.

That first target does not require deciding DAG behavior, brms support, report rendering, or full simulation syntax.
