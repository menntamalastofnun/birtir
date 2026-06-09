## Ideas

### Ideas audit from `ideas.R`
- [ ] Add relationship labels for bivariate numeric summaries, e.g. `label_correlation()` for `y ~ x`. CX
- [ ] Add grouped difference labels, e.g. `label_group_diff()` for `x ~ g`. CX
- [x] Add skewness and kurtosis to `summarise_continuous()` without adding a heavy dependency. CX
- [x] Store an overall summary in grouped `describe_data()` objects and print it before factor-level summaries. CX
- [ ] Add a missing-data simulation/example helper inspired by the `mice::ampute()` scratch example, without taking a hard dependency unless needed. CX
- [x] dep: Do not automatically treat character variables as nominal; keep requiring explicit `factor()` conversion. Implemented in `read_variable_type()`. CX
- [x] dep: Do not bring back formula-first `describe_data(formula, data)`; keep the data-first pipeable API. Current API is `describe_data(data, formula, ...)`. CX
- [x] dep: Do not bring back `cat()` inside inline plot builders; keep S3 objects with print methods. Builders return objects/strings; `cat()` only appears in print methods. CX
- [x] dep: Do not add `dplyr`, `purrr`, `glue`, `psych`, or `mice` only for scratch helper ideas unless a later milestone clearly needs them. Current implementation avoids these dependencies. CX
- [x] dep: Scratch `classify_variable()` idea is deprecated because character variables must be explicit factors, not automatic nominal variables. CX
- [x] dep: Scratch `data_summary_function()` name is deprecated; production worker is `summarise_continuous()`. CX
- [x] dep: Scratch `formula_to_latex()` helper is replaced by production `formula_to_latex()` with mixed-effect support. CX
- [x] dep: Scratch `inline_hist()` and `inline_boxplot()` helpers are replaced by production public helpers with validation and print methods where needed. CX

## Don
- [x] boxplot have whiskers and values outside the whiskers (1.5IQR) are potential outliers * CX
- [x] Decide whether `inline_boxplot()` should show true Tukey whiskers, outlier points, or stay as a compact five-number summary. CX
- [x] Replace `stopifnot()` in public output helpers with `cli::cli_abort()` messages. CX
- [x] Add examples that combine `summarise_continuous()` with `inline_hist()`, `inline_boxplot()`, and `inline_bar()`. CX
- [x] Consider splitting formula/type/label helpers into separate files when `R/helpers.R` grows again. CX
- [x]  Should be justified -- if control and treatment are different length the line-up is not correct CX
	control │███████████████████▓  49.3
	treatment │███████████████████▓  49.3
- [x] Add `formula_to_latex()` or similar equation-output helper for formulas. CX-approved

- [x] Extend `formula_to_latex()` for mixed-effect formulas using lme4/brms-style random terms, starting with `(1 | g)`, `(x | g)`, `(1 + x | g)`, `(x || g)`, `(1 | g1) + (1 | g2)`, and `(1 | g1/g2)`. CX-researched
- [x] Add `notation = c("expanded", "matrix")` to `formula_to_latex()`: `expanded` should show an observation-level equation, while `matrix` should show the compact mixed-model form `Y | B = b ~ N(X beta + Z b, sigma^2 W^-1)` and `B ~ N(0, Sigma)`. CX-researched
- [x] For Gaussian mixed models, render examples like `Reaction ~ Days + (Days | Subject)` as a readable equation with fixed intercept/slope plus subject-level random intercept/slope terms. CX-researched
- [x] For GLMM/brms-style families, add `family` and `link` arguments so non-Gaussian models use linear predictor notation such as `g(mu_i) = eta_i = X_i beta + Z_i b` instead of an additive residual-error equation. CX-researched
- [x] Decide whether to write an internal random-effect parser or use an optional adapter to `lme4::findbars()` if `lme4` is installed; avoid making `lme4` a hard dependency unless mixed-formula parsing becomes too fragile. CX-researched
- [x] In `formula_to_latex.Rd`, document terminology: lme4 says fixed/random effects, while brms says population-level/group-level or varying effects; keep output wording neutral enough for both. CX-researched
- [x] Add Rd examples for mixed models: random intercept `(1 | group)`, correlated random intercept/slope `(1 + x | group)`, uncorrelated random intercept/slope `(x || group)`, and crossed grouping factors `(1 | subject) + (1 | item)`. CX-researched
