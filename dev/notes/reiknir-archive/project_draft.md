# reiknir

**An R Package for Model-Based Statistics**

*Project Draft — v0.1*

---

## Philosophy

reiknir is built on the model-based statistics philosophy for example *Statistical Rethinking* (McElreath) and other new ideas in statisitcs -- model baised statistics. The core idea is simple: before fitting any model, you should describe the world as honestly as possible. This means making causal assumptions explicit, understanding your variables, and letting the data structure guide your analysis — not the other way around.

The package follows a four-step workflow: declare your causal assumptions as a Directed Acyclic Graph, or derive the right formula from that graph, describe the data, then fit the model. Each step is transparent about what it is doing and why.

---

## Core Workflow

| Step | Function | Question answered | Workhorse |
|---|---|---|---|
| 1. Causal structure | `define_dag()` | What causes what? | dagitty |
| 2. Identification | `to_formula()` | What must I condition on? | dagitty |
| 3. Description | `describe_data()` | What does the data look like? | dplyr, moments |
| 4. Model | `model_data()` | What does the model say? | lm, lme4, brms |

The entire workflow is designed to be pipeable using R's native pipe operator `|>` and folow tidyverse workflows and syling. Each function passes its result to the next, carrying DAG metadata throughout so later steps can reference and validate the causal assumptions.

---

## Example

### 1. Define the causal structure

Declare what causes what using standard R formula syntax. The `~` is reinterpreted as `←` (caused by):

```r
dag <- define_dag(
  y ~ x + g,    # y is caused by x and g
  x ~ g         # g also causes x (confounder)
)
```

This produces three directed edges: `x → y`, `g → y`, and `g → x`.

### 2. Derive the formula from the DAG

The DAG identifies the correct adjustment set automatically — you should never have to hand-write a formula:

```r
dag |> to_formula(outcome = "y", exposure = "x")

#> ✔ Formula derived from DAG: y ~ x + g
#> ℹ Adjustment set: {g}
#> ℹ Blocks backdoor: x ← g → y
```

### 3. Describe the data

`describe_data()` reads all variables in the formula, detects their types, and produces honest summaries before any model is fit:

```r
dag |>
  to_formula(outcome = "y", exposure = "x") |>
  describe_data(data = mydata)

#> Model: y_i = x_i + g_i + ε_i
#>
#> Variables:
#>   y  | continuous | n=100  mean=0.31  sd=2.1  skew=0.12
#>   x  | continuous | n=100  mean=0.04  sd=0.98
#>   g  | nominal    | levels: 0, 1  (balanced: 50/50)
#>
#> Relationships:
#>   y ~ x  | r = 0.18  (weak positive)
#>   y ~ g  | Δmean = 0.94  (group 1 higher)
```

### 4. Fit the model

```r
dag |>
  to_formula(outcome = "y", exposure = "x") |>
  describe_data(data = mydata) |>
  model_data(data = mydata)
```

---

## Formula Dispatch

The heart of the package is formula-based dispatch. The same function handles different analysis types depending on the formula structure and the detected type of each variable:

| Formula | `g` type | Analysis | Output class |
|---|---|---|---|
| `x ~ 1` | — | Univariate descriptives | `reiknir_univariate` |
| `x ~ g` | Nominal / ordinal | Group comparison | `reiknir_grouped` |
| `x ~ g` | Continuous | Covariation / correlation | `reiknir_covariation` |
| `y ~ x1 + x2` | — | Regression / GLM | `reiknir_model` |
| `y ~ x + (1|g)` | Group factor | Mixed effects | `reiknir_model` |

When `g` is ambiguous — for example a numeric variable that could be treated as continuous or as a grouping factor — the package detects the type from the data and tells the user what it assumed, with a suggestion to convert if needed. This is in the spirit of being explicit about assumptions.

---

## Statistical Notation

`describe_data()` prints the model equation in proper statistical notation. The `formula_to_latex()` utility converts R formulas automatically, with optional subscripts:

```r
formula_to_latex(y ~ x + g)
#> $y_i = x_i + g_i + \epsilon_i$

formula_to_latex(y ~ x + (1|g))
#> $y_i = x_i + u_{0g} + \epsilon_i$

formula_to_latex(y ~ x + g, subscript = FALSE)
#> $y = x + g + \epsilon$
```

---

## Directed Acyclic Graphs

DAG support is built on `dagitty`, the same engine McElreath uses in *Statistical Rethinking*. `define_dag()` converts R formulas into dagitty objects by parsing each formula as a set of directed edges (`lhs ← rhs` terms), then builds the dagitty string automatically.

```r
# Under the hood: define_dag() builds this dagitty string
# dag { x -> y; g -> y; g -> x }
```

This means the full dagitty engine is available: adjustment sets, backdoor path detection, implied conditional independencies, and testable implications. The reiknir wrapper just makes the syntax feel like the rest of the package.

**Key insight:** the DAG is the single source of truth. The formula is derived from it, not written separately. This prevents the common mistake of specifying a causal story in words and then fitting a model that contradicts it.

---

## Package Architecture

reiknir uses **S7** for result objects, giving formal class hierarchies and well-defined slots without the verbosity of S4. All result objects share a common set of generics:

- `print()` — clean, readable console output with `cli`
- `plot()` — ggplot2-based visualisations
- `tidy()` — broom-compatible tibbles for downstream use
- `augment()` — adds fitted values and residuals to the original data

Workhorse packages are soft dependencies (`Suggests`), with informative errors if missing:

- `lme4` / `lmerTest` — mixed effects models
- `brms` — Bayesian models
- `dagitty` — causal DAG analysis
- `moments` — skewness and kurtosis
- `psych` — item analysis

### File structure

```
reiknir/
│
├── R/
│   │
│   ├── core/                        # the engine — not user facing
│   │   ├── formula-utils.R          # classify_formula(), formula_to_latex()
│   │   ├── variable-utils.R         # classify_variable(), detect_type()
│   │   ├── dag-utils.R              # parse_dag_formula(), edges_to_dagitty()
│   │   └── check-utils.R           # check_pkg(), check_data(), check_formula()
│   │
│   ├── classes/                     # S7 class definitions only, no logic
│   │   ├── reiknir-dag.R            # reiknir_dag class
│   │   ├── reiknir-formula.R        # reiknir_formula class
│   │   ├── reiknir-description.R    # reiknir_univariate, _grouped, _covariation
│   │   └── reiknir-model.R          # reiknir_model class
│   │
│   ├── generics/                    # S7 methods — print, plot, tidy, augment
│   │   ├── print.R
│   │   ├── plot.R
│   │   └── tidy.R
│   │
│   ├── dag/                         # DAG functions
│   │   ├── define-dag.R             # define_dag()
│   │   └── to-formula.R             # to_formula()
│   │
│   ├── describe/                    # describe_data() and its workers
│   │   ├── describe-data.R          # main dispatcher
│   │   ├── describe-univariate.R    # x ~ 1
│   │   ├── describe-grouped.R       # x ~ g (nominal/ordinal)
│   │   └── describe-covariation.R   # x ~ g (continuous)
│   │
│   └── model/                       # model_data() and its workers
│       ├── model-data.R             # main dispatcher
│       ├── model-lm.R               # y ~ x1 + x2
│       ├── model-glm.R              # with family argument
│       └── model-mixed.R            # y ~ x + (1|g)
│
├── tests/
│   ├── testthat/
│   │   ├── test-formula-utils.R
│   │   ├── test-dag.R
│   │   ├── test-describe.R
│   │   └── test-model.R
│
├── vignettes/
│   ├── getting-started.Rmd          # the four-step workflow
│   └── dag-causal-inference.Rmd     # deeper DAG usage
│
├── man/                             # auto-generated by roxygen2
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

---

## Out of Scope for v0.1

Latent variable models (CFA, SEM, IRT) are intentionally excluded from the first version. The `lavaan` `=~` syntax requires a separate parsing path and a different conceptual frame. These can be added as a dedicated extension once the core is stable.

---

*This is a working draft. Function names, API design, and scope are subject to change.*