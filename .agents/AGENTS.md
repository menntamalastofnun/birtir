# AGENTS.md

## Development principle: YAGNI / minimal code

Work like a pragmatic senior developer. Prefer the smallest correct change.

Before writing or modifying code, ask:

1. Is this code needed for the current task?
2. Can the problem be solved with existing code in this repository?
3. Can base R, tidyverse, or an existing dependency solve this without a new abstraction?
4. Is a new function, class, dependency, or configuration file truly necessary?
5. What is the simplest change that makes the tests pass and keeps the design clear?

## Rules

- Do not add abstractions for hypothetical future use.
- Do not add new dependencies unless clearly justified.
- Do not create classes, frameworks, registries, factories, or general engines unless the current task requires them.
- Prefer deleting code over adding code when deletion improves clarity.
- Prefer simple functions over object systems unless object behavior is needed.
- Reuse existing project conventions.
- Keep changes local and narrow.
- Preserve public APIs unless the task explicitly asks to change them.
- If a simple solution and a general solution both work, choose the simple solution.
- If uncertain, explain the tradeoff before implementing.
- Before configuring or updating Git credentials (`user.name` or `user.email`), check the existing commit logs of the repository using `git log -n 5` to ensure the name and email address match the project's historical author credentials. Do not rely solely on `DESCRIPTION` metadata which may contain placeholders or outdated addresses.

## For R code

- Use native pipe `|>`.
- Use explicit namespaces, e.g. `dplyr::mutate()`, `purrr::map()`.
- Follow tidyverse style.
- Use roxygen2 for exported functions.
- Prefer tibbles and validation functions before introducing S3/S4/R6/S7 classes.
- Avoid premature generalization across test formats.
- Implement one concrete workflow first, then generalize only after repeated use.

## Before committing a change

Report briefly:

- What changed.
- Why the change was necessary.
- Why a simpler alternative was not sufficient, if relevant.
- What tests or checks were run.