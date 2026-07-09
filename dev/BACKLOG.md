# birtir backlog

## Release metadata

- Run `R CMD build .` and `R CMD check --no-manual --no-build-vignettes` from a
  clean worktree before release.


## Render context

- [x] Replaced the package-global render state with a robust stack-based Render Context to support nested/batch renders.

## Session side effects

- Decide how much a rendered script is allowed to affect the calling session.
- Current behavior allows `library(...)`, attached packages, options changes,
  random seed changes, and globals created during the analysis script.
- This is convenient for interactive use, but weakens reproducibility and
  isolation.
- Consider whether some cleanup or scoped execution should be added later.

## Helper API

- Keep `md_table()` and `md_plot()` as the shared public script API.
- Consider adding lightweight validation:
  - clearer error for non-ggplot input to `md_plot()`
  - clearer error if helpers are called with a broken render state

## Testing gaps

- Add tests for render-state cleanup after helper failures.
- Add tests for malformed helper inputs.
- Add tests for scripts that intentionally modify session state, so expected
  behavior is explicit.

## Recently handled

- Implemented a stack-based Render Context system (supporting nested and batch renders safely) and added corresponding unit tests.
- Resolved repository hygiene tasks: marked `.githooks/pre-commit` executable, verified `birtir.Rproj` is ignored, and fixed `.gitignore` typo.
- Removed unused Excel files from `data/` and `inst/extdata/`.
- Removed redundant standalone `fmt_num()` and `fmt_p()` tests from
  `test-render-analysis-md.R`; formatter behavior remains covered in
  `test-description-helpers.R`, and `md_text()` still checks render-facing
  formatting behavior.
