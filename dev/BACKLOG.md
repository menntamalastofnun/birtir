# birtir backlog

## Release metadata

- Run `R CMD build .` and `R CMD check --no-manual --no-build-vignettes` from a
  clean worktree before release.


## Render context

- [x] Replaced the package-global render state with a robust stack-based Render Context to support nested/batch renders.


## Recently handled

- Implemented a stack-based Render Context system (supporting nested and batch renders safely) and added corresponding unit tests.
- Resolved repository hygiene tasks: marked `.githooks/pre-commit` executable, verified `birtir.Rproj` is ignored, and fixed `.gitignore` typo.
- Removed unused Excel files from `data/` and `inst/extdata/`.
- Removed redundant standalone `fmt_num()` and `fmt_p()` tests from
  `test-render-analysis-md.R`; formatter behavior remains covered in
  `test-description-helpers.R`, and `md_text()` still checks render-facing
  formatting behavior.
